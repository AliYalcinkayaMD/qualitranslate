# Minimal WebSocket client for the browser's loopback DevTools (CDP) endpoint.
#
# Built on a base R socket connection. Reads are non-blocking: before every read the
# connection is polled with socketSelect() in short slices, and between slices the app's
# event loop is served (qt_wait), so the browser interface stays responsive while a reply
# of several megabytes is awaited, and Stop and every deadline are honoured even if the
# browser stalls in the middle of a frame. (R's blocking socket reads wait for the full
# requested size and, once httpuv/later are loaded, do not honour the socket timeout, so a
# stalled frame could otherwise freeze the app.) Bytes read beyond the current frame are
# kept in a buffer. Frames we send are masked (RFC 6455 client rule); 16- and 64-bit payload
# lengths, fragmented messages, ping/pong and close are handled, and CDP replies are matched
# by message id while events are skipped.

WS_GUID <- "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
MAX_FRAME <- 12000000

cancelled_check <- function(cancel) {
  if (!is.null(cancel) && cancel$is_set()) {
    browser_error("Stopped. No further Copilot messages will be sent. A reply already being written may finish in the Copilot window.", "cancelled")
  }
}

# Encode one client frame (FIN set, masked).
ws_frame <- function(payload, opcode = 1L, mask = openssl::rand_bytes(4)) {
  if (is.character(payload)) payload <- charToRaw(enc2utf8(payload))
  n <- length(payload)
  if (n > MAX_FRAME) browser_error("Browser message is too large. Reduce the batch size.")
  head <- as.raw(bitwOr(0x80L, opcode))
  if (n < 126) {
    head <- c(head, as.raw(bitwOr(0x80L, n)))
  } else if (n < 65536) {
    head <- c(head, as.raw(0x80L + 126L), as.raw(c(n %/% 256L, n %% 256L)))
  } else {
    head <- c(head, as.raw(0x80L + 127L), as.raw((n %/% 256^(7:0)) %% 256))
  }
  masked <- if (n) xor(payload, rep_len(mask, n)) else raw(0)
  c(head, mask, masked)
}

# Decode the length fields of a frame header; used by the reader and by the tests.
ws_header_length <- function(b2, extended) {
  n <- bitwAnd(as.integer(b2), 127L)
  if (n == 126L) return(sum(as.integer(extended[1:2]) * c(256, 1)))
  if (n == 127L) return(sum(as.integer(extended[1:8]) * 256^(7:0)))
  n
}

# `.connect = FALSE` (tests only) checks the address and builds the object without connecting.
devtools_socket <- function(url, expected_port, cancel = NULL, deadline = NULL, .connect = TRUE) {
  u <- url_split(url)
  if (!identical(u$scheme, "ws") || !isTRUE(u$hostname %in% c("127.0.0.1", "localhost")) || !u$port_ok ||
      !identical(u$port, as.integer(expected_port)) ||
      !grepl("^/devtools/(?:page|browser)/[A-Za-z0-9-]+\\z", u$path, perl = TRUE)) {
    browser_error("Unexpected browser control address. Open a new Copilot window from this app.")
  }
  self <- new.env(parent = emptyenv())
  self$cancel <- cancel; self$deadline <- deadline; self$next_id <- 0L
  self$con <- NULL; self$buffer <- raw(0)

  self$close <- function() {
    if (!is.null(self$con)) {
      try(close(self$con), silent = TRUE)
      self$con <- NULL
    }
    invisible()
  }

  # Wait until the socket has data (or EOF), serving the app between short polls, and
  # return the bytes that are available now (never blocks for more).
  self$fill <- function(deadline) {
    repeat {
      cancelled_check(self$cancel)
      if (qt_clock() > deadline) browser_error("Browser control timed out. Check the Copilot window before resuming.")
      if (isTRUE(socketSelect(list(self$con), FALSE, 0.01))) {
        part <- readBin(self$con, "raw", 1048576L)
        if (!length(part)) browser_error("The Copilot browser window was closed.", "browser_closed")
        return(part)
      }
      qt_wait(0)
    }
  }

  # Read exactly n bytes.
  self$read <- function(n, deadline) {
    cancelled_check(self$cancel)
    if (qt_clock() > deadline) browser_error("Browser control timed out. Check the Copilot window before resuming.")
    if (n == 0) return(raw(0))
    chunks <- list(self$buffer); have <- length(self$buffer)
    while (have < n) {
      part <- self$fill(deadline)
      chunks[[length(chunks) + 1L]] <- part
      have <- have + length(part)
    }
    data <- if (length(chunks) == 1L) chunks[[1]] else do.call(c, chunks)
    self$buffer <- if (length(data) > n) data[(n + 1):length(data)] else raw(0)
    if (length(data) > n) data[seq_len(n)] else data
  }

  self$send <- function(payload, opcode = 1L) {
    writeBin(ws_frame(payload, opcode), self$con)
    flush(self$con)
    invisible()
  }

  self$message <- function(deadline) {
    collected <- list(); total <- 0
    repeat {
      h <- self$read(2, deadline)
      a <- as.integer(h[1]); b <- as.integer(h[2])
      opcode <- bitwAnd(a, 15L)
      n <- bitwAnd(b, 127L)
      if (n == 126L) n <- ws_header_length(h[2], self$read(2, deadline))
      else if (n == 127L) n <- ws_header_length(h[2], self$read(8, deadline))
      if (n > MAX_FRAME || total + n > MAX_FRAME) browser_error("Browser response is too large.")
      mask <- if (bitwAnd(b, 128L)) self$read(4, deadline) else NULL
      body <- self$read(n, deadline)
      if (!is.null(mask) && length(body)) body <- xor(body, rep_len(mask, length(body)))
      if (opcode == 8L) browser_error("The Copilot browser connection closed.", "browser_closed")
      if (opcode == 9L) {
        self$send(body, 10L)
        next
      }
      if (opcode == 10L) next
      if (!(opcode %in% c(0L, 1L))) browser_error("Unsupported browser control frame.")
      collected[[length(collected) + 1L]] <- body
      total <- total + length(body)
      if (bitwAnd(a, 128L)) {
        bytes <- if (length(collected) == 1L) collected[[1]] else do.call(c, collected)
        text <- raw_to_utf8(bytes)
        if (!validUTF8(text)) browser_error("Unsupported browser control frame.")
        return(text)
      }
    }
  }

  self$call <- function(method, params = NULL, timeout = 12) {
    cancelled_check(self$cancel)
    self$next_id <- self$next_id + 1L
    request_id <- self$next_id
    self$send(json_str(list(id = request_id, method = method, params = params %||% empty_obj())))
    deadline <- min(qt_clock() + timeout, self$deadline %||% Inf)
    repeat {
      message <- json_parse(self$message(deadline))
      mid <- if (is_obj(message)) message[["id"]] else NULL
      if (is.null(mid) || !is_num(mid) || mid != request_id) next
      if ("error" %in% names(message)) {
        browser_error("Copilot page changed or is not ready. Check its window and reconnect.", "page_changed")
      }
      return(message[["result"]] %||% empty_obj())
    }
  }

  self$evaluate <- function(expression, context_id = NULL) {
    params <- list(expression = expression, returnByValue = TRUE, awaitPromise = TRUE)
    if (!is.null(context_id)) params$contextId <- context_id
    result <- self$call("Runtime.evaluate", params)
    if (!is.null(result[["exceptionDetails"]])) {
      browser_error("The Copilot page changed. No reply was imported; check the browser and reconnect.", "page_changed")
    }
    oget(result[["result"]], "value")
  }

  if (!.connect) return(self)
  # Connect and perform the HTTP upgrade handshake.
  con <- suppressWarnings(socketConnection("127.0.0.1", as.integer(expected_port), blocking = FALSE,
                                           open = "r+b", timeout = 3))
  self$con <- con
  ok <- FALSE
  on.exit(if (!ok) self$close(), add = TRUE)
  key <- openssl::base64_encode(openssl::rand_bytes(16))
  request <- paste0("GET ", u$path, " HTTP/1.1\r\nHost: 127.0.0.1:", expected_port,
                    "\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: ", key,
                    "\r\nSec-WebSocket-Version: 13\r\n\r\n")
  writeBin(charToRaw(request), con)
  flush(con)
  hs_deadline <- min(qt_clock() + 5, deadline %||% Inf)
  data <- raw(0)
  terminator <- charToRaw("\r\n\r\n")
  repeat {
    end <- grepRaw(terminator, data, fixed = TRUE)
    if (length(end)) break
    if (length(data) > 65536) browser_error("Browser control connection closed.")
    part <- tryCatch(self$fill(hs_deadline), qt_browser_error = function(e) {
      if (identical(e$code, "cancelled")) stop(e)
      if (identical(e$code, "browser_closed")) browser_error("Browser control connection closed.")
      browser_error("Browser control handshake timed out.")
    })
    data <- c(data, part)
  }
  header <- data[seq_len(end + 3L)]
  # Anything after the headers already belongs to the first frame.
  self$buffer <- if (length(data) > end + 3L) data[(end + 4L):length(data)] else raw(0)
  text <- rawToChar(header)
  accept <- openssl::base64_encode(openssl::sha1(charToRaw(paste0(key, WS_GUID))))
  first_line <- strsplit(text, "\r\n", fixed = TRUE)[[1]][1]
  if (!startsWith(first_line, "HTTP/1.1 101") || !grepl(tolower(accept), tolower(text), fixed = TRUE)) {
    browser_error("Browser control handshake was rejected.")
  }
  ok <- TRUE
  self
}
