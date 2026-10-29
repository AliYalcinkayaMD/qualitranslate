# Copilot transport safety, the WebSocket client and the connection watcher
# (port of the browser-free parts of python/tests/test_copilot_browser.py, plus R-specific
# framing tests). No real browser and no user profile are used.

test_that("an expired control deadline cannot be extended by buffered events", {
  s <- qt("devtools_socket")("ws://127.0.0.1:9222/devtools/page/abc", 9222, .connect = FALSE)
  s$buffer <- charToRaw("12")
  expect_error(s$read(2, qt("qt_clock")() - 1), "timed out", class = "qt_browser_error")
})

test_that("non-loopback debugger addresses are rejected before any connection", {
  for (url in c("ws://example.com:9222/devtools/page/abc", "ws://127.0.0.1:9999/devtools/page/abc",
                "wss://127.0.0.1:9222/devtools/page/abc", "ws://127.0.0.1:9222/arbitrary",
                "ws://127.0.0.1:9222/devtools/page/abc\n", "ws://user@evil.example:9222/devtools/page/abc")) {
    expect_error(qt("devtools_socket")(url, 9222, .connect = FALSE), "Unexpected browser control address", class = "qt_browser_error")
  }
})

test_that("only institutional Copilot hosts are eligible in production", {
  browser <- qt("new_copilot_browser")()
  expect_true(browser$allowed("https://copilot.cloud.microsoft/chat"))
  expect_true(browser$allowed("https://m365.cloud.microsoft/chat?x=1"))
  for (url in c("http://copilot.cloud.microsoft/chat", "https://copilot.cloud.microsoft.evil.test/", "https://copilot.microsoft.com/",
                "http://127.0.0.1/", "https://copilot.cloud.microsoft:8443/", "https://copilot.cloud.microsoft:99999/")) {
    expect_false(browser$allowed(url), info = url)
  }
  fixture <- qt("new_copilot_browser")(allow_fixture = TRUE)
  expect_true(fixture$allowed("http://127.0.0.1:5000/"))
})

test_that("large Unicode packets use masked 64-bit WebSocket frames", {
  payload <- charToRaw(enc2utf8(strrep(paste0("Danish ", intToUtf8(c(0xe6, 0xf8, 0xe5)), " \u00b7 Korean ", intToUtf8(c(0xd55c, 0xad6d, 0xc5b4)), " \u00b7 "), 4000)))
  frame <- qt("ws_frame")(payload)
  expect_identical(frame[1], as.raw(0x81)); expect_identical(frame[2], as.raw(0xff))
  expect_equal(qt("ws_header_length")(frame[2], frame[3:10]), length(payload))
  mask <- frame[11:14]
  expect_identical(xor(frame[15:length(frame)], rep_len(mask, length(payload))), payload)
  medium <- qt("ws_frame")(as.raw(rep(65, 300)))
  expect_identical(medium[2], as.raw(0x80 + 126)); expect_equal(qt("ws_header_length")(medium[2], medium[3:4]), 300)
  small <- qt("ws_frame")("hi")
  expect_identical(small[2], as.raw(0x82))
  expect_error(qt("ws_frame")(raw(12000001)), "too large", class = "qt_browser_error")
})

test_that("sign-in, verification and limits pause without any bypass", {
  for (key in c("authentication_required", "challenge", "limit")) {
    info <- list(); info[[key]] <- TRUE
    expect_error(qt("copilot_attention")(info), class = "qt_browser_error")
  }
  expect_silent(qt("copilot_attention")(list(has_editor = TRUE)))
})

test_that("refusals, prose and normalisation follow the Python rules", {
  expect_true(grepl(qt("RX_REFUSAL"), "Sorry, it looks like I can\u2019t respond to this.", perl = TRUE, ignore.case = TRUE))
  expect_true(grepl(qt("RX_REFUSAL"), "Hmm...it looks like I can't chat about this.", perl = TRUE, ignore.case = TRUE))
  expect_false(grepl(qt("RX_REFUSAL"), "Here is the translation: When can one judge", perl = TRUE, ignore.case = TRUE))
  # As in Python: trailing spaces are stripped first, then only the editor's zero-width tail.
  expect_identical(qt("editor_text")("Text  \u200b\u200c"), "Text  ")
  expect_identical(qt("editor_text")("Text\u200b\u200c  "), "Text")
})

# ----- the WebSocket reader over a real loopback socket pair --------------------------------

socket_pair <- function() {
  # Base R's serverSocket()/socketAccept() pair does not connect reliably on Windows runners.
  # The framing logic tested here is platform independent; the WebSocket client itself is
  # exercised on Windows by the handshake test against httpuv's server.
  testthat::skip_on_os("windows")
  listener <- NULL
  for (i in 1:20) {
    port <- httpuv::randomPort()
    listener <- tryCatch(suppressWarnings(serverSocket(port)), error = function(e) NULL)
    if (!is.null(listener)) break
  }
  client <- socketConnection("127.0.0.1", port, blocking = FALSE, open = "r+b", timeout = 5)
  server <- socketAccept(listener, blocking = TRUE, open = "r+b", timeout = 5)
  close(listener)
  s <- qt("devtools_socket")(sprintf("ws://127.0.0.1:%d/devtools/page/test", port), port, .connect = FALSE)
  s$con <- client
  list(s = s, server = server, close = function() { try(close(server), silent = TRUE); s$close() })
}

server_frame <- function(payload, opcode = 1L, fin = TRUE) {
  if (is.character(payload)) payload <- charToRaw(enc2utf8(payload))
  n <- length(payload)
  first <- as.raw((if (fin) 0x80L else 0L) + opcode)
  len <- if (n < 126) as.raw(n) else if (n < 65536) c(as.raw(126), as.raw(c(n %/% 256, n %% 256))) else c(as.raw(127), as.raw((n %/% 256^(7:0)) %% 256))
  c(first, len, payload)
}

test_that("fragmented messages, ping/pong and long frames are read correctly", {
  p <- socket_pair(); on.exit(p$close())
  long <- strrep("x", 70000)
  writeBin(c(server_frame("Hel", 1L, fin = FALSE), server_frame("p1", 9L), server_frame("lo", 0L),
             server_frame(strrep("y", 300)), server_frame(long)), p$server)
  flush(p$server)
  deadline <- qt("qt_clock")() + 5
  expect_identical(p$s$message(deadline), "Hello")
  pong <- readBin(p$server, "raw", 8)
  expect_identical(pong[1], as.raw(0x8a)); expect_identical(pong[2], as.raw(0x82))
  expect_identical(rawToChar(xor(pong[7:8], pong[3:4])), "p1")
  expect_identical(p$s$message(deadline), strrep("y", 300))
  expect_identical(p$s$message(deadline), long)
})

test_that("a close frame or a closed socket is reported as a closed browser window", {
  p <- socket_pair(); on.exit(p$close())
  writeBin(server_frame(raw(0), 8L), p$server); flush(p$server)
  err <- tryCatch(p$s$message(qt("qt_clock")() + 5), error = function(e) e)
  expect_identical(err$code, "browser_closed")
  q <- socket_pair(); on.exit(q$close(), add = TRUE)
  close(q$server)
  err <- tryCatch(q$s$message(qt("qt_clock")() + 5), error = function(e) e)
  expect_identical(err$code, "browser_closed")
  r <- socket_pair(); on.exit(r$close(), add = TRUE)
  writeBin(server_frame(as.raw(1:3), 2L), r$server); flush(r$server)
  expect_error(r$s$message(qt("qt_clock")() + 5), "Unsupported browser control frame", class = "qt_browser_error")
})

test_that("CDP calls skip events and other replies and match their own id", {
  p <- socket_pair(); on.exit(p$close())
  writeBin(c(server_frame('{"method":"Page.loadEventFired","params":{}}'), server_frame('{"id":7,"result":{"wrong":true}}'),
             server_frame('{"id":1,"result":{"value":"\\u00e6 ok"}}'), server_frame('{"id":2,"error":{"message":"x"}}')), p$server)
  flush(p$server)
  expect_identical(p$s$call("Runtime.evaluate", list(expression = "1"))$value, "\u00e6 ok")
  sent <- readBin(p$server, "raw", 2)   # exactly the header bytes that were sent
  expect_identical(bitwAnd(as.integer(sent[2]), 128L), 128L)    # client frames are masked
  err <- tryCatch(p$s$call("Page.navigate", list(url = "x")), error = function(e) e)
  expect_identical(err$code, "page_changed")
})

test_that("the client completes a real WebSocket handshake and receives a multi-megabyte reply", {
  big <- strrep("\u00e6\u00f8\u00e5 translation ", 200000)
  app <- list(
    call = function(req) list(status = 404L, headers = list(), body = ""),
    onWSOpen = function(ws) {
      ws$onMessage(function(binary, message) {
        request <- jsonlite::parse_json(message)
        ws$send('{"method":"Runtime.consoleAPICalled","params":{}}')
        ws$send(qt("json_str")(list(id = request$id, result = list(result = list(value = big)))))
      })
    })
  port <- httpuv::randomPort()
  server <- httpuv::startServer("127.0.0.1", port, app); on.exit(httpuv::stopServer(server))
  s <- qt("devtools_socket")(sprintf("ws://127.0.0.1:%d/devtools/page/ABC-123", port), port)
  on.exit(s$close(), add = TRUE)
  value <- s$evaluate("document.title")
  expect_identical(nchar(value), nchar(big))
  expect_identical(value, big)
})

test_that("a cancelled job stops the transport before anything is sent", {
  cancel <- qt("qt_event")(); cancel$set()
  s <- qt("devtools_socket")("ws://127.0.0.1:9222/devtools/page/abc", 9222, cancel = cancel, .connect = FALSE)
  err <- tryCatch(s$call("Runtime.evaluate"), error = function(e) e)
  expect_identical(err$code, "cancelled")
})

# ----- connection watcher -----------------------------------------------------------------------

fake_browser <- function() {
  self <- new.env(parent = emptyenv())
  self$requests <- list(); self$ready <- FALSE; self$opened <- FALSE; self$checks <- 0L
  self$summary <- function() list(open = self$opened, ready = self$ready, experimental = TRUE,
                                  message = if (self$ready) "Fixture controls ready" else "Sign in and check connection.")
  self$open <- function() { self$opened <- TRUE; self$summary() }
  self$check <- function() { self$checks <- self$checks + 1L; self$ready <- self$opened; self$summary() }
  self$close <- function(forget = FALSE) { self$ready <- FALSE; self$opened <- FALSE }
  self$complete <- function(box, cancel = NULL) stop("not expected")
  self
}

test_that("sign-in is rechecked without another button and without a prompt", {
  browser <- fake_browser()
  browser$check <- function() { browser$checks <- browser$checks + 1L; browser$ready <- browser$checks >= 3; browser$summary() }
  connection <- qt("new_copilot_connection")(browser, timeout = 1, interval = 0.01)
  expect_true(connection$start()$connecting)
  connection$run()
  expect_true(connection$summary()$ready)
  expect_identical(browser$checks, 3L)
  expect_length(browser$requests, 0)
})

test_that("a timeout shows the last blocker without sending or reopening", {
  browser <- fake_browser(); opens <- 0L
  browser$open <- function() { opens <<- opens + 1L; browser$opened <- TRUE; browser$summary() }
  browser$check <- function() browser$summary()
  connection <- qt("new_copilot_connection")(browser, timeout = 0.05, interval = 0.01)
  connection$start(); connection$run()
  expect_identical(connection$summary()$connection_phase, "timeout")
  expect_match(connection$summary()$message, "Sign in", fixed = TRUE)
  expect_false(connection$summary()$connecting)
  expect_identical(opens, 1L)
})

test_that("human verification stops the watcher instead of retrying", {
  browser <- fake_browser()
  browser$check <- function() {
    browser$checks <- browser$checks + 1L
    c(browser$summary()[c("open", "ready", "experimental")], list(error_code = "verification", message = "Human verification required"))
  }
  connection <- qt("new_copilot_connection")(browser, timeout = 1, interval = 0.01)
  connection$start(); connection$run()
  expect_identical(connection$summary()$connection_phase, "error")
  expect_identical(browser$checks, 1L)
})

test_that("an open() failure is reported and nothing is sent", {
  browser <- fake_browser()
  browser$open <- function() qt("browser_error")("Google Chrome or Microsoft Edge is needed for automatic Copilot mode.", "no_browser")
  connection <- qt("new_copilot_connection")(browser, timeout = 1, interval = 0.01)
  connection$start(); connection$run()
  expect_identical(connection$summary()$connection_phase, "error")
  expect_match(connection$summary()$message, "Connection stopped. Nothing was sent. Google Chrome", fixed = TRUE)
})

test_that("cancel prevents late readiness while state and export stay responsive", {
  browser <- fake_browser()
  ws <- qt("new_workspace")(fake_client(), browser)
  qt("ws_load_text")(ws, "P: Fictional source", "Cancellation test")
  srv <- start_test_server(ws, "test-token"); on.exit(httpuv::stopServer(srv$server))
  seen <- list()
  # The browser window takes a long time to open; meanwhile the interface keeps polling.
  browser$open <- function() {
    browser$opened <- TRUE
    seen$state <<- api(srv, "state", loop = FALSE)$json
    seen$export <<- api(srv, "export", list(kind = "json"), loop = FALSE)$json
    seen$busy <<- api(srv, "copilot/open", qt("empty_obj")(), loop = FALSE)$json
    seen$cancel <<- api(srv, "copilot/cancel", qt("empty_obj")(), loop = FALSE)$json
    seen$again <<- api(srv, "copilot/open", qt("empty_obj")(), loop = FALSE)
    browser$summary()
  }
  opened <- api(srv, "copilot/open", qt("empty_obj")(), loop = FALSE)$json
  expect_true(opened$connecting)
  qt("qt_loop_once")(ws)       # the main loop runs the watcher step, which opens the window
  expect_true(seen$state$copilot$connecting)
  expect_identical(seen$export$title, "Cancellation test")
  expect_true(seen$busy$connecting)
  expect_identical(seen$cancel$connection_phase, "cancelled")
  expect_identical(seen$again$status, 400L)
  expect_match(seen$again$json$error, "previous connection check is stopping", fixed = TRUE)
  expect_false(isTRUE(api(srv, "state")$json$copilot$ready))
  expect_false(ws$copilot_connection$active)
  expect_identical(browser$checks, 0L)
})

test_that("close and forget run outside the request handler and reply afterwards", {
  browser <- fake_browser(); closed <- list()
  browser$close <- function(forget = FALSE) { closed[[length(closed) + 1L]] <<- forget; browser$opened <- FALSE }
  ws <- qt("new_workspace")(fake_client(), browser)
  srv <- start_test_server(ws); on.exit(httpuv::stopServer(srv$server))
  res <- api(srv, "copilot/forget", qt("empty_obj")())
  expect_identical(res$status, 200L)
  expect_identical(res$json$connection_phase, "idle")
  expect_identical(closed, list(TRUE))
  qt("ws_load_text")(ws, qt("DEMO_SOURCE"), "demo", demo = TRUE)
  qt("ws_start_job")(ws, "all", demo = TRUE)
  busy <- api(srv, "copilot/close", qt("empty_obj")(), loop = FALSE)
  expect_identical(busy$status, 400L)
  expect_match(busy$json$error, "A translation job is running", fixed = TRUE)
  qt("ws_run_job")(ws)
})

# ----- the dedicated profile ----------------------------------------------------------------

test_that("the default profile is in the R user data folder and is not created before Connect", {
  old <- Sys.getenv("R_USER_DATA_DIR", NA)
  base <- tempfile("qt-user-data-")
  Sys.setenv(R_USER_DATA_DIR = base)
  on.exit({ if (is.na(old)) Sys.unsetenv("R_USER_DATA_DIR") else Sys.setenv(R_USER_DATA_DIR = old); unlink(base, recursive = TRUE) })
  expect_identical(qt("default_profile_dir")(), file.path(tools::R_user_dir("qualitranslate", "data"), "copilot-browser-profile"))
  expect_true(startsWith(qt("default_profile_dir")(), base))
  browser <- qt("new_copilot_browser")()
  browser$close()
  browser$close(forget = TRUE)
  expect_false(dir.exists(base))
})

test_that("one profile cannot be driven twice in this session", {
  dir <- tempfile("qt-lock-"); dir.create(dir); on.exit(unlink(dir, recursive = TRUE))
  first <- qt("new_copilot_browser")(profile_dir = dir); second <- qt("new_copilot_browser")(profile_dir = dir)
  first$acquire(dir)
  err <- tryCatch(second$acquire(dir), error = function(e) e)
  expect_identical(err$code, "in_use")
  first$release()
  second$acquire(dir); second$release()
})

test_that("another process holding the profile lock keeps this session out", {
  skip_on_cran()
  dir <- tempfile("qt-lock-"); dir.create(dir); on.exit(unlink(dir, recursive = TRUE))
  lockfile <- file.path(dir, "QualiTranslate-in-use.lock")
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  # deparse() quotes the path safely (Windows paths contain backslashes).
  holder <- processx::process$new(rscript, c("-e", sprintf("l <- filelock::lock(%s); cat('locked\\n'); Sys.sleep(30)",
                                                         deparse(normalizePath(lockfile, winslash = "/", mustWork = FALSE)))),
                                  stdout = "|", stderr = "|")
  on.exit(holder$kill(), add = TRUE)
  expect_true(holder$poll_io(15000)[["output"]] == "ready")
  expect_identical(trimws(holder$read_output_lines()), "locked")
  browser <- qt("new_copilot_browser")(profile_dir = dir)
  err <- tryCatch(browser$acquire(dir), error = function(e) e)
  expect_identical(err$code, "in_use")
  holder$kill(); holder$wait(2000)
  browser$acquire(dir); browser$release()
  # The Python version takes the same POSIX lock (fcntl.lockf), so a Python copy also keeps R out.
  python <- Sys.which("python3")
  skip_if(!nzchar(python) || .Platform$OS.type == "windows", "python3 is not available")
  py <- processx::process$new(python, c("-c", sprintf("import fcntl,sys,time;h=open(%s,'a+');fcntl.lockf(h.fileno(),fcntl.LOCK_EX|fcntl.LOCK_NB);print('locked',flush=True);time.sleep(30)",
                                                      deparse(lockfile))), stdout = "|", stderr = "|")
  on.exit(py$kill(), add = TRUE)
  expect_true(py$poll_io(15000)[["output"]] == "ready")
  expect_identical(trimws(py$read_output_lines()), "locked")
  err <- tryCatch(qt("new_copilot_browser")(profile_dir = dir)$acquire(dir), error = function(e) e)
  expect_identical(err$code, "in_use")
})

test_that("the consumer Copilot for personal accounts gets a clear message", {
  browser <- qt("new_copilot_browser")(profile_dir = tempfile("qt-personal-"))
  browser$pages <- function() list(list(type = "page", url = "https://copilot.microsoft.com/", id = "x"))
  err <- tryCatch(browser$page(), error = function(e) e)
  expect_match(conditionMessage(err), "personal Microsoft accounts")
  expect_identical(err$code, "other_page")
})
