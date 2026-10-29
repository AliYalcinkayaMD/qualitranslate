# Visible-browser transport for institutional Microsoft 365 Copilot Chat.
#
# QualiTranslate opens Chrome or Edge with its own dedicated browser profile and a
# loopback-only DevTools port, then works the visible chat page the way a person
# would: open a new (temporary) chat, put the prepared text in the message box,
# press Send, and read the reply shown on screen.
#
# It never reads the user's ordinary browser profile, cookies, tokens, storage or
# network traffic, and it never calls a Microsoft model endpoint directly.
# The user signs in themselves in the dedicated window; sign-in pages, human-verification
# challenges and usage limits are never bypassed. The sign-in is kept in the dedicated
# profile so it is not needed every session; "Forget sign-in" removes the profile folder.
#
# Port of copilot_browser.py from the Python version; behaviour, waits, error codes and
# messages are the same.

COPILOT_URL <- "https://copilot.cloud.microsoft/chat"
ALLOWED_HOSTS <- c("copilot.cloud.microsoft", "m365.cloud.microsoft")
PROFILE_NAME <- "copilot-browser-profile"
LOCK_NAME <- "QualiTranslate-in-use.lock"

# Copilot's editor (Lexical) appends U+200B U+200C after typed text. Only true invisible
# formatting (ZWSP, word joiner, BOM) and that trailing artefact are ignored; ZWNJ/ZWJ and
# direction marks inside the text are meaningful and must match exactly.
RX_INVISIBLE <- "[\u200b\u2060\ufeff]"
RX_EDITOR_TAIL <- "[\u200b\u200c]+$"

# Short service refusals observed live: "Sorry, it looks like I can't respond to this. Let's try a
# different topic." / "Hmm...it looks like I can't chat about this."
RX_REFUSAL <- paste0("can[\u2019']?t (?:respond|chat|help|answer|assist)|cannot (?:respond|help|answer|assist)|unable to (?:respond|help|answer)|",
                     "not able to (?:respond|help)|different topic|kan ikke (?:svare|hj\u00e6lpe)|yard\u0131mc\u0131 olamam|yan\u0131t veremem")

# The composer text without the editor's trailing zero-width artefact.
editor_text <- function(text) {
  text <- if (is_str(text)) text else ""
  sub(RX_EDITOR_TAIL, "", py_rstrip(text), perl = TRUE)
}

# Compare composer text the way a reader would: ignore spacing and invisible formatting.
normalise <- function(text) {
  py_strip(gsub(paste0(WS, "+"), " ", gsub(RX_INVISIBLE, "", editor_text(text), perl = TRUE), perl = TRUE))
}

# End (byte index) of the JSON value that starts with the "{" at byte `start`, using the
# string/bracket tokens of the text; NA when the brackets do not balance.
json_object_end <- function(bytes, start) {
  tail_bytes <- bytes[start:length(bytes)]
  m <- gregexpr('"(?:[^"\\\\]|\\\\.)*+"|[][{}]', rawToChar(tail_bytes), perl = TRUE, useBytes = TRUE)[[1]]
  if (m[1] == -1L) return(NA_integer_)
  pos <- as.integer(m); len <- attr(m, "match.length")
  # Byte positions throughout: a one-byte match that is not a quote is a bracket.
  pos <- pos[len == 1L & tail_bytes[pos] != charToRaw('"')]
  if (!length(pos)) return(NA_integer_)
  opening <- tail_bytes[pos] == charToRaw("{") | tail_bytes[pos] == charToRaw("[")
  depth <- cumsum(ifelse(opening, 1L, -1L))
  close <- which(depth == 0L)
  if (!length(close)) return(NA_integer_)
  start + pos[close[1]] - 1L
}

# Only accept an object carrying this run's unpredictable receipt; never an input echo.
extract_receipt_json <- function(text, receipt) {
  if (!is_str(text) || !nzchar(text) || !grepl(receipt, text, fixed = TRUE)) return(NULL)
  bytes <- charToRaw(enc2utf8(text))
  opens <- which(bytes == charToRaw("{"))
  last_receipt <- max(gregexpr(receipt, text, fixed = TRUE, useBytes = TRUE)[[1]])
  for (start in opens[opens < last_receipt]) {
    end <- json_object_end(bytes, start)
    if (is.na(end)) next
    candidate <- raw_to_utf8(bytes[start:end])
    value <- tryCatch(json_parse(candidate), error = function(e) NULL)
    if (is_obj(value) && identical(value[["qt_receipt"]], receipt) && is_arr(value[["segments"]])) return(candidate)
  }
  NULL
}

# The dedicated browser profile lives in the R user data folder (CRAN policy). It is created
# only when the user clicks Connect in Copilot mode.
default_profile_dir <- function() {
  file.path(tools::R_user_dir("qualitranslate", "data"), PROFILE_NAME)
}

find_browser <- function() {
  paths <- character()
  sysname <- Sys.info()[["sysname"]]
  if (identical(sysname, "Darwin")) {
    for (base in c("/Applications", file.path(Sys.getenv("HOME"), "Applications"))) {
      paths <- c(paths, file.path(base, "Google Chrome.app/Contents/MacOS/Google Chrome"),
                 file.path(base, "Microsoft Edge.app/Contents/MacOS/Microsoft Edge"))
    }
  } else if (.Platform$OS.type == "windows") {
    for (env in c("PROGRAMFILES", "PROGRAMFILES(X86)", "LOCALAPPDATA")) {
      base <- Sys.getenv(env)
      if (nzchar(base)) {
        paths <- c(paths, file.path(base, "Google/Chrome/Application/chrome.exe"),
                   file.path(base, "Microsoft/Edge/Application/msedge.exe"))
      }
    }
  } else {
    found <- Sys.which(c("google-chrome", "chromium", "chromium-browser", "microsoft-edge"))
    paths <- unname(found[nzchar(found)])
  }
  for (path in paths) if (file.exists(path) && !dir.exists(path)) return(path)
  browser_error("Google Chrome or Microsoft Edge is needed for automatic Copilot mode. Install one from its official website, then reopen this app.", "no_browser")
}

# Test facilities (executable, start_url, allow_fixture, extra_args, environment, profile_dir,
# timeouts) are R-only constructor arguments, never HTTP or UI parameters.
new_copilot_browser <- function(executable = NULL, start_url = COPILOT_URL, allow_fixture = FALSE, extra_args = character(),
                                environment = NULL, timeout = 300, profile_dir = NULL, temporary_chat = TRUE,
                                new_chat_timeout = 45, settle_seconds = 6, exclusive = TRUE) {
  self <- new.env(parent = emptyenv())
  self$executable <- executable; self$start_url <- start_url; self$allow_fixture <- allow_fixture
  self$extra_args <- extra_args; self$environment <- environment; self$timeout <- timeout
  self$new_chat_timeout <- new_chat_timeout
  # Copilot keeps initialising for a few seconds after the page appears; a message sent
  # too early can be silently cleared (observed live, 27 Oct 2026).
  self$settle_ms <- settle_seconds * 1000
  self$decline_quiet_seconds <- 12   # a finished non-JSON reply this long is treated as a refusal
  self$exclusive <- exclusive        # only one QualiTranslate copy may drive the window
  self$lock_handle <- NULL; self$lock_key <- ""
  self$temporary_chat <- temporary_chat
  self$profile_dir <- profile_dir    # NULL = default persistent dedicated profile
  self$temp_profile <- NULL
  self$port <- NULL; self$browser_ws <- NULL; self$process <- NULL; self$target_id <- NULL
  self$browser_version <- ""; self$last_receipt <- ""
  self$cached <- list(open = FALSE, ready = FALSE, experimental = TRUE, error_code = "",
                      message = "Not connected yet. Click Connect & translate with Copilot.",
                      step = "idle", step_detail = "", page_host = "", temporary_chat = temporary_chat)
  self$step_since <- NULL

  # ----- status ---------------------------------------------------------------------------
  self$summary <- function() {
    result <- self$cached
    result$step_elapsed <- if (!is.null(self$step_since)) as.integer(qt_clock() - self$step_since) else 0L
    result
  }
  self$update <- function(...) {
    values <- list(...)
    for (nm in names(values)) self$cached[nm] <- list(values[[nm]])
  }
  self$set_step <- function(step, detail = "") {
    self$update(step = step, step_detail = detail)
    self$step_since <- qt_clock()
  }

  # ----- launching / attaching --------------------------------------------------------------
  self$profile <- function(create = TRUE) {
    if (is.null(self$profile_dir)) self$profile_dir <- default_profile_dir()
    if (identical(self$profile_dir, "temporary")) {
      if (is.null(self$temp_profile)) {
        self$temp_profile <- tempfile("QualiTranslate-Copilot-")
        dir.create(self$temp_profile, recursive = TRUE)
      }
      return(self$temp_profile)
    }
    if (create) {
      dir.create(self$profile_dir, recursive = TRUE, showWarnings = FALSE)
      try(Sys.chmod(self$profile_dir, "0700", use_umask = FALSE), silent = TRUE)
    }
    self$profile_dir
  }

  self$http <- function(path, port = NULL) {
    port <- port %||% self$port
    if (is.null(port)) browser_error("Open the Copilot window first.")
    closed <- "The Copilot window is not connected. Click Connect & translate with Copilot (or Resume with Copilot) to open it again."
    h <- curl::new_handle()
    curl::handle_setopt(h, timeout = 3L, connecttimeout = 3L, noproxy = "*", followlocation = 0L)
    outcome <- qt_fetch_wait(paste0("http://127.0.0.1:", port, path), h)
    if (!outcome$ok || outcome$res$status_code != 200L) browser_error(closed, "browser_closed")
    if (length(outcome$res$content) > 2000000) browser_error("Unexpected browser control response.")
    value <- tryCatch(json_parse(raw_to_utf8(outcome$res$content)), error = function(e) NULL)
    if (is.null(value)) browser_error(closed, "browser_closed")
    value
  }

  self$allowed <- function(url) {
    u <- url_split(url)
    if (!u$port_ok) return(FALSE)
    (identical(u$scheme, "https") && isTRUE(u$hostname %in% ALLOWED_HOSTS) && (is.na(u$port) || u$port == 443L)) ||
      (isTRUE(self$allow_fixture) && identical(u$scheme, "http") && identical(u$hostname, "127.0.0.1"))
  }

  self$read_portfile <- function(portfile) {
    lines <- readLines(portfile, warn = FALSE)
    port <- suppressWarnings(as.integer(lines[1])); ws_path <- lines[2]
    if (is.na(port) || port < 1L || port > 65535L || is.na(ws_path) ||
        !grepl("^/devtools/browser/[A-Za-z0-9-]+\\z", ws_path, perl = TRUE)) stop("bad port file")
    list(port = port, ws = paste0("ws://127.0.0.1:", port, ws_path))
  }

  self$attach <- function(port, browser_ws) {
    info <- self$http("/json/version", port)
    self$port <- port; self$browser_ws <- browser_ws
    self$browser_version <- substr(as.character(oget(info, "Browser", ""))[1], 1, 60)
  }

  # Hold an OS lock in the profile folder while this app copy uses the window.
  self$acquire <- function(profile) {
    if (!isTRUE(self$exclusive) || !is.null(self$lock_handle)) return(invisible())
    busy <- function() browser_error(paste0(
      "Another QualiTranslate copy is already using the Copilot window. Save progress in that copy and close its Terminal window, ",
      "then try again here. Two copies must not type into the same Copilot window."), "in_use")
    key <- normalizePath(profile, mustWork = FALSE)
    # POSIX record locks belong to the process, so a second user of the same profile inside
    # this R session is refused here, before the lock file is opened again.
    if (key %in% .qt$held_profiles) busy()
    # filelock takes the same kind of lock as Python's fcntl.lockf(), so a Python and an R
    # session cannot drive the same Copilot window at once.
    handle <- tryCatch(filelock::lock(file.path(profile, LOCK_NAME), exclusive = TRUE, timeout = 0),
                       error = function(e) NULL)
    if (is.null(handle)) busy()
    .qt$held_profiles <- c(.qt$held_profiles, key)
    self$lock_key <- key
    self$lock_handle <- handle
    invisible()
  }

  self$release <- function() {
    if (!is.null(self$lock_handle)) {
      try(filelock::unlock(self$lock_handle), silent = TRUE)
      .qt$held_profiles <- setdiff(.qt$held_profiles, self$lock_key)
      self$lock_handle <- NULL
    }
    invisible()
  }

  self$process_exited_with_error <- function() {
    p <- self$process
    !is.null(p) && !p$is_alive() && !identical(p$get_exit_status(), 0L)
  }

  self$open <- function() {
    if (!is.null(self$port)) {
      reused <- tryCatch({
        self$http("/json/version"); self$ensure_tab(); TRUE
      }, qt_browser_error = function(e) FALSE)
      if (reused) return(self$summary())
      self$forget_connection()
    }
    self$set_step("browser_starting", "Opening the dedicated Copilot browser window")
    profile <- self$profile(); portfile <- file.path(profile, "DevToolsActivePort")
    self$acquire(profile)
    # Re-use the dedicated window if it is still open from an earlier session.
    if (file.exists(portfile)) {
      reattached <- tryCatch({
        pf <- self$read_portfile(portfile); self$attach(pf$port, pf$ws)
        self$ensure_tab()
        TRUE
      }, error = function(e) {
        if (inherits(e, "qt_browser_error") && identical(e$code, "cancelled")) stop(e)
        FALSE
      })
      if (reattached) {
        self$update(open = TRUE, ready = FALSE, error_code = "", message = "Reconnected to the open Copilot window.")
        return(self$summary())
      }
      self$forget_connection()
      unlink(portfile, force = TRUE)
    }
    executable <- self$executable %||% find_browser()
    args <- c("--remote-debugging-address=127.0.0.1", "--remote-debugging-port=0", paste0("--user-data-dir=", profile),
              "--no-first-run", "--no-default-browser-check", "--new-window",
              # Keep the chat page responsive while the user works in other windows.
              "--disable-background-timer-throttling", "--disable-renderer-backgrounding", "--disable-backgrounding-occluded-windows",
              self$extra_args, self$start_url)
    started <- tryCatch({
      # cleanup = FALSE: the window stays open when R exits, so the next session re-attaches.
      self$process <- processx::process$new(executable, args, stdout = NULL, stderr = NULL, cleanup = FALSE,
                                            env = self$environment %||% "current")
      TRUE
    }, error = function(e) FALSE)
    if (!started) {
      self$process <- NULL
      browser_error("The Copilot browser could not start. Check that Chrome or Edge is installed.", "no_browser")
    }
    deadline <- qt_clock() + 25
    while (qt_clock() < deadline) {
      if (file.exists(portfile)) {
        attached <- tryCatch({
          pf <- self$read_portfile(portfile); self$attach(pf$port, pf$ws); TRUE
        }, error = function(e) FALSE)
        if (attached) {
          self$update(open = TRUE, ready = FALSE, error_code = "", message = "Copilot window opened. Sign in there if Microsoft asks you to.")
          return(self$summary())
        }
      }
      if (self$process_exited_with_error()) break
      qt_wait(0.15)
    }
    self$forget_connection()
    browser_error(paste0("The browser opened without its local control connection. Close every window titled with the QualiTranslate Copilot profile and try again. ",
                         "If this repeats, your organisation may block browser automation."), "no_control")
  }

  self$forget_connection <- function() {
    self$port <- NULL; self$browser_ws <- NULL; self$process <- NULL; self$target_id <- NULL
    self$update(open = FALSE, ready = FALSE)
  }

  self$browser_call <- function(method, params = NULL, timeout = 8) {
    cdp <- devtools_socket(self$browser_ws, self$port)
    on.exit(cdp$close(), add = TRUE)
    cdp$call(method, params %||% empty_obj(), timeout = timeout)
  }

  self$pages <- function() {
    Filter(function(p) identical(oget(p, "type"), "page"), self$http("/json/list"))
  }

  self$ensure_tab <- function() {
    if (!length(self$pages())) {
      result <- self$browser_call("Target.createTarget", list(url = self$start_url))
      self$target_id <- oget(result, "targetId")
    }
  }

  # ----- page and frame inspection ------------------------------------------------------------
  self$page <- function() {
    pages <- self$pages()
    url_of <- function(p) { u <- oget(p, "url", ""); if (is_str(u)) u else "" }
    eligible <- Filter(function(p) self$allowed(url_of(p)), pages)
    chosen <- NULL
    for (p in eligible) if (identical(oget(p, "id"), self$target_id)) { chosen <- p; break }
    if (is.null(chosen) && length(eligible)) {
      chat <- Filter(function(p) startsWith(url_split(url_of(p))$path, "/chat"), eligible)
      chosen <- if (length(chat)) chat[[1]] else eligible[[1]]
    }
    if (!is.null(chosen)) {
      self$target_id <- oget(chosen, "id")
      host <- url_split(url_of(chosen))$hostname
      self$update(page_host = if (is.na(host)) "" else host)
      return(chosen)
    }
    if (!length(pages)) {
      tryCatch(self$ensure_tab(), qt_browser_error = function(e) NULL)
      browser_error("Opening Copilot in its window\u2026", "loading")
    }
    hosts <- unique(vapply(pages, function(p) { h <- url_split(url_of(p))$hostname; if (is.na(h)) "" else h }, character(1)))
    hosts <- sort(setdiff(hosts, ""), method = "radix")
    joined <- paste(hosts, collapse = ", ")
    self$update(page_host = substr(joined, 1, 120))
    if (any(grepl("login|microsoftonline|sts\\.|adfs|wayf", hosts, perl = TRUE))) {
      browser_error(paste0("Waiting for you to finish signing in to Microsoft in the Copilot window (", substr(joined, 1, 80), ")."), "sign_in")
    }
    browser_error(paste0("The Copilot window is showing another page (", substr(joined, 1, 80),
                         "). Finish sign-in there; the app will open Copilot Chat when it can."), "other_page")
  }

  self$connect_page <- function(cancel = NULL, deadline = NULL) {
    page <- self$page()
    devtools_socket(oget(page, "webSocketDebuggerUrl", ""), self$port, cancel, deadline)
  }

  self$inspect <- function(cdp) {
    tree <- oget(cdp$call("Page.getFrameTree"), "frameTree", empty_obj())
    frames <- list()
    collect <- function(node) {
      frame <- oget(node, "frame", empty_obj())
      u <- oget(frame, "url", "")
      if (self$allowed(if (is_str(u)) u else "")) frames[[length(frames) + 1L]] <<- frame
      for (child in oget(node, "childFrames", list())) collect(child)
    }
    collect(tree)
    script <- asset_text("copilot_dom.js")
    best <- NULL
    for (frame in frames) {
      found <- tryCatch({
        context <- oget(cdp$call("Page.createIsolatedWorld", list(frameId = frame$id, worldName = "QualiTranslate",
                                                                  grantUniveralAccess = FALSE)), "executionContextId")
        list(context = context, info = cdp$evaluate(script, context))
      }, qt_browser_error = function(e) {
        if (identical(e$code, "cancelled")) stop(e)
        NULL
      })
      if (is.null(found) || !is_obj(found$info)) next
      info <- found$info
      if (truthy(info$challenge) || truthy(info$limit)) copilot_attention(info)
      if (truthy(info$has_editor)) return(found)
      if (is.null(best)) best <- found
    }
    if (!is.null(best)) return(best)
    browser_error("Copilot Chat is still loading or its message box is not visible yet.", "loading")
  }

  self$wait_for <- function(cdp, test, seconds, cancel, step_message) {
    deadline <- qt_clock() + seconds
    while (qt_clock() < deadline) {
      cancelled_check(cancel)
      found <- tryCatch({
        found <- self$inspect(cdp)
        copilot_attention(found$info)
        if (isTRUE(test(found$info))) found else NULL
      }, qt_browser_error = function(e) {
        if (e$code %in% c("cancelled", "verification", "usage_limit", "sign_in", "browser_closed", "reloaded")) stop(e)
        NULL
      })
      if (!is.null(found)) return(found)
      qt_wait(0.3)
    }
    browser_error(step_message, "timeout")
  }

  # ----- connection check (no prompt is ever sent) ---------------------------------------------
  self$check <- function() {
    cdp <- NULL
    on.exit(if (!is.null(cdp)) cdp$close(), add = TRUE)
    tryCatch({
      if (!is.null(self$process) && !self$process$is_alive() && !self$alive()) {
        self$forget_connection()
        browser_error("The Copilot window was closed. Click Connect & translate with Copilot (or Resume with Copilot) to open it again.", "browser_closed")
      }
      cdp <- self$connect_page(deadline = qt_clock() + 10)
      found <- self$inspect(cdp); info <- found$info
      copilot_attention(info)
      if (!truthy(info$has_editor)) browser_error("Copilot Chat is open but its message box is not visible yet.", "loading")
      self$update(open = TRUE, ready = TRUE, error_code = "",
                  message = "Connected: you are signed in and the Copilot message box was found. Nothing has been sent yet.",
                  model_label = oget(info, "model_label", ""), browser = self$browser_version)
    }, qt_browser_error = function(e) {
      self$update(ready = FALSE, error_code = e$code, message = conditionMessage(e))
    })
    self$summary()
  }

  self$alive <- function() {
    tryCatch({ self$http("/json/version"); TRUE }, qt_browser_error = function(e) FALSE)
  }

  # ----- one structured request ---------------------------------------------------------------
  # Navigate to a new chat, switch on Temporary chat if requested, and verify it is empty.
  self$fresh_chat <- function(cdp, cancel) {
    self$set_step("new_chat", "Opening a new empty Copilot chat")
    old_load <- tryCatch(oget(self$inspect(cdp)$info, "page_load_id"), qt_browser_error = function(e) {
      if (identical(e$code, "cancelled")) stop(e)
      NULL
    })
    cdp$call("Page.navigate", list(url = self$start_url), timeout = 15)
    start_path <- sub("/+$", "", url_split(self$start_url)$path)
    if (!nzchar(start_path)) start_path <- "/"
    empty <- function(info) {
      path <- sub("/+$", "", oget(info, "path", ""))
      if (!nzchar(path)) path <- "/"
      # Only the newly loaded page counts: the previous page can look empty while it unloads.
      truthy(info$page_load_id) && !identical(info$page_load_id, old_load) &&
        truthy(info$has_editor) && truthy(info$composer_empty) && identical(as.numeric(info$turn_count), 0) &&
        !truthy(info$stopping) && !truthy(info$previous_receipt_present) && identical(path, start_path)
    }
    settled <- function(info) {
      empty(info) && identical(info$ready_state, "complete") && as.numeric(oget(info, "page_age_ms", 0)) >= self$settle_ms
    }
    found <- self$wait_for(cdp, settled, self$new_chat_timeout, cancel,
                           "A new empty Copilot chat could not be confirmed. Nothing was sent. Check the Copilot window, then use Retry.")
    isolation <- "new_empty_chat_verified"; notice <- ""
    if (isTRUE(self$temporary_chat)) {
      self$set_step("new_chat", "Switching on Copilot temporary chat")
      if (!truthy(found$info$has_temporary)) {
        browser_error(paste0("Copilot did not show its Temporary chat switch, so nothing was sent. To continue with ordinary (saved) chats, ",
                             "turn off \"Use Copilot temporary chats\" in Advanced settings and record that change."), "temporary_unavailable")
      }
      if (!truthy(found$info$temporary_on)) {
        cdp$evaluate("globalThis.__qtUI.temporaryButton.click(); true", found$context)
        qt_wait(1.5)
      }
      found <- self$wait_for(cdp, function(i) truthy(i$temporary_on) && truthy(i$temporary_heading) && empty(i), 15, cancel,
                             "Copilot temporary chat could not be switched on and confirmed. Nothing was sent.")
      notice <- cdp$evaluate("globalThis.__qtUI.temporaryNotice()", found$context)
      if (!is_str(notice)) notice <- ""
      isolation <- "new_temporary_chat_verified"
    }
    list(context = found$context, info = found$info, isolation = isolation, notice = notice)
  }

  # `box$task` is the task; like the Python version, the transport records on it the exact
  # text placed in the message box (transport_prompt), submitted_at and any refusal text.
  self$complete <- function(box, cancel = NULL) {
    cdp <- NULL; submitted <- FALSE
    on.exit({
      self$set_step("idle", "")
      if (!is.null(cdp)) cdp$close()
    }, add = TRUE)
    reloaded_after_send <- "The Copilot page reloaded just after Send. The message may or may not have reached Copilot. Nothing was imported and it will not be resent automatically."
    tryCatch({
      cdp <- self$connect_page(cancel)
      fresh <- self$fresh_chat(cdp, cancel)
      context <- fresh$context; info <- fresh$info
      load_id <- info$page_load_id
      receipt <- paste0("QT_RECEIPT_", random_hex(16))
      packet <- paste0(box$task$system, "\n\nINPUT JSON:\n", box$task$user, "\n\n",
                       gsub("{receipt}", receipt, prompt_data()$reply_format, fixed = TRUE))
      box$task$transport_prompt <- packet
      self$last_receipt <- receipt
      cdp$evaluate(paste0("globalThis.__qtReceipt=", json_str(receipt), "; true"), context)
      self$set_step("inserting", "Putting the prepared text in the Copilot message box")
      cdp$evaluate("globalThis.__qtUI.editor.focus(); true", context)
      cdp$call("Input.insertText", list(text = packet), timeout = 60)
      wanted <- normalise(packet); visible <- ""
      deadline <- qt_clock() + 15; filled <- empty_obj()
      while (qt_clock() < deadline) {
        cancelled_check(cancel)
        found <- self$inspect(cdp); context <- found$context; filled <- found$info
        copilot_attention(filled)
        if (!identical(filled$page_load_id, load_id)) {
          browser_error("The Copilot page reloaded while the text was being prepared. Nothing was sent. Use Retry.", "reloaded")
        }
        visible <- cdp$evaluate("globalThis.__qtUI.composerText()", context)
        if (!is_str(visible)) visible <- ""
        if (truthy(filled$send_enabled) && identical(normalise(visible), wanted)) break
        qt_wait(0.3)
      }
      if (!identical(normalise(visible), wanted)) {
        got <- nchar_py(normalise(visible))
        browser_error(paste0("The Copilot message box did not accept the full text (", got, " of ", nchar_py(wanted),
                             " characters visible). Nothing was sent. Reduce the batch size in Advanced settings, then use Retry."), "composer_mismatch")
      }
      if (!truthy(filled$send_enabled)) {
        browser_error("The text is in the Copilot message box, but its Send button was not available. Nothing was sent.", "no_send")
      }
      box$task$transport_prompt <- editor_text(visible)
      cancelled_check(cancel)
      self$set_step("sending", "Pressing Send in Copilot")
      cdp$evaluate("globalThis.__qtUI.sendButton.click(); true", context)
      submitted <- TRUE
      box$task$submitted_at <- now_z()
      sent <- function(i) {
        if (!identical(i$page_load_id, load_id)) browser_error(reloaded_after_send, "reloaded")
        truthy(i$composer_empty) && (as.numeric(oget(i, "user_turns", 0)) >= 1 || as.numeric(oget(i, "reply_count", 0)) >= 1 || truthy(i$stopping))
      }
      found <- self$wait_for(cdp, sent, 25, cancel,
                             "Send was pressed, but Copilot did not show the message as sent. Check the Copilot window. The app will not resend it automatically.")
      context <- found$context; info <- found$info
      # Confirm Copilot actually started answering, and notice if the page silently cleared the message.
      self$set_step("waiting_reply", "Waiting for Copilot to start its reply")
      start_deadline <- qt_clock() + 60; cleared_since <- NULL
      repeat {
        cancelled_check(cancel)
        if (qt_clock() > start_deadline) {
          browser_error("Copilot showed the message but did not start a reply within 60 seconds. Nothing was imported and nothing was resent. Check the Copilot window, then use Retry.", "no_reply_started")
        }
        found <- tryCatch(self$inspect(cdp), qt_browser_error = function(e) {
          if (e$code %in% c("cancelled", "verification", "usage_limit", "browser_closed")) stop(e)
          NULL
        })
        if (is.null(found)) { qt_wait(0.4); next }
        context <- found$context; info <- found$info
        copilot_attention(info)
        if (!identical(info$page_load_id, load_id)) browser_error(reloaded_after_send, "reloaded")
        if (truthy(info$stopping) || as.numeric(oget(info, "reply_count", 0)) >= 1) break
        if (as.numeric(oget(info, "user_turns", 0)) == 0 && as.numeric(oget(info, "turn_count", 0)) == 0) {
          if (is.null(cleared_since)) cleared_since <- qt_clock()
          if (qt_clock() - cleared_since > 3) {
            browser_error("Copilot cleared the message before replying, so it was probably not processed. Nothing was imported and it was not resent automatically. Use Retry.", "message_cleared")
          }
        } else {
          cleared_since <- NULL
        }
        qt_wait(0.4)
      }
      self$set_step("waiting_reply", "Waiting for Copilot to finish its reply")
      deadline <- qt_clock() + self$timeout; last <- NULL; stable_since <- qt_clock(); cleared_since <- NULL
      last_text <- NULL; text_since <- qt_clock()
      while (qt_clock() < deadline) {
        cancelled_check(cancel)
        found <- tryCatch(self$inspect(cdp), qt_browser_error = function(e) {
          if (e$code %in% c("cancelled", "verification", "usage_limit", "browser_closed", "reloaded")) stop(e)
          NULL
        })
        if (is.null(found)) { qt_wait(0.5); next }
        context <- found$context; info <- found$info
        copilot_attention(info)
        if (!identical(info$page_load_id, load_id)) {
          browser_error("The Copilot page reloaded while its reply was being written. Nothing was imported and nothing was resent. Use Retry.", "reloaded")
        }
        if (!truthy(info$turn_count) && !truthy(info$stopping)) {
          if (is.null(cleared_since)) cleared_since <- qt_clock()
          if (qt_clock() - cleared_since > 3) {
            browser_error("The Copilot conversation was cleared before a complete reply arrived. Nothing was imported and nothing was resent. Use Retry.", "message_cleared")
          }
        } else {
          cleared_since <- NULL
        }
        text <- cdp$evaluate("globalThis.__qtUI.lastReplyText()", context)
        if (!is_str(text)) text <- ""
        if (!nzchar(text) && !truthy(info$reply_count)) {
          texts <- cdp$evaluate("globalThis.__qtUI.replyTexts()", context)
          text <- paste(unlist(Filter(is_str, texts %||% list())), collapse = "\n")
        }
        raw <- extract_receipt_json(text, receipt)
        if (!identical(text, last_text)) { last_text <- text; text_since <- qt_clock() }
        quiet <- qt_clock() - text_since
        if (is.null(raw) && nzchar(py_strip(text)) && !truthy(info$stopping) &&
            (quiet >= self$decline_quiet_seconds || (truthy(info$reply_done) && quiet >= 3))) {
          body <- py_strip(sub(paste0("^", WS, "*Copilot said:", WS, "*"), "", text, perl = TRUE))
          box$task$declined_reply <- substr(body, 1, 2000)
          flat <- gsub(paste0(WS, "+"), " ", body, perl = TRUE)
          if (!grepl("{", body, fixed = TRUE) && nchar_py(body) < 600 && grepl(RX_REFUSAL, body, perl = TRUE, ignore.case = TRUE)) {
            browser_error(paste0("Copilot declined or could not answer this batch. Its reply was: \"", substr(flat, 1, 220),
                                 "\" Nothing was imported and nothing was resent."), "declined")
          }
          if (!grepl("{", body, fixed = TRUE)) {
            browser_error(paste0("Copilot answered in ordinary text instead of the structured format, so nothing was imported ",
                                 "(its reply begins: \"", substr(flat, 1, 120), "\"). Nothing was resent. ",
                                 "Use Resume to try this batch again; if it repeats, reduce the batch size in Advanced settings."), "no_json")
          }
          browser_error(paste0("Copilot finished its reply, but it did not contain the complete structured result for this batch ",
                               "(the receipt or closing brackets are missing, for example because the reply was cut short). ",
                               "Nothing was imported. Reduce the batch size in Advanced settings, then use Retry."), "no_receipt")
        }
        if (!is.null(raw) && identical(raw, last) && !truthy(info$stopping)) {
          if (truthy(info$reply_done) || qt_clock() - stable_since >= 3) {
            self$set_step("validating", "Checking the reply")
            label <- oget(info, "model_label", "")
            self$update(open = TRUE, ready = TRUE, error_code = "", message = "Copilot reply received.", model_label = label)
            return(list(raw = raw, provenance = list(
              provider = "copilot_browser", model = "not reported by Copilot Chat",
              copilot_model_selector_label = if (truthy(label)) label else "not visible",
              integration = "visible_browser_ui", chat_isolation = fresh$isolation,
              temporary_chat_notice = fresh$notice, receipt_verified = TRUE,
              browser = self$browser_version, submitted_at = box$task$submitted_at %||% "",
              completed_at = now_z())))
          }
        } else if (!identical(raw, last)) {
          last <- raw; stable_since <- qt_clock()
        }
        qt_wait(0.5)
      }
      browser_error(paste0("Copilot did not return a complete reply with the expected receipt within ", as.integer(self$timeout),
                           " seconds. Its reply stays visible in the Copilot window. Nothing was imported and nothing was resent."), "reply_timeout")
    }, qt_browser_error = function(e) {
      if (submitted && !identical(e$code, "cancelled") && !nzchar(e$code %||% "")) e$code <- "after_submit"
      self$update(ready = self$alive(), error_code = e$code, message = conditionMessage(e))
      stop(e)
    })
  }

  # ----- closing ------------------------------------------------------------------------------
  self$close <- function(forget = FALSE) {
    if (is.null(self$browser_ws)) {
      # The window may still be open from an earlier app session: reconnect so it can be closed.
      # The profile folder is not created here if it does not exist yet.
      tryCatch({
        profile <- self$profile(create = FALSE); portfile <- file.path(profile, "DevToolsActivePort")
        if (file.exists(portfile)) {
          self$acquire(profile)
          pf <- self$read_portfile(portfile); self$attach(pf$port, pf$ws)
        }
      }, error = function(e) NULL)   # nothing open to reconnect to (or another copy holds it)
    }
    if (!is.null(self$browser_ws)) {
      tryCatch(self$browser_call("Browser.close", timeout = 3), error = function(e) NULL)
    }
    if (!is.null(self$process)) {
      p <- self$process
      end <- qt_clock() + 5
      while (p$is_alive() && qt_clock() < end) qt_wait(0.1)
      if (p$is_alive()) {
        tryCatch(if (.Platform$OS.type == "windows") p$kill() else p$signal(tools::SIGTERM), error = function(e) NULL)
      }
    }
    self$forget_connection()
    if (!is.null(self$temp_profile)) {
      unlink(self$temp_profile, recursive = TRUE, force = TRUE)
      self$temp_profile <- NULL
    }
    message <- "Copilot window closed. Your sign-in is kept in the dedicated QualiTranslate browser profile."
    if (forget) {
      profile <- self$profile_dir %||% default_profile_dir()
      portfile <- file.path(profile, "DevToolsActivePort")
      still_open <- FALSE
      if (file.exists(portfile)) {
        port <- tryCatch(self$read_portfile(portfile)$port, error = function(e) NULL)
        end <- qt_clock() + 6   # Chrome needs a moment to shut down
        while (!is.null(port) && qt_clock() < end) {
          up <- tryCatch({ self$http("/json/version", port); TRUE }, qt_browser_error = function(e) FALSE)
          if (up) {
            still_open <- TRUE; qt_wait(0.3)
          } else {
            still_open <- FALSE; break
          }
        }
      }
      if (still_open) {
        self$release()
        self$update(open = FALSE, ready = FALSE, error_code = "in_use",
                    message = "The Copilot window could not be closed, so the saved sign-in was NOT removed. Quit that Chrome window (Chrome > Quit), then use Forget again.")
        return(invisible())
      }
      if (dir.exists(profile) && identical(basename(profile), PROFILE_NAME)) {
        qt_wait(0.5)
        self$release()
        unlink(profile, recursive = TRUE, force = TRUE)
      }
      message <- "Copilot window closed and the saved Copilot sign-in was removed from this computer. Copilot conversation history in Microsoft 365 is not affected."
    }
    self$release()
    self$update(open = FALSE, ready = FALSE, error_code = "", message = message)
    invisible()
  }

  self
}

copilot_attention <- function(info) {
  if (truthy(oget(info, "authentication_required"))) browser_error("Sign in to Copilot yourself in its browser window.", "sign_in")
  if (truthy(oget(info, "challenge"))) {
    browser_error("Copilot is asking for human verification. Resolve it yourself in that window; the app will not bypass it.", "verification")
  }
  if (truthy(oget(info, "limit"))) {
    browser_error("Copilot reports a usage limit, a message-length limit, or a temporary restriction. Stop and resume later or reduce the batch size; completed batches are retained.", "usage_limit")
  }
  invisible()
}

# Non-blocking connection watcher. Checks the page only; never sends a prompt.
# The Python version runs it in a thread; here the main loop calls tick() when it is due,
# one step (open the window, or one check) at a time.
new_copilot_connection <- function(browser, timeout = 600, interval = 2) {
  self <- new.env(parent = emptyenv())
  self$browser <- browser; self$timeout <- timeout; self$interval <- interval
  self$cancel_event <- qt_event(); self$active <- FALSE; self$opened <- FALSE; self$next_at <- 0
  self$phase <- "idle"; self$message <- ""; self$last_check <- ""; self$started <- 0; self$waiting_for <- ""

  self$summary <- function() {
    result <- self$browser$summary()
    connecting <- self$phase %in% c("opening", "waiting")
    result$connecting <- connecting
    result$connection_phase <- self$phase
    result$waiting_for <- self$waiting_for
    result$connection_elapsed <- if (connecting) as.integer(qt_clock() - self$started) else 0L
    result$connection_timeout <- as.integer(self$timeout)
    result$last_check <- self$last_check
    if (self$phase %in% c("opening", "waiting", "cancelled", "timeout", "error")) {
      result$ready <- FALSE
      result$message <- self$message
    }
    result
  }

  self$start <- function() {
    if (isTRUE(self$active)) {
      if (self$cancel_event$is_set()) browser_error("The previous connection check is stopping. Please retry in a few seconds.")
    } else {
      self$cancel_event <- qt_event(); self$started <- qt_clock()
      self$phase <- "opening"; self$last_check <- ""; self$waiting_for <- "browser"
      self$message <- "Opening the Copilot window\u2026"
      self$active <- TRUE; self$opened <- FALSE; self$next_at <- qt_clock()
    }
    self$summary()
  }

  self$cancel <- function() {
    self$cancel_event$set(); self$phase <- "cancelled"
    self$message <- "Connection cancelled. Nothing was sent to Copilot. You can retry or save your project."
    self$summary()
  }

  self$due <- function() isTRUE(self$active) && qt_clock() >= self$next_at

  self$tick <- function() {
    cancel <- self$cancel_event
    if (cancel$is_set()) { self$active <- FALSE; return(invisible()) }
    tryCatch({
      if (!self$opened) {
        self$browser$open()
        self$opened <- TRUE
        if (cancel$is_set()) { self$active <- FALSE; return(invisible()) }
      }
      info <- self$browser$check()
      if (cancel$is_set()) { self$active <- FALSE; return(invisible()) }
      self$last_check <- oget(info, "message", "")
      code <- oget(info, "error_code", "")
      if (truthy(oget(info, "ready"))) {
        self$phase <- "ready"; self$message <- ""; self$waiting_for <- ""; self$active <- FALSE
      } else if (is_str(code) && code %in% c("verification", "usage_limit", "browser_closed", "no_browser", "no_control", "in_use")) {
        self$phase <- "error"; self$message <- paste0("Connection needs your attention. Nothing was sent. ", self$last_check); self$active <- FALSE
      } else if (qt_clock() - self$started >= self$timeout) {
        self$phase <- "timeout"
        self$message <- paste0("Still not connected after ", as.integer(self$timeout %/% 60), " minutes. Nothing was sent. Last check: ",
                               self$last_check, " Click Retry when the Copilot window shows the chat page.")
        self$active <- FALSE
      } else {
        self$phase <- "waiting"
        waiting <- c(sign_in = "sign_in", other_page = "sign_in", loading = "loading")
        self$waiting_for <- if (is_str(code) && code %in% names(waiting)) waiting[[code]] else "controls"
        self$message <- self$last_check
        self$next_at <- qt_clock() + self$interval
      }
    }, error = function(e) {
      self$active <- FALSE
      if (cancel$is_set()) return(invisible())
      self$phase <- "error"
      detail <- if (is_qt_message_error(e) || inherits(e, "simpleError")) conditionMessage(e) else "The browser connection could not be completed."
      self$message <- paste0("Connection stopped. Nothing was sent. ", detail)
    })
    invisible()
  }

  # Runs the watcher to completion (used by tests and scripts; the app uses tick()).
  self$run <- function() {
    while (isTRUE(self$active)) {
      if (self$due()) self$tick() else qt_wait(max(0.001, min(0.05, self$next_at - qt_clock())))
    }
    invisible()
  }
  self
}
