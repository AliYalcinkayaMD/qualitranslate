# Real headless Chromium against a local stand-in for the signed-in Microsoft 365 Copilot Chat
# page, modelled on the structure observed live on 27 Oct 2026 (ids, data-testid values, labels,
# Lexical zero-width tail, Send appearing only with text, Temporary chat toggle, "Copilot said:"
# replies in a code block, Copy Response when finished). Fixture behaviour is not evidence of
# live compatibility. Set QT_TEST_CHROME to a test Chromium executable to run these tests.

chrome <- Sys.getenv("QT_TEST_CHROME")

skip_if_no_chrome <- function() {
  skip_on_cran()
  skip_if(!nzchar(chrome) || !file.exists(chrome), "Set QT_TEST_CHROME to a test Chromium executable for browser integration checks.")
}

fixture_server <- function() {
  html <- readBin(test_path("fixtures", "copilot_fixture.html"), "raw", file.size(test_path("fixtures", "copilot_fixture.html")))
  events <- new.env(parent = emptyenv())
  events$list <- list()
  app <- list(call = function(req) {
    if (identical(req$REQUEST_METHOD, "POST")) {
      data <- rawToChar(req$rook.input$read()); Encoding(data) <- "UTF-8"
      events$list[[length(events$list) + 1L]] <- list(event = sub("^.*event=", "", req$QUERY_STRING), data = data)
      return(list(status = 204L, headers = list(), body = raw(0)))
    }
    events$list[[length(events$list) + 1L]] <- list(event = "load", data = req$PATH_INFO)
    list(status = 200L, headers = list("Content-Type" = "text/html; charset=utf-8"), body = html)
  })
  port <- httpuv::randomPort()
  server <- httpuv::startServer("127.0.0.1", port, app)
  list(server = server, port = port, events = events,
       sends = function() unlist(lapply(Filter(function(e) e$event == "send", events$list), function(e) e$data)),
       clear = function() events$list <- list())
}

launch <- function(fx, query = "", settle = 0.3, profile = NULL, ...) {
  profile <- profile %||% tempfile("qt-test-profile-")
  browser <- qt("new_copilot_browser")(executable = chrome, start_url = paste0("http://127.0.0.1:", fx$port, "/", query),
                                       allow_fixture = TRUE, profile_dir = profile, settle_seconds = settle,
                                       extra_args = c("--headless=new", "--no-sandbox", "--disable-dev-shm-usage", "--disable-gpu"),
                                       timeout = 8, ...)
  # Close the browser even when opening or the ready check fails.
  ok <- FALSE
  on.exit(if (!ok) shutdown(browser), add = TRUE)
  browser$open()
  end <- Sys.time() + 15
  while (Sys.time() < end) {
    if (isTRUE(browser$check()$ready)) { ok <- TRUE; return(browser) }
    qt("qt_wait")(0.1)
  }
  stop(browser$summary()$message)
}

shutdown <- function(browser) {
  try(browser$close(), silent = TRUE)
  if (!is.null(browser$profile_dir)) unlink(browser$profile_dir, recursive = TRUE, force = TRUE)
}

browser_workspace <- function(browser, text = "Interviewer: SOURCE_SENTINEL_91\nParticipant A: ORIGINAL_DANISH_92") {
  ws <- qt("new_workspace")(fake_client(), browser)
  qt("ws_load_text")(ws, text, "Fixture")
  settings <- qt("settings_defaults")()
  for (st in names(settings$stages)) { settings$stages[[st]]$provider <- "copilot"; settings$stages[[st]]$chat_tool <- "Institutional Copilot" }
  qt("ws_save_settings")(ws, settings)
  ws$copilot_connection$phase <- "ready"
  ws
}

`%||%` <- function(a, b) if (is.null(a)) b else a

test_that("three stages run in verified temporary chats with receipts and a blind back-translation", {
  skip_if_no_chrome()
  fx <- fixture_server(); on.exit(httpuv::stopServer(fx$server), add = TRUE)
  browser <- launch(fx); on.exit(shutdown(browser), add = TRUE)
  persian <- intToUtf8(c(0x0645, 0x06cc, 0x200c, 0x062e, 0x0648, 0x0627, 0x0647, 0x0645))
  ws <- browser_workspace(browser, paste0("Interviewer: SOURCE_SENTINEL_91\nParticipant A: ORIGINAL_DANISH_92 ", persian))
  start_and_run(ws, "all")
  expect_identical(ws$job$error, "")
  expect_length(ws$project$runs, 3)
  sent <- fx$sends()
  expect_length(sent, 3)
  expect_false(grepl("SOURCE_SENTINEL_91", sent[2])); expect_false(grepl("ORIGINAL_DANISH_92", sent[2]))
  expect_identical(trimws(ws$project$runs[[2]]$transport_prompt), trimws(sent[2]))
  for (run in ws$project$runs) {
    expect_identical(run$provenance$chat_isolation, "new_temporary_chat_verified")
    expect_true(run$provenance$receipt_verified)
    expect_match(run$provenance$temporary_chat_notice, "won't be saved", fixed = TRUE)
    expect_identical(run$provenance$copilot_model_selector_label, "Auto")
    expect_false(grepl("\u200b", run$transport_prompt, fixed = TRUE))
  }
  expect_match(ws$project$segments[[1]]$forward$text, "\"quoted\" *star* _under_", fixed = TRUE)
  # Zero-width non-joiner (Persian) is meaningful text: kept in replies and in the sent record.
  zwnj <- intToUtf8(c(0x06cc, 0x200c, 0x062e))
  expect_true(grepl(zwnj, ws$project$segments[[1]]$forward$text, fixed = TRUE))
  expect_true(grepl(zwnj, ws$project$runs[[1]]$transport_prompt, fixed = TRUE))
  expect_false(endsWith(ws$project$runs[[1]]$transport_prompt, "\u200c"))
  loads <- sum(vapply(fx$events$list, function(e) e$event == "load", logical(1)))
  expect_gte(loads, 4)
})

test_that("an ordinary chat is used when temporary chat is switched off", {
  skip_if_no_chrome()
  fx <- fixture_server(); on.exit(httpuv::stopServer(fx$server), add = TRUE)
  browser <- launch(fx); on.exit(shutdown(browser), add = TRUE)
  ws <- browser_workspace(browser, "P: SOURCE")
  settings <- ws$project$settings; settings$copilot$temporary_chat <- FALSE
  qt("ws_save_settings")(ws, settings)
  start_and_run(ws, "forward")
  expect_identical(ws$job$error, "")
  expect_identical(ws$project$runs[[1]]$provenance$chat_isolation, "new_empty_chat_verified")
})

test_that("a missing temporary-chat switch stops before anything is sent", {
  skip_if_no_chrome()
  fx <- fixture_server(); on.exit(httpuv::stopServer(fx$server), add = TRUE)
  browser <- launch(fx, "?no_temp"); on.exit(shutdown(browser), add = TRUE)
  ws <- browser_workspace(browser, "P: SOURCE")
  start_and_run(ws, "all")
  expect_match(ws$job$error, "Temporary chat switch", fixed = TRUE)
  expect_length(fx$sends(), 0)
  expect_length(ws$project$runs, 0)
})

test_that("a message sent before the page settles is not lost silently", {
  skip_if_no_chrome()
  # Live Copilot silently cleared a message sent ~1-2 s after the page loaded (27 Oct 2026).
  fx <- fixture_server(); on.exit(httpuv::stopServer(fx$server), add = TRUE)
  browser <- launch(fx, "?early_reset", settle = 0); on.exit(shutdown(browser), add = TRUE)
  ws <- browser_workspace(browser, "P: SOURCE")
  start_and_run(ws, "forward")
  expect_match(ws$job$error, "cleared the message", fixed = TRUE)
  expect_length(ws$project$runs, 0)
  browser$settle_ms <- 4500; fx$clear()
  start_and_run(ws, "forward")
  expect_identical(ws$job$error, "")
  expect_length(fx$sends(), 1)
  expect_false(is.null(ws$project$segments[[1]]$forward))
})

test_that("a declined section is isolated and the rest is translated", {
  skip_if_no_chrome()
  fx <- fixture_server(); on.exit(httpuv::stopServer(fx$server), add = TRUE)
  browser <- launch(fx); on.exit(shutdown(browser), add = TRUE)
  browser$decline_quiet_seconds <- 1.5
  text <- paste(sprintf("P: Line %d%s", 1:5, ifelse(1:5 == 3, " DECLINE_ME", "")), collapse = "\n")
  ws <- browser_workspace(browser, text)
  start_and_run(ws, "all")
  expect_match(ws$job$error, "declined", fixed = TRUE); expect_match(ws$job$error, "S0003", fixed = TRUE)
  done <- vapply(ws$project$segments, function(s) !is.null(s$forward), logical(1))
  expect_identical(unname(done), c(TRUE, TRUE, FALSE, TRUE, TRUE))
  expect_identical(vapply(ws$job$declined, function(d) d$id, ""), "S0003")
  neighbours <- Filter(function(r) identical(unlist(r$segment_ids), "S0002") || identical(unlist(r$segment_ids), "S0004"), ws$project$runs)
  expect_gt(length(neighbours), 0)
  expect_true(all(vapply(neighbours, function(r) isTRUE(r$context_omitted) && !grepl("DECLINE_ME", r$user_prompt), logical(1))))
  expect_false(any(vapply(ws$project$segments, function(s) !is.null(s$back), logical(1))))
  qt("ws_edit")(ws, "S0003", "forward", "Human translation of line 3", "Entered by a fictional reviewer after Copilot declined")
  expect_identical(ws$project$segments[[3]]$forward$run_id, "human_entry")
  fx$clear()
  start_and_run(ws, "all")
  # The human forward text passes blind back-translation; reconciliation still carries the original.
  expect_match(ws$job$error, "reconciliation: S0003", fixed = TRUE)
  expect_true(all(vapply(ws$project$segments, function(s) !is.null(s$back), logical(1))))
  missing <- vapply(Filter(function(s) is.null(s$reconcile), ws$project$segments), function(s) s$id, "")
  expect_identical(missing, "S0003")
  qt("ws_edit")(ws, "S0003", "reconcile", "Human proposed translation 3", "Reconciled by a fictional reviewer")
  expect_identical(ws$project$segments[[3]]$reconcile$run_id, "human_entry")
  expect_gt(length(qt("export_bundle")(ws$project)), 0)
  again <- qt("new_workspace")(fake_client(), scripted_copilot())
  qt("ws_import_project")(again, qt("json_parse")(qt("json_str")(ws$project)))
  expect_false(is.null(again$project$segments[[3]]$forward$human_entry))
  expect_false(is.null(again$project$segments[[3]]$reconcile$human_entry))
})

test_that("a usage limit stops without an automatic resend", {
  skip_if_no_chrome()
  fx <- fixture_server(); on.exit(httpuv::stopServer(fx$server), add = TRUE)
  browser <- launch(fx, "?limit"); on.exit(shutdown(browser), add = TRUE)
  ws <- browser_workspace(browser, "P: SOURCE")
  start_and_run(ws, "all")
  expect_match(ws$job$error, "usage limit", fixed = TRUE)
  expect_identical(ws$job$error_code, "usage_limit")
  expect_length(ws$project$runs, 0)
  expect_length(fx$sends(), 1)
})

test_that("a missing receipt never imports the echoed input", {
  skip_if_no_chrome()
  fx <- fixture_server(); on.exit(httpuv::stopServer(fx$server), add = TRUE)
  browser <- launch(fx, "?no_receipt"); on.exit(shutdown(browser), add = TRUE)
  ws <- browser_workspace(browser, "P: SOURCE")
  box <- new.env(); box$task <- qt("ws_prepare_task")(ws, "forward")
  err <- tryCatch(browser$complete(box), error = function(e) e)
  expect_s3_class(err, "qt_browser_error")
  expect_match(conditionMessage(err), "complete structured result", fixed = TRUE)
  expect_null(ws$project$segments[[1]]$forward)
  expect_length(fx$sends(), 1)
})

test_that("the app re-attaches to the open dedicated window without a second browser", {
  skip_if_no_chrome()
  fx <- fixture_server(); on.exit(httpuv::stopServer(fx$server), add = TRUE)
  browser <- launch(fx); on.exit(shutdown(browser), add = TRUE)
  port <- browser$port
  again <- qt("new_copilot_browser")(executable = chrome, start_url = browser$start_url, allow_fixture = TRUE, profile_dir = browser$profile_dir)
  err <- tryCatch(again$open(), error = function(e) e)       # one copy at a time
  expect_match(conditionMessage(err), "Another QualiTranslate copy", fixed = TRUE)
  expect_identical(err$code, "in_use")
  browser$release()   # as if the first app copy had quit, leaving its window open
  again$open(); on.exit(again$release(), add = TRUE)
  expect_identical(again$port, port)
  expect_true(again$check()$ready)
})

test_that("Forget closes a window left open by an earlier session and removes only its profile", {
  skip_if_no_chrome()
  fx <- fixture_server(); on.exit(httpuv::stopServer(fx$server), add = TRUE)
  base <- tempfile("qt-forget-"); dir.create(base); on.exit(unlink(base, recursive = TRUE), add = TRUE)
  profile <- file.path(base, "copilot-browser-profile")
  first <- qt("new_copilot_browser")(executable = chrome, start_url = paste0("http://127.0.0.1:", fx$port, "/"), allow_fixture = TRUE,
                                     profile_dir = profile, extra_args = c("--headless=new", "--no-sandbox", "--disable-gpu"))
  first$open(); port <- first$port; first$release(); first$forget_connection()   # app restarted; window still open
  later_browser <- qt("new_copilot_browser")(executable = chrome, allow_fixture = TRUE, profile_dir = profile)
  later_browser$close(forget = TRUE)
  expect_false(dir.exists(profile), info = later_browser$summary()$message)
  expect_true(dir.exists(base))
  up <- tryCatch({ curl::curl_fetch_memory(sprintf("http://127.0.0.1:%d/json/version", port), handle = curl::handle_setopt(curl::new_handle(), noproxy = "*", timeout = 1)); TRUE },
                 error = function(e) FALSE)
  expect_false(up)
})

test_that("a prose reply is not treated as a refusal and is not resent", {
  skip_if_no_chrome()
  fx <- fixture_server(); on.exit(httpuv::stopServer(fx$server), add = TRUE)
  browser <- launch(fx, "?prose"); on.exit(shutdown(browser), add = TRUE)
  browser$decline_quiet_seconds <- 1.5
  ws <- browser_workspace(browser, "P: one\nP: two")
  start_and_run(ws, "all")
  expect_match(ws$job$error, "ordinary text", fixed = TRUE)
  expect_identical(ws$job$error_code, "no_json")
  expect_length(fx$sends(), 1)
  expect_length(ws$job$declined, 0)
  expect_length(ws$project$runs, 0)
})

test_that("the app keeps answering state polls and Stop while Copilot batches are under way", {
  skip_if_no_chrome()
  fx <- fixture_server(); on.exit(httpuv::stopServer(fx$server), add = TRUE)
  browser <- launch(fx); on.exit(shutdown(browser), add = TRUE)
  ws <- browser_workspace(browser, paste(sprintf("P: line %d", 1:4), collapse = "\n"))
  settings <- ws$project$settings; settings$batch_segments <- 1L
  qt("ws_save_settings")(ws, settings)
  srv <- start_test_server(ws); on.exit(httpuv::stopServer(srv$server), add = TRUE)
  expect_identical(api(srv, "run", list(stage = "forward"), loop = FALSE)$status, 200L)
  seen <- list()
  fire <- function(path, body = NULL) {
    h <- curl::new_handle()
    curl::handle_setopt(h, noproxy = "*", timeout = 10)
    curl::handle_setheaders(h, "X-QualiTranslate-Token" = srv$token, "Content-Type" = "application/json")
    if (!is.null(body)) curl::handle_setopt(h, post = 1L, copypostfields = charToRaw(qt("json_str")(body)))
    curl::curl_fetch_multi(paste0(srv$url, "/api/", path), pool = qt("qt_pool")(), handle = h,
                           done = function(r) seen[[length(seen) + 1L]] <<- list(path = path, json = qt("json_parse")(rawToChar(r$content))),
                           fail = function(m) seen[[length(seen) + 1L]] <<- list(path = path, error = m))
  }
  for (t in c(0.5, 1, 1.5)) later::later(function() fire("state"), t)
  # Stop once the first batch is in, while the second is being typed or answered.
  stopper <- function() if (length(ws$project$runs) >= 1) fire("stop", qt("empty_obj")()) else later::later(stopper, 0.1)
  later::later(stopper, 0.2)
  qt("qt_loop_once")(ws)        # runs the queued job with the real transport
  qt("qt_wait")(0.2)
  states <- Filter(function(s) identical(s$path, "state"), seen)
  expect_length(states, 3)
  expect_true(all(vapply(states, function(s) isTRUE(s$json$job$running), logical(1))))
  expect_true(any(vapply(states, function(s) nzchar(s$json$copilot$step_detail %||% ""), logical(1))))
  expect_match(ws$job$error, "Stopped", fixed = TRUE)
  expect_gte(length(ws$project$runs), 1)
  expect_lt(length(ws$project$runs), 4)
  # A message already sent when Stop arrived is never resent; at most that one has no run.
  expect_lte(length(fx$sends()) - length(ws$project$runs), 1)
})
