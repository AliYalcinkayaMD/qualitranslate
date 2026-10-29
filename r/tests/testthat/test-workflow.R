# Port of python/tests/test_workflow.py (pipeline, validation, providers, HTTP).

pipeline_ws <- function() {
  ws <- qt("new_workspace")(fake_client(), scripted_copilot())
  qt("ws_load_text")(ws, "Interviewer: SOURCE_SENTINEL_91\nParticipant A: Pain mattered to me.", "Test")
  ws
}

api_config <- function(ws, batch_segments = 12L) {
  settings <- qt("settings_defaults")()
  settings$batch_segments <- batch_segments
  settings$study_context <- "CONTEXT_SENTINEL_92"
  settings$glossary <- "GLOSSARY_SENTINEL_93"
  settings$prompt_additions$forward <- "FORWARD_ADDITION_SENTINEL_94"
  for (st in names(settings$stages)) settings$stages[[st]] <- list(provider = "mistral", model = "fixture-model", workspace_id = "")
  qt("ws_save_settings")(ws, settings)
  qt("ws_set_key")(ws, "mistral", "NEVER_EXPORT_THIS_KEY")
}

manual_forward <- function(ws) {
  task <- qt("ws_prepare_task")(ws, "forward")
  rows <- lapply(unlist(task$ids), function(id) list(id = id, text = paste0("TARGET_SENTINEL_", id), notes = list("FORWARD_NOTE_SENTINEL_95")))
  qt("ws_apply_manual")(ws, task$task_id, qt("json_str")(list(segments = rows)), "Institutional interface / version not reported")
}

test_that("the blind request contains no original, context or notes", {
  ws <- pipeline_ws(); api_config(ws); manual_forward(ws)
  task <- qt("ws_prepare_task")(ws, "back")
  request <- paste0(task$system, task$user)
  for (secret in c("SOURCE_SENTINEL_91", "CONTEXT_SENTINEL_92", "GLOSSARY_SENTINEL_93", "FORWARD_ADDITION_SENTINEL_94",
                   "FORWARD_NOTE_SENTINEL_95", "Pain mattered", "Participant A", "NEVER_EXPORT_THIS_KEY")) {
    expect_false(grepl(secret, request, fixed = TRUE), info = secret)
  }
  expect_match(request, "TARGET_SENTINEL_S0001", fixed = TRUE)
})

test_that("back-translation refuses custom source context", {
  ws <- pipeline_ws()
  settings <- qt("settings_defaults")(); settings$prompt_additions$back <- "Here is the original interview"
  expect_error(qt("ws_save_settings")(ws, settings), class = "qt_workflow_error")
})

test_that("stage prerequisites are enforced", {
  ws <- pipeline_ws()
  expect_error(qt("ws_prepare_task")(ws, "back"), class = "qt_workflow_error")
  manual_forward(ws)
  expect_error(qt("ws_prepare_task")(ws, "reconcile"), class = "qt_workflow_error")
})

test_that("the complete pipeline uses separate requests", {
  ws <- pipeline_ws(); api_config(ws)
  start_and_run(ws, "all")
  expect_identical(ws$job$error, "")
  expect_identical(vapply(ws$client$requests, function(r) r$stage, ""), c("forward", "back", "reconcile"))
  for (s in ws$project$segments) {
    expect_true(s$reconcile$needs_human)
    expect_null(s$review)
  }
  payload <- qt("json_parse")(ws$client$requests[[3]]$user)
  expect_identical(payload$segments[[1]]$original_source, "SOURCE_SENTINEL_91")
  expect_false(is.null(payload$segments[[1]]$blind_back_translation))
})

test_that("a partial run resumes without repeating completed batches", {
  ws <- pipeline_ws(); api_config(ws, batch_segments = 1L)
  ws$client$fail_once <- TRUE
  start_and_run(ws, "forward")
  expect_match(ws$job$error, "Simulated", fixed = TRUE)
  expect_false(is.null(ws$project$segments[[1]]$forward))
  expect_null(ws$project$segments[[2]]$forward)
  first_ids <- ws$client$requests[[1]]$ids
  start_and_run(ws, "forward")
  expect_false(identical(ws$client$requests[[length(ws$client$requests)]]$ids, first_ids))
  expect_true(all(vapply(ws$project$segments, function(s) !is.null(s$forward), logical(1))))
})

test_that("a pending prompt cannot be applied after the source changes", {
  ws <- pipeline_ws()
  task <- qt("ws_prepare_task")(ws, "forward")
  qt("ws_edit")(ws, "S0001", "source", "A different source")
  expect_error(qt("ws_apply_manual")(ws, task$task_id, '{"segments":[]}', "Manual"), class = "qt_workflow_error")
})

test_that("a forward edit invalidates all downstream context", {
  ws <- pipeline_ws(); api_config(ws)
  start_and_run(ws, "all")
  qt("ws_review")(ws, "S0002", "Reviewed", "Reviewer", "")
  qt("ws_edit")(ws, "S0001", "forward", "Changed translation")
  expect_true(all(vapply(ws$project$segments, function(s) !is.null(s$forward), logical(1))))
  expect_true(all(vapply(ws$project$segments, function(s) is.null(s$back) && is.null(s$reconcile) && is.null(s$review), logical(1))))
  expect_length(ws$project$runs, 3)
})

test_that("a glossary change invalidates outputs but a batch size change does not", {
  ws <- pipeline_ws(); manual_forward(ws)
  settings <- ws$project$settings; settings$batch_segments <- 1L
  qt("ws_save_settings")(ws, settings)
  expect_true(all(vapply(ws$project$segments, function(s) !is.null(s$forward), logical(1))))
  settings$glossary <- "New glossary"
  qt("ws_save_settings")(ws, settings)
  expect_true(all(vapply(ws$project$segments, function(s) is.null(s$forward), logical(1))))
})

test_that("the demonstration is synthetic and never calls a provider", {
  ws <- pipeline_ws()
  qt("ws_load_text")(ws, qt("DEMO_SOURCE"), "Example", demo = TRUE)
  start_and_run(ws, "all", demo = TRUE)
  expect_identical(ws$job$error, "")
  expect_length(ws$client$requests, 0)
  row <- ws$project$segments[[4]]
  expect_match(row$reconcile$text, "not only", fixed = TRUE)
  expect_identical(row$reconcile$issues[[1]]$category, "negation")
  expect_error(qt("ws_review")(ws, row$id, row$reconcile$text, "AY", ""), class = "qt_workflow_error")
  qt("ws_review")(ws, row$id, row$reconcile$text, "AY", "Restored missing negation after source check.")
  expect_identical(ws$project$segments[[4]]$review$status, "reviewed")
})

test_that("the demonstration refuses real data", {
  ws <- pipeline_ws()
  expect_error(qt("ws_start_job")(ws, "all", demo = TRUE), class = "qt_workflow_error")
  expect_false(ws$job$running)
})

test_that("keys are not in the state, the project or the exports", {
  ws <- pipeline_ws(); api_config(ws)
  start_and_run(ws, "all")
  expect_false(grepl("NEVER_EXPORT_THIS_KEY", qt("json_str")(qt("ws_snapshot")(ws)), fixed = TRUE))
  zipfile <- tempfile(fileext = ".zip"); on.exit(unlink(zipfile))
  writeBin(qt("export_bundle")(ws$project), zipfile)
  dir <- tempfile(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  zip::unzip(zipfile, exdir = dir)
  for (f in list.files(dir, full.names = TRUE)) {
    expect_false(grepl("NEVER_EXPORT_THIS_KEY", rawToChar(readBin(f, "raw", file.size(f))), fixed = TRUE))
  }
})

test_that("restore keeps results, prompts and human review", {
  ws <- pipeline_ws(); api_config(ws)
  start_and_run(ws, "all")
  qt("ws_review")(ws, "S0001", "Reviewed target", "AY", "Checked")
  restored <- qt("new_workspace")(fake_client(), scripted_copilot())
  qt("ws_import_project")(restored, qt("json_parse")(qt("json_str")(ws$project)))
  expect_identical(restored$project$segments[[1]]$review$text, "Reviewed target")
  expect_identical(restored$project$runs[[1]]$system_prompt, ws$project$runs[[1]]$system_prompt)
  expect_false(is.null(restored$project$runs[[1]]$raw_response))
  expect_length(restored$keys, 0)
})

test_that("the audit hash chain covers recorded actions", {
  ws <- pipeline_ws(); manual_forward(ws)
  previous <- ""
  for (event in ws$project$audit) {
    expect_identical(event$previous_hash, previous)
    expect_identical(event$hash, qt("digest")(event[names(event) != "hash"]))
    previous <- event$hash
  }
  # The chain survives a JSON round trip (a saved and re-read project).
  events <- qt("json_parse")(qt("json_str")(ws$project$audit))
  for (event in events) expect_identical(event$hash, qt("digest")(event[names(event) != "hash"]))
})

test_that("an invalid title or project metadata preserves existing work", {
  ws <- pipeline_ws(); manual_forward(ws)
  before <- qt("json_str")(ws$project)
  expect_error(qt("ws_load_text")(ws, "Participant: Replacement text", ""), class = "qt_workflow_error")
  expect_identical(qt("json_str")(ws$project), before)
  for (field in c("title", "project_id")) {
    value <- qt("json_parse")(before); value[[field]] <- ""
    expect_error(qt("ws_import_project")(ws, value), class = "qt_workflow_error")
    expect_identical(qt("json_str")(ws$project), before)
  }
})

test_that("missing, duplicated, reordered or invented ids are rejected atomically", {
  for (ids in list("S0001", c("S0001", "S0001"), c("S0002", "S0001"), c("S0001", "S9999"))) {
    raw <- qt("json_str")(list(segments = lapply(ids, function(id) list(id = id, text = "Text", notes = list()))))
    expect_error(qt("parse_response")(raw, "forward", list("S0001", "S0002")), "Segment ids", class = "qt_workflow_error")
  }
})

test_that("an empty translation or non-JSON is rejected", {
  for (raw in c('{"segments":[{"id":"S0001","text":""}]}', "Here is a translation: hello")) {
    expect_error(qt("parse_response")(raw, "forward", list("S0001")), class = "qt_workflow_error")
  }
})

test_that("code-fenced JSON is supported", {
  raw <- '```json\n{"segments":[{"id":"S0001","text":"Text","notes":[]}]}\n```'
  expect_identical(qt("parse_response")(raw, "forward", list("S0001"))[[1]]$text, "Text")
})

test_that("Unicode turns and multi-line speech are preserved", {
  text <- paste0("G", intToUtf8(0xf6), "r", intToUtf8(0xfc), intToUtf8(0x15f), "meci: A", intToUtf8(0x11f), "r", intToUtf8(0x131), "?\n",
                 "Hasta ", intToUtf8(0x3b1), ": ", intToUtf8(0xc7), "ok de", intToUtf8(0x11f), "ildi.\nAma bazen zordu.\n\n[duraklama]")
  segments <- qt("parse_transcript")(text)
  expect_length(segments, 3)
  expect_identical(segments[[2]]$speaker, paste0("Hasta ", intToUtf8(0x3b1)))
  expect_identical(segments[[2]]$source, paste0(intToUtf8(0xc7), "ok de", intToUtf8(0x11f), "ildi.\nAma bazen zordu."))
  expect_identical(segments[[3]]$speaker, paste0("Hasta ", intToUtf8(0x3b1)))
})

test_that("batching covers each segment once", {
  segments <- qt("parse_transcript")(paste(rep(paste0("P: ", strrep(intToUtf8(0xf6), 250)), 15), collapse = "\n"))
  settings <- qt("settings_defaults")(); settings$batch_chars <- 500L
  groups <- qt("make_batches")(segments, settings, "forward")
  expect_identical(unlist(groups), vapply(segments, function(s) s$id, ""))
  expect_true(all(lengths(groups) <= 2))
})

test_that("paragraph mode keeps an inline colon as content", {
  segments <- qt("parse_transcript")("Pain: it affected my life.\nI could not sleep.\n\nA separate paragraph.", split_speakers = FALSE)
  expect_length(segments, 2)
  expect_identical(segments[[1]]$speaker, "")
  expect_true(startsWith(segments[[1]]$source, "Pain: it affected"))
})

test_that("times and web addresses do not start speaker turns", {
  segments <- qt("parse_transcript")("P: We met at\n10: 30 in the morning\nhttps: //example.org was mentioned")
  expect_length(segments, 1)
})

test_that(".docx text and line breaks are read without external entities", {
  data <- fixture_docx(word_body("<w:p><w:r><w:t>Participant A: Pain</w:t><w:br/><w:t>was important.</w:t></w:r></w:p>"))
  expect_identical(qt("read_document")("test.docx", data)$text, "Participant A: Pain\nwas important.")
})

test_that(".docx with a DOCTYPE is rejected", {
  data <- fixture_docx('<!DOCTYPE foo [<!ENTITY a "bad">]><foo/>')
  expect_error(qt("read_document")("test.docx", data), "entity declarations", class = "qt_workflow_error")
})

test_that("text uploads must be UTF-8 and documents must be supported types", {
  latin1 <- openssl::base64_encode(as.raw(c(0x50, 0x3a, 0x20, 0xe6, 0x0a)))
  expect_error(qt("read_document")("x.txt", latin1), "UTF-8", class = "qt_workflow_error")
  expect_error(qt("read_document")("x.pdf", openssl::base64_encode(charToRaw("abc"))), "Supported transcript files", class = "qt_workflow_error")
  expect_error(qt("read_document")("x.txt", "not base64!"), "Invalid upload encoding", class = "qt_workflow_error")
  bom <- openssl::base64_encode(c(as.raw(c(0xef, 0xbb, 0xbf)), charToRaw("P: Hej\r\nI: Tak")))
  expect_identical(qt("read_document")("x.txt", bom)$text, "P: Hej\nI: Tak")
})

test_that("exports escape HTML and spreadsheet formulas", {
  ws <- qt("new_workspace")(fake_client(), scripted_copilot())
  qt("ws_load_text")(ws, 'P: =HYPERLINK("https://example.com")\nQ: <script>alert(1)</script>', "<img src=x onerror=alert(1)>")
  csv <- qt("raw_to_utf8")(qt("export_csv")(ws$project))
  report <- qt("raw_to_utf8")(qt("export_html")(ws$project))
  expect_match(csv, "'=HYPERLINK", fixed = TRUE)
  expect_match(csv, '"\'=HYPERLINK(""https://example.com"")"', fixed = TRUE)
  expect_false(grepl("<script>", report, fixed = TRUE))
  expect_match(report, "&lt;script&gt;", fixed = TRUE)
  expect_false(grepl("<img src=x", report, fixed = TRUE))
  expect_true(startsWith(csv, "\ufeffid,speaker,source"))
  expect_match(csv, "\r\n", fixed = TRUE)
})

capture_client <- function(reply) {
  client <- qt("new_api_client")()
  client$payloads <- list()
  client$request <- function(provider, key, workspace_id, path, payload = NULL) {
    client$payloads[[length(client$payloads) + 1L]] <- payload
    list(result = reply, request_id = "request-id")
  }
  client
}

test_that("an Anthropic back-translation request has no temperature and no original", {
  ws <- qt("new_workspace")(fake_client(), scripted_copilot())
  qt("ws_load_text")(ws, "P: Original source")
  ws$project$segments[[1]]$forward <- list(text = "Translated target", notes = list())
  ws$project$settings$stages$back <- list(provider = "anthropic", model = "chosen-model", workspace_id = "workspace")
  task <- qt("build_task")(ws$project, "back", list("S0001"))
  client <- capture_client(list(content = list(list(type = "thinking", thinking = "ignore"), list(type = "text", text = '{"segments":[]}')),
                                model = "returned-model", stop_reason = "end_turn"))
  out <- client$complete(task, "dummy")
  expect_null(client$payloads[[1]]$temperature)
  expect_false(grepl("Original source", qt("json_str")(client$payloads[[1]]), fixed = TRUE))
  expect_identical(out$provenance$returned_model, "returned-model")
  expect_identical(out$raw, '{"segments":[]}')
})

test_that("a truncated provider response is never accepted", {
  ws <- qt("new_workspace")(fake_client(), scripted_copilot())
  qt("ws_load_text")(ws, "P: Text")
  ws$project$settings$stages$forward <- list(provider = "mistral", model = "test", workspace_id = "")
  client <- capture_client(list(choices = list(list(finish_reason = "length", message = list(content = "partial")))))
  expect_error(client$complete(qt("build_task")(ws$project, "forward", list("S0001")), "dummy"), "truncated", class = "qt_workflow_error")
})

test_that("provider HTTP errors are reported without the body and retried only for 429/5xx", {
  calls <- 0L
  server <- httpuv::startServer("127.0.0.1", port <- httpuv::randomPort(), list(call = function(req) {
    calls <<- calls + 1L
    status <- if (req$PATH_INFO == "/retry" && calls < 3) 503L else if (req$PATH_INFO == "/retry") 200L else 401L
    list(status = status, headers = list("Content-Type" = "application/json", "Retry-After" = "1", "request-id" = "req-7"),
         body = if (status == 200L) '{"data":[{"id":"b"},{"id":"a"},{"id":"a"}]}' else '{"error":"SECRET_ECHO"}')
  }))
  on.exit(httpuv::stopServer(server))
  client <- qt("new_api_client")(.base = sprintf("http://127.0.0.1:%d", port))
  res <- client$request("mistral", "k", "", "/retry")
  expect_identical(calls, 3L)
  expect_identical(res$request_id, "req-7")
  expect_identical(client$model_ids(res$result), list("a", "b"))
  err <- tryCatch(client$request("mistral", "k", "", "/denied"), error = function(e) e)
  expect_identical(conditionMessage(err), "mistral HTTP 401. Check the API key.")
  expect_false(grepl("SECRET_ECHO", conditionMessage(err), fixed = TRUE))
  expect_error(client$request("mistral", "", "", "/x"), "Add the provider API key", class = "qt_workflow_error")
})

# ----- HTTP level -----------------------------------------------------------------------------

test_that("the HTTP server enforces its security rules and serves the demo", {
  ws <- qt("new_workspace")(fake_client(), scripted_copilot())
  srv <- start_test_server(ws); on.exit(httpuv::stopServer(srv$server))
  root <- http_request(srv$url, loop_ws = ws)
  expect_identical(root$status, 200L)
  expect_match(root$text, srv$token, fixed = TRUE)
  expect_identical(root$headers[["cache-control"]], "no-store")
  expect_match(root$headers[["content-security-policy"]], "frame-ancestors 'none'", fixed = TRUE)
  expect_identical(root$headers[["x-frame-options"]], "DENY")
  expect_identical(http_request(paste0(srv$url, "/api/state"), loop_ws = ws)$status, 404L)
  evil <- api(srv, "demo", qt("empty_obj")(), extra = list(Origin = "https://evil.example"))
  expect_identical(evil$status, 403L)
  expect_identical(http_request(srv$url, headers = list(Host = "evil.example"), loop_ws = ws)$status, 403L)
  expect_identical(http_request(paste0(srv$url, "/assets/../core.py"), loop_ws = ws)$status, 404L)
  expect_identical(http_request(paste0(srv$url, "/assets/app.js"), loop_ws = ws)$status, 200L)
  expect_identical(http_request(paste0(srv$url, "/health"), loop_ws = ws)$json$version, "1.5.0")
  expect_identical(http_request(paste0(srv$url, "/api/demo"), method = "OPTIONS", loop_ws = ws)$status, 403L)
  # Content type, size and format checks.
  wrong_type <- api(srv, "demo", qt("empty_obj")(), extra = list("Content-Type" = "text/plain"))
  expect_identical(wrong_type$status, 400L)
  expect_identical(wrong_type$json$error, "Requests must use application/json.")
  not_object <- api(srv, "demo", list(1, 2))
  expect_identical(not_object$json$error, "Request must be a JSON object.")
  bad_json <- api(srv, "demo", charToRaw("{nope"))
  expect_identical(bad_json$json$error, "Invalid request format.")
  # The server answers 413 and may close the connection before the client has finished
  # sending 25 MB; a reset connection is an equally valid refusal.
  big <- tryCatch(api(srv, "import", charToRaw(strrep(" ", 25000001))),
                  error = function(e) if (grepl("reset|closed|broken pipe|send failure", conditionMessage(e), ignore.case = TRUE)) list(status = 413L) else stop(e))
  expect_identical(big$status, 413L)
  expect_identical(api(srv, "state")$status, 200L)
  expect_identical(api(srv, "nothing-here", qt("empty_obj")())$status, 404L)
  # Demo and state.
  ok <- api(srv, "demo", qt("empty_obj")())
  expect_identical(ok$json, list(ok = TRUE))
  state <- api(srv, "state")
  expect_length(state$json$project$segments, 6)
  expect_null(state$json$project$runs)
  expect_identical(state$json$project$run_count, 0L)
  expect_null(state$json$keys)
  expect_identical(names(state$json$credentials), c("mistral", "anthropic"))
})

test_that("the job runs in the main loop while state polls and Stop keep working", {
  slow <- fake_client()
  plain <- slow$complete
  slow$complete <- function(task, key) {
    qt("qt_wait")(0.6)       # an API request that takes a while
    plain(task, key)
  }
  ws <- qt("new_workspace")(slow, scripted_copilot())
  qt("ws_load_text")(ws, paste(sprintf("P: fictional line %d", 1:6), collapse = "\n"), "Fixture")
  api_config(ws, batch_segments = 1L)
  srv <- start_test_server(ws); on.exit(httpuv::stopServer(srv$server))
  expect_identical(api(srv, "run", list(stage = "forward"))$status, 200L)
  # Pump only (loop = FALSE): the queued job must not start inside this helper.
  expect_identical(api(srv, "settings", list(settings = ws$project$settings), loop = FALSE)$json$error,
                   "A translation job is running. Stop it and wait for it to finish before editing.")
  # Requests sent while the job runs: they are answered from inside the job's waits.
  seen <- list()
  fire <- function(path, body = NULL) {
    h <- curl::new_handle()
    curl::handle_setopt(h, noproxy = "*", timeout = 10)
    curl::handle_setheaders(h, "X-QualiTranslate-Token" = srv$token, "Content-Type" = "application/json")
    if (!is.null(body)) curl::handle_setopt(h, post = 1L, copypostfields = charToRaw(qt("json_str")(body)))
    curl::curl_fetch_multi(paste0(srv$url, "/api/", path), pool = qt("qt_pool")(), handle = h,
                           done = function(r) seen[[length(seen) + 1L]] <<- list(path = path, at = ws$job$done, running = ws$job$running,
                                                                                 json = qt("json_parse")(rawToChar(r$content))),
                           fail = function(m) seen[[length(seen) + 1L]] <<- list(path = path, error = m))
  }
  later::later(function() fire("state"), 0.3)
  later::later(function() fire("state"), 0.9)
  later::later(function() fire("stop", qt("empty_obj")()), 1.5)
  started <- Sys.time()
  qt("qt_loop_once")(ws)       # runs the whole job (it was queued by /api/run)
  expect_false(ws$job$running)
  qt("qt_wait")(0.2)
  states <- Filter(function(s) identical(s$path, "state"), seen)
  expect_length(states, 2)
  expect_true(all(vapply(states, function(s) isTRUE(s$json$job$running) && grepl("Forward translation", s$json$job$message), logical(1))))
  expect_match(ws$job$error, "Stopped", fixed = TRUE)
  expect_gt(ws$job$done, 0L)
  expect_lt(ws$job$done, 6L)
  expect_lt(as.numeric(difftime(Sys.time(), started, units = "secs")), 4)
})

test_that("/api/models answers asynchronously and reports provider errors without their body", {
  fixture <- httpuv::startServer("127.0.0.1", port <- httpuv::randomPort(), list(call = function(req) {
    if (!identical(req$HTTP_AUTHORIZATION, "Bearer good")) {
      return(list(status = 401L, headers = list("Content-Type" = "application/json"), body = '{"error":"SECRET_ECHO"}'))
    }
    list(status = 200L, headers = list("Content-Type" = "application/json"), body = '{"data":[{"id":"m-b"},{"id":"m-a"},{"x":1}]}')
  }))
  on.exit(httpuv::stopServer(fixture))
  ws <- qt("new_workspace")(qt("new_api_client")(.base = sprintf("http://127.0.0.1:%d", port)), scripted_copilot())
  srv <- start_test_server(ws); on.exit(httpuv::stopServer(srv$server), add = TRUE)
  no_key <- api(srv, "models", list(provider = "mistral"))
  expect_identical(no_key$status, 400L)
  expect_identical(no_key$json$error, "Add the provider API key in Settings first.")
  expect_identical(api(srv, "key", list(provider = "mistral", key = "  good  "))$status, 200L)
  ok <- api(srv, "models", list(provider = "mistral"))
  expect_identical(ok$status, 200L)
  expect_identical(ok$json$models, list("m-a", "m-b"))
  expect_true(api(srv, "state")$json$credentials$mistral)
  ws$keys$anthropic <- "bad"
  bad <- api(srv, "models", list(provider = "anthropic", workspace_id = "w"))
  expect_identical(bad$status, 400L)
  expect_identical(bad$json$error, "anthropic HTTP 401. Check the API key.")
})

test_that("review fixes: null characters, other methods, upgrades, empty model lists", {
  expect_error(qt("json_parse")('{"text":"Hello\\u0000 hidden"}'), "null character")
  expect_identical(qt("json_parse")('{"text":"a\\\\u0000b"}')$text, "a\\u0000b")
  ids <- qt("new_api_client")()$model_ids(list(data = list()))
  expect_identical(ids, list())
})
