# Shared test helpers. Only fictional text is used, and nothing is written outside tempdir().

qt <- function(name) get(name, envir = asNamespace("qualitranslate"))

# Fake API client: answers every request locally (fixture text only).
fake_client <- function() {
  self <- new.env(parent = emptyenv())
  self$requests <- list()
  self$fail_once <- FALSE
  self$complete <- function(task, key) {
    self$requests[[length(self$requests) + 1L]] <- task
    if (self$fail_once && length(self$requests) == 2L) {
      self$fail_once <- FALSE
      qt("workflow_error")("Simulated provider failure")
    }
    data <- qt("json_parse")(task$user)
    rows <- lapply(data$segments, function(s) {
      if (task$stage == "reconcile") list(id = s$id, text = s$forward_translation, issues = list(), rationale = "Checked in fixture", needs_human = FALSE)
      else list(id = s$id, text = paste("fixture", s$text), notes = list())
    })
    list(raw = qt("json_str")(list(segments = rows)),
         provenance = list(provider = "fake", returned_model = "test-model", usage = list(input_tokens = 10L)))
  }
  self$models_async <- function(provider, key, workspace_id = "") {
    promises::promise(function(resolve, reject) resolve(list("model-a")))
  }
  self
}

# Scripted stand-in for the Copilot browser transport: declines any packet containing a marker.
scripted_copilot <- function(marker = "BLOCKED_WORD", on_complete = NULL) {
  self <- new.env(parent = emptyenv())
  self$marker <- marker; self$requests <- list(); self$temporary_values <- logical(); self$on_complete <- on_complete
  self$temporary_chat <- TRUE; self$ready <- TRUE; self$opened <- TRUE
  self$summary <- function() list(open = self$opened, ready = self$ready, experimental = TRUE, message = if (self$ready) "ready" else "Sign in and check connection.",
                                  step = "idle", step_detail = "")
  self$open <- function() { self$opened <- TRUE; self$summary() }
  self$check <- function() self$summary()
  self$close <- function(forget = FALSE) { self$ready <- FALSE; self$opened <- FALSE }
  self$complete <- function(box, cancel = NULL) {
    task <- box$task
    self$requests[[length(self$requests) + 1L]] <- task
    self$temporary_values <- c(self$temporary_values, self$temporary_chat)
    if (!is.null(self$on_complete)) self$on_complete(task)
    packet <- paste0(task$system, task$user)
    if (grepl(self$marker, packet, fixed = TRUE)) {
      box$task$declined_reply <- "Sorry, it looks like I can't respond to this."
      qt("browser_error")("Copilot declined or could not answer this batch.", "declined")
    }
    box$task$transport_prompt <- paste0("VISIBLE_UI_PACKET\n", task$system, "\n", task$user)
    payload <- qt("json_parse")(task$user)
    rows <- lapply(payload$segments, function(row) {
      if (task$stage == "reconcile") list(id = row$id, text = paste("T", row$id), issues = list(), rationale = "ok", needs_human = TRUE)
      else list(id = row$id, text = paste("T", row$id), notes = list())
    })
    list(raw = qt("json_str")(list(qt_receipt = "r", segments = rows)),
         provenance = list(provider = "copilot_browser", model = "not reported by Copilot Chat",
                           chat_isolation = "new_temporary_chat_verified", receipt_verified = TRUE,
                           copilot_model_selector_label = "Auto", temporary_chat_notice = "notice", browser = "Chrome/x", submitted_at = "t"))
  }
  self
}

# Run a queued job (the app's main loop does this).
run_job <- function(ws) {
  expect_true(ws$job$running)
  qt("ws_run_job")(ws)
  expect_false(ws$job$running)
  invisible(ws)
}

start_and_run <- function(ws, stage = "all", demo = FALSE, ...) {
  qt("ws_start_job")(ws, stage, demo = demo, ...)
  run_job(ws)
}

copilot_workspace <- function(copilot, text) {
  ws <- qt("new_workspace")(fake_client(), copilot)
  qt("ws_load_text")(ws, text, "Fixture")
  settings <- qt("settings_defaults")()
  for (st in names(settings$stages)) { settings$stages[[st]]$provider <- "copilot"; settings$stages[[st]]$chat_tool <- "Institutional Copilot" }
  qt("ws_save_settings")(ws, settings)
  ws$copilot_connection$phase <- "ready"
  ws
}

api_settings <- function(ws, provider = "mistral", model = "fixture-model") {
  settings <- ws$project$settings
  for (st in names(settings$stages)) { settings$stages[[st]]$provider <- provider; settings$stages[[st]]$model <- model }
  qt("ws_save_settings")(ws, settings)
  ws$keys[[provider]] <- "NEVER_EXPORT_THIS_KEY"
  invisible(ws)
}

manual_answer <- function(ws, stage, make_text) {
  task <- qt("ws_prepare_task")(ws, stage)
  rows <- lapply(unlist(task$ids), function(id) {
    row <- list(id = id, text = make_text(id))
    if (stage == "reconcile") c(row, list(issues = list(), rationale = "Fixture", needs_human = TRUE)) else c(row, list(notes = list()))
  })
  qt("ws_apply_manual")(ws, task$task_id, qt("json_str")(list(segments = rows)), "Manual QA")
}

# ----- HTTP helpers: requests are made asynchronously and the app's loop is pumped meanwhile.
start_test_server <- function(ws, token = "test-token-private") {
  port <- httpuv::randomPort()
  server <- qt("qt_start_server")(ws, token, port)
  list(server = server, port = port, url = paste0("http://127.0.0.1:", port), token = token, ws = ws)
}

http_request <- function(url, body = NULL, headers = list(), method = NULL, loop_ws = NULL, timeout = 20) {
  h <- curl::new_handle()
  curl::handle_setopt(h, noproxy = "*", timeout = timeout)
  if (!is.null(body)) {
    raw <- if (is.raw(body)) body else charToRaw(qt("json_str")(body))
    curl::handle_setopt(h, post = 1L, copypostfields = raw)
    if (is.null(headers[["Content-Type"]])) headers[["Content-Type"]] <- "application/json"
  }
  if (!is.null(method)) curl::handle_setopt(h, customrequest = method)
  if (length(headers)) curl::handle_setheaders(h, .list = headers)
  result <- NULL
  curl::curl_fetch_multi(url, done = function(r) result <<- r, fail = function(m) result <<- list(error = m),
                         pool = qt("qt_pool")(), handle = h)
  end <- Sys.time() + timeout
  while (is.null(result) && Sys.time() < end) {
    if (!is.null(loop_ws)) qt("qt_loop_once")(loop_ws, 0.01) else qt("qt_wait")(0.01)
  }
  if (is.null(result)) stop("request timed out")
  if (!is.null(result$error)) stop(result$error)
  text <- rawToChar(result$content)
  Encoding(text) <- "UTF-8"
  hdrs <- curl::parse_headers_list(result$headers)
  parsed <- tryCatch(qt("json_parse")(text), error = function(e) NULL)
  list(status = result$status_code, json = parsed, text = text, raw = result$content, headers = hdrs)
}

api <- function(srv, path, body = NULL, extra = list(), loop = TRUE) {
  headers <- c(list("X-QualiTranslate-Token" = srv$token), extra)
  http_request(paste0(srv$url, "/api/", path), body, headers, loop_ws = if (loop) srv$ws else NULL)
}

fixture_docx <- function(document_xml, path = tempfile(fileext = ".docx")) {
  dir <- tempfile("docx-")
  dir.create(file.path(dir, "word"), recursive = TRUE)
  writeBin(charToRaw(enc2utf8(document_xml)), file.path(dir, "word", "document.xml"))
  zip::zip(path, files = "word/document.xml", root = dir, mode = "mirror")
  unlink(dir, recursive = TRUE)
  b64 <- openssl::base64_encode(readBin(path, "raw", file.size(path)))
  unlink(path)
  b64
}

word_body <- function(body) {
  paste0('<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"><w:body>', body, "</w:body></w:document>")
}
