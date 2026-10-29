# Local HTTP server: the browser interface and its JSON API (same routes, status codes,
# error texts and security checks as the Python version's app.py).

MAX_REQUEST <- 25000000
CSP <- "default-src 'self'; script-src 'self'; style-src 'self'; img-src 'self' data:; connect-src 'self'; frame-ancestors 'none'; base-uri 'none'; form-action 'self'"
INTERNAL_ERROR <- "An internal error occurred. Completed work remains in memory; export your project before restarting."

qt_response <- function(data, content_type = "application/json; charset=utf-8", status = 200L, filename = NULL) {
  body <- if (is.raw(data)) data else charToRaw(json_str(data))
  headers <- list("Content-Type" = content_type, "Cache-Control" = "no-store", "X-Content-Type-Options" = "nosniff",
                  "Referrer-Policy" = "no-referrer", "X-Frame-Options" = "DENY", "Content-Security-Policy" = CSP)
  if (!is.null(filename)) headers[["Content-Disposition"]] <- paste0("attachment; filename=\"", filename, "\"")
  list(status = as.integer(status), headers = headers, body = body)
}

error_response <- function(e) {
  if (is_qt_message_error(e)) return(qt_response(list(error = conditionMessage(e)), status = 400L))
  if (inherits(e, "qt_format_error")) return(qt_response(list(error = "Invalid request format."), status = 400L))
  qt_response(list(error = INTERNAL_ERROR), status = 500L)
}

media_type <- function(value) {
  # email.message.get_content_type(): lower-cased type/subtype, text/plain if absent or malformed.
  if (!is_str(value)) return("text/plain")
  mt <- tolower(py_strip(sub(";.*$", "", value)))
  if (!grepl("^[^/]+/[^/]+$", mt)) "text/plain" else mt
}

state_payload <- function(ws) {
  state <- ws_snapshot(ws)
  # Exact prompt history is exported separately, not resent on each UI poll.
  runs <- state$project$runs
  state$project$runs <- NULL
  state$project$run_count <- length(runs)
  label <- ""
  for (r in rev(runs)) {
    p <- oget(r, "provenance", empty_obj())
    if (identical(oget(p, "provider"), "manual")) {
      label <- oget(p, "tool_model_reported_by_user", "")
      break
    }
  }
  state$project <- jset(state$project, "last_manual_label", label)
  audit_count <- length(state$project$audit)
  state$project$audit <- NULL
  state$project$audit_count <- audit_count
  state
}

qt_server_app <- function(ws, token, port) {
  allowed_hosts <- paste0(c("127.0.0.1:", "localhost:"), port)
  allowed_origins <- paste0("http://", allowed_hosts)
  host_ok <- function(req) is_str(req$HTTP_HOST) && req$HTTP_HOST %in% allowed_hosts
  authorized <- function(req) {
    origin <- req$HTTP_ORIGIN
    host_ok(req) && (is.null(origin) || isTRUE(origin %in% allowed_origins)) &&
      constant_time_equal(req$HTTP_X_QUALITRANSLATE_TOKEN %||% "", token)
  }
  rejected <- function() qt_response(list(error = "Request rejected. Open the app from its local address."), status = 403L)

  handle_get <- function(req, path) {
    if (!host_ok(req)) return(qt_response(list(error = "Host rejected."), status = 403L))
    if (path %in% c("/", "/index.html")) {
      page <- gsub("__CSRF_TOKEN__", token, asset_text("www", "index.html"), fixed = TRUE)
      return(qt_response(charToRaw(enc2utf8(page)), "text/html; charset=utf-8"))
    }
    if (path %in% c("/assets/app.js", "/assets/style.css")) {
      filename <- sub("^/assets/", "", path)
      mime <- if (endsWith(filename, ".js")) "text/javascript; charset=utf-8" else "text/css; charset=utf-8"
      file <- asset_path("www", filename)
      return(qt_response(readBin(file, "raw", file.size(file)), mime))
    }
    if (path == "/api/state" && authorized(req)) return(qt_response(state_payload(ws)))
    if (path == "/health") return(qt_response(list(app = "QualiTranslate", version = APP_VERSION)))
    qt_response(list(error = "Not found or access denied."), status = 404L)
  }

  # Returns a response, a promise of a response, or NULL for {"ok": true}.
  route_post <- function(route, body) {
    get <- function(key, default = NULL) jget(body, key, default)
    switch(route,
      "/api/import" = ws_load_text(ws, get("text", ""), get("title", "Untitled interview"),
                                   split_speakers = is_true(get("split_speakers", TRUE))),
      "/api/file" = return(qt_response(read_document(get("filename", ""), get("data", "")))),
      "/api/inspect" = return(qt_response(ws_inspect_document(ws, get("text", ""), get("blocks")))),
      "/api/import-selected" = ws_import_selected(ws, get("inspection_id"), get("included_ids"), get("title", "Untitled interview"),
                                                  get("settings"), is_true(get("split_speakers", TRUE))),
      "/api/demo" = ws_load_text(ws, DEMO_SOURCE, "Synthetic sample \u00b7 treatment success", demo = TRUE),
      "/api/settings" = ws_save_settings(ws, get("settings")),
      "/api/key" = ws_set_key(ws, get("provider"), get("key", "")),
      "/api/copilot/open" = , "/api/copilot/check" = {
        ws_idle(ws)
        return(qt_response(ws$copilot_connection$start()))
      },
      "/api/copilot/cancel" = {
        ws_idle(ws)
        return(qt_response(ws$copilot_connection$cancel()))
      },
      "/api/copilot/close" = , "/api/copilot/forget" = {
        ws_idle(ws)
        ws$copilot_connection$cancel()
        forget <- route == "/api/copilot/forget"
        # Closing waits for the browser, so it runs in the main loop; the reply follows then.
        return(promises::promise(function(resolve, reject) {
          ws$deferred[[length(ws$deferred) + 1L]] <- function() {
            resolve(tryCatch({
              ws$copilot$close(forget = forget)
              ws$copilot_connection$phase <- "idle"
              qt_response(ws$copilot_connection$summary())
            }, error = error_response))
          }
        }))
      },
      "/api/models" = {
        provider <- get("provider")
        key <- if (is_str(provider) && provider %in% names(ws$keys)) ws$keys[[provider]] else ""
        p <- ws$client$models_async(provider, key, get("workspace_id", ""))
        return(promises::then(p, onFulfilled = function(ids) qt_response(list(models = ids)), onRejected = error_response))
      },
      "/api/prompt" = {
        task <- ws_prepare_task(ws, get("stage"))
        task$copy_text <- paste0(task$system, "\n\nINPUT JSON:\n", task$user)
        return(qt_response(task))
      },
      "/api/manual" = ws_apply_manual(ws, get("task_id"), get("response", ""), get("model_label", "")),
      "/api/run" = ws_start_job(ws, get("stage"), demo = is_true(get("demo")),
                                expected_project_id = get("expected_project_id"), expected_revision = get("expected_revision")),
      "/api/methods" = return(qt_response(list(paragraph = methods_paragraph(ws_snapshot(ws)$project)))),
      "/api/stop" = ws$cancel$set(),
      "/api/edit" = ws_edit(ws, get("id"), get("stage"), get("text", ""), get("note", "")),
      "/api/review" = ws_review(ws, get("id"), get("text", ""), get("reviewer", ""), get("note", ""),
                                is_true(get("uncertainty")), get("method", "individual")),
      "/api/review-batch" = ws_review_batch_minor(ws, get("ids"), get("reviewer", ""), get("revision")),
      "/api/review-clear" = ws_clear_review(ws, get("id")),
      "/api/project" = ws_import_project(ws, get("project")),
      "/api/reset" = ws_clear(ws),
      "/api/export" = {
        project <- ws_snapshot(ws)$project
        kind <- get("kind", "zip")
        if (identical(kind, "json")) {
          return(qt_response(charToRaw(json_pretty(project)), "application/json", filename = "qualitranslate-project.json"))
        } else if (identical(kind, "csv")) {
          return(qt_response(export_csv(project), "text/csv; charset=utf-8", filename = "qualitranslate-translations.csv"))
        } else if (identical(kind, "html")) {
          return(qt_response(export_html(project), "text/html; charset=utf-8", filename = "qualitranslate-parallel-review.html"))
        } else if (identical(kind, "zip")) {
          return(qt_response(export_bundle(project), "application/zip", filename = "qualitranslate-research-bundle.zip"))
        }
        workflow_error("Unknown export format.")
      },
      return(qt_response(list(error = "Unknown endpoint."), status = 404L)))
    NULL
  }

  handle_post <- function(req, path) {
    if (!authorized(req)) return(rejected())
    tryCatch({
      length <- suppressWarnings(as.numeric(req$HTTP_CONTENT_LENGTH %||% "0"))
      if (is.na(length)) format_error()
      if (!(length > 0 && length <= MAX_REQUEST)) {
        return(qt_response(list(error = "Request is empty or exceeds 25 MB."), status = 413L))
      }
      if (media_type(req$HTTP_CONTENT_TYPE) != "application/json") workflow_error("Requests must use application/json.")
      raw <- req$rook.input$read()
      body <- tryCatch({
        text <- rawToChar(raw)
        Encoding(text) <- "UTF-8"
        if (!validUTF8(text)) stop("invalid UTF-8")
        json_parse(text)
      }, error = function(e) format_error())
      if (!is_obj(body)) workflow_error("Request must be a JSON object.")
      result <- route_post(path, body)
      if (is.null(result)) qt_response(list(ok = TRUE)) else result
    }, error = error_response)
  }

  list(
    # Reject unauthorised or oversized uploads before their body is read.
    onHeaders = function(req) {
      method <- req$REQUEST_METHOD
      if (!host_ok(req)) return(qt_response(list(error = "Host rejected."), status = 403L))
      # No WebSocket endpoint exists; refuse upgrades instead of accepting and failing.
      if (!is.null(req$HTTP_UPGRADE)) return(qt_response(list(error = "Not found or access denied."), status = 403L))
      if (!isTRUE(method %in% c("GET", "POST", "OPTIONS"))) return(qt_response(list(error = "Unsupported method."), status = 501L))
      if (!identical(method, "POST")) {
        length <- suppressWarnings(as.numeric(req$HTTP_CONTENT_LENGTH %||% "0"))
        if (is.na(length) || length > 0 || !is.null(req$HTTP_TRANSFER_ENCODING)) return(qt_response(list(error = "Request body not allowed."), status = 400L))
        return(NULL)
      }
      if (!authorized(req)) return(rejected())
      length <- suppressWarnings(as.numeric(req$HTTP_CONTENT_LENGTH %||% "0"))
      if (!is.na(length) && !(length > 0 && length <= MAX_REQUEST)) {
        return(qt_response(list(error = "Request is empty or exceeds 25 MB."), status = 413L))
      }
      NULL
    },
    # Upgrades are refused in onHeaders; if httpuv still opens a socket, close it quietly.
    onWSOpen = function(ws) try(ws$close(), silent = TRUE),
    call = function(req) {
      .qt$in_handler <- TRUE
      on.exit(.qt$in_handler <- FALSE, add = TRUE)
      path <- paste0(req$PATH_INFO %||% "", req$QUERY_STRING %||% "")
      method <- req$REQUEST_METHOD
      tryCatch({
        if (identical(method, "GET")) return(handle_get(req, path))
        if (identical(method, "POST")) return(handle_post(req, path))
        if (identical(method, "OPTIONS")) return(qt_response(list(error = "Cross-origin access is disabled."), status = 403L))
        qt_response(list(error = "Unsupported method."), status = 501L)
      }, error = error_response)
    })
}

qt_start_server <- function(ws, token, port) {
  httpuv::startServer("127.0.0.1", as.integer(port), qt_server_app(ws, token, as.integer(port)))
}

# Return the version of a QualiTranslate already serving this port, if any.
running_version <- function(port) {
  tryCatch({
    h <- curl::new_handle()
    curl::handle_setopt(h, timeout_ms = 1500L, connecttimeout_ms = 1500L, noproxy = "*")
    res <- curl::curl_fetch_memory(paste0("http://127.0.0.1:", port, "/health"), handle = h)
    data <- json_parse(raw_to_utf8(utils::head(res$content, 2000)))
    if (identical(data$app, "QualiTranslate")) as.character(data$version %||% "unknown") else ""
  }, error = function(e) "")
}
