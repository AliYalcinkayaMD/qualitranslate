# API client for the Mistral and Anthropic (Claude) APIs.
# Only fixed HTTPS provider endpoints; no credentials in exports or request logs.

API_HINTS <- c(
  "401" = "Check the API key.",
  "403" = "Check model/workspace permissions.",
  "404" = "The model or resource was not found. Load the available models and choose its exact id.",
  "429" = "Rate or spending limit reached. Wait before resuming.",
  "400" = "The provider rejected these settings. Check the model id, workspace and token budget. Some models do not accept temperature, top-p or reasoning effort: set them back to Provider default in Settings and try again.")

# `.base` is an R-only test hook that points both providers at a local fixture server.
new_api_client <- function(.base = NULL) {
  self <- new.env(parent = emptyenv())

  # Starts the request and calls on_done(list(result, request_id)) or on_error(condition).
  # Retries only HTTP 429/502/503/504, at most twice, after the provider's Retry-After delay.
  # A timeout or connection failure is never retried automatically: the provider may still
  # have processed (and charged for) the request.
  self$request_async <- function(provider, key, workspace_id, path, payload, on_done, on_error) {
    if (!is_str(provider) || !(provider %in% c("mistral", "anthropic")) || !is_str(key) || !nzchar(key)) {
      workflow_error("Add the provider API key in Settings first.")
    }
    base <- .base %||% (if (provider == "mistral") "https://api.mistral.ai" else "https://api.anthropic.com")
    headers <- c("Content-Type" = "application/json", "Accept" = "application/json",
                 "Authorization" = paste("Bearer", key))
    if (provider == "anthropic") {
      headers <- c(headers, "anthropic-version" = "2023-06-01")
      if (is_str(workspace_id) && nzchar(workspace_id)) headers <- c(headers, "anthropic-workspace-id" = workspace_id)
    }
    body <- if (!is.null(payload)) charToRaw(json_str(payload)) else NULL
    attempt <- 0L
    submit <- function() {
      h <- curl::new_handle()
      curl::handle_setopt(h, followlocation = 0L, connecttimeout = 120L, low_speed_time = 120L,
                          low_speed_limit = 1L, accept_encoding = "")
      if (!is.null(.base)) curl::handle_setopt(h, noproxy = "*")
      curl::handle_setheaders(h, .list = as.list(headers))
      if (!is.null(body)) curl::handle_setopt(h, post = 1L, copypostfields = body)
      qt_fetch_async(paste0(base, path), h, done = handle_done, fail = function(msg) {
        on_error(qt_condition("qt_workflow_error", "Provider connection failed or timed out. No automatic timeout retry was made; check results before retrying."))
      })
    }
    handle_done <- function(res) {
      code <- res$status_code
      if (code >= 200 && code < 300) {
        if (length(res$content) > 6000000) {
          return(on_error(qt_condition("qt_workflow_error", "Provider response exceeds the size limit.")))
        }
        parsed <- tryCatch(json_parse(raw_to_utf8(res$content)), error = function(e) e)
        if (inherits(parsed, "error")) {
          return(on_error(qt_condition("qt_workflow_error", "Provider did not return valid JSON.")))
        }
        hdrs <- curl::parse_headers_list(res$headers)
        rid <- hdrs[["request-id"]] %||% ""
        return(on_done(list(result = parsed, request_id = rid)))
      }
      if (code %in% c(429, 502, 503, 504) && attempt < 2L) {
        hdrs <- curl::parse_headers_list(res$headers)
        retry <- hdrs[["retry-after"]]
        delay <- if (is.null(retry)) 2^attempt else suppressWarnings(as.numeric(retry))
        delay <- if (is.na(delay)) 2 else min(10, max(1, delay))
        attempt <<- attempt + 1L
        later::later(submit, delay)
        return(invisible())
      }
      hint <- if (as.character(code) %in% names(API_HINTS)) API_HINTS[[as.character(code)]] else "The provider request failed."
      # Provider error bodies can echo content or credentials; do not display or log them.
      on_error(qt_condition("qt_workflow_error", paste0(provider, " HTTP ", code, ". ", hint)))
    }
    submit()
    invisible()
  }

  # Cooperative (pumping) version used by the job worker.
  self$request <- function(provider, key, workspace_id, path, payload = NULL) {
    outcome <- NULL
    self$request_async(provider, key, workspace_id, path, payload,
                       on_done = function(value) outcome <<- list(ok = TRUE, value = value),
                       on_error = function(cond) outcome <<- list(ok = FALSE, cond = cond))
    while (is.null(outcome)) qt_wait(0.01)
    if (!outcome$ok) stop(outcome$cond)
    outcome$value
  }

  self$model_ids <- function(result) {
    models <- if (is_obj(result)) oget(result, "data", list()) else format_error()
    ids <- unlist(lapply(models, function(m) if (is_obj(m) && is_str(m[["id"]])) m[["id"]] else NULL))
    if (is.null(ids)) return(list())
    as.list(sort(unique(ids), method = "radix"))
  }

  # Returns a promise of the sorted model ids (used by the /api/models request handler,
  # which must not wait).
  self$models_async <- function(provider, key, workspace_id = "") {
    promises::promise(function(resolve, reject) {
      self$request_async(provider, key, workspace_id, "/v1/models", NULL,
                         on_done = function(value) {
                           ids <- tryCatch(self$model_ids(value$result), error = function(e) e)
                           if (inherits(ids, "error")) reject(ids) else resolve(ids)
                         },
                         on_error = reject)
    })
  }

  self$models <- function(provider, key, workspace_id = "") {
    self$model_ids(self$request(provider, key, workspace_id, "/v1/models")$result)
  }

  self$complete <- function(task, key) {
    config <- task$configuration
    provider <- config$provider; model <- py_strip(config$model)
    if (!nzchar(model)) workflow_error("Choose an exact model id in Settings before running the API.")
    if (provider == "mistral") {
      payload <- list(model = model,
                      messages = list(list(role = "system", content = task$system), list(role = "user", content = task$user)),
                      response_format = list(type = "json_object"), max_tokens = task$max_tokens, stream = FALSE)
      for (option in c("temperature", "top_p", "random_seed")) {
        if (!is.null(config[[option]])) payload[[option]] <- config[[option]]
      }
      if (truthy(config$effort)) payload$reasoning_effort <- config$effort
      response <- self$request(provider, key, "", "/v1/chat/completions", payload)
      result <- response$result
      if (!is_obj(result)) format_error()
      choices <- oget(result, "choices", list())
      if (!length(choices)) workflow_error("Provider returned no completion.")
      first <- choices[[1]]
      if (identical(oget(first, "finish_reason"), "length")) {
        workflow_error("Output was truncated. Increase the token budget or reduce batch size, then resume.")
      }
      content <- oget(oget(first, "message", empty_obj()), "content", "")
    } else if (provider == "anthropic") {
      payload <- list(model = model, system = task$system,
                      messages = list(list(role = "user", content = task$user)), max_tokens = task$max_tokens)
      # Sampling values are sent only when the user sets them: several current Claude
      # models reject temperature/top-p, and older ones do not support effort.
      for (option in c("temperature", "top_p")) {
        if (!is.null(config[[option]])) payload[[option]] <- config[[option]]
      }
      if (truthy(config$effort)) payload$output_config <- list(effort = config$effort)
      response <- self$request(provider, key, config$workspace_id %||% "", "/v1/messages", payload)
      result <- response$result
      if (!is_obj(result)) format_error()
      stop_reason <- oget(result, "stop_reason")
      if (is_str(stop_reason) && stop_reason %in% c("max_tokens", "pause_turn", "refusal")) {
        workflow_error(paste0("Provider did not finish a usable translation (", stop_reason,
                              "). Reduce batch size or change model."))
      }
      content <- oget(result, "content", list())
    } else {
      workflow_error("Manual stages use the copy/paste workflow.")
    }
    if (is_arr(content)) {
      texts <- vapply(content, function(x) {
        if (is_obj(x) && identical(x[["type"]], "text") && is_str(x[["text"]] %||% "")) x[["text"]] %||% "" else ""
      }, character(1))
      content <- paste(texts, collapse = "")
    }
    if (!is_str(content) || !nzchar(py_strip(content))) {
      workflow_error("Provider returned no text. Try a larger output budget or a different model.")
    }
    rid <- response$request_id
    list(raw = content,
         provenance = list(provider = provider, requested_model = model,
                           returned_model = oget(result, "model", model),
                           request_id = if (nzchar(rid)) rid else oget(result, "id", ""),
                           usage = oget(result, "usage", empty_obj()), completed_at = now_iso()))
  }
  self
}
