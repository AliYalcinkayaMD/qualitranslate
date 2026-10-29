# Settings, transcript parsing, response validation, batching and prompt construction.

stage_defaults <- function() {
  list(provider = "manual", model = "", workspace_id = "", temperature = NULL, top_p = NULL,
       random_seed = NULL, effort = "", chat_tool = "", chat_model = "")
}

settings_defaults <- function() {
  list(
    source_language = "Danish", target_language = "English",
    study_context = "",
    glossary = "", batch_chars = 5000L, batch_segments = 12L,
    max_tokens = 16384L, terminology = list(),
    reporting = list(translation_scope = "Selected interview transcript before analysis", dialect_context = "",
                     reviewer_background = "", decision_summary = ""),
    stages = list(forward = stage_defaults(), back = stage_defaults(), reconcile = stage_defaults()),
    prompt_additions = list(forward = "", back = "", reconcile = ""))
}

validate_settings <- function(value) {
  if (!is_obj(value)) workflow_error("Settings must be an object.")
  result <- settings_defaults()
  for (key in c("source_language", "target_language", "study_context", "glossary")) {
    long <- key %in% c("study_context", "glossary")
    result[[key]] <- text_value(jget(value, key, result[[key]]), key, if (long) 15000 else 120, allow_empty = long)
  }
  if (casefold_py(py_strip(result$source_language)) == casefold_py(py_strip(result$target_language))) {
    workflow_error("Choose different source and target languages.")
  }
  for (spec in list(list("batch_chars", 500, 12000), list("batch_segments", 1, 40), list("max_tokens", 1024, 32768))) {
    key <- spec[[1]]
    n <- jget(value, key, result[[key]])
    if (!is_int(n) || n < spec[[2]] || n > spec[[3]]) workflow_error(paste0(key, " is outside the allowed range."))
    result[[key]] <- n
  }
  stages_in <- jget(value, "stages", empty_obj())
  additions_in <- jget(value, "prompt_additions", empty_obj())
  for (stage in STAGES) {
    item <- jget(stages_in, stage, result$stages[[stage]])
    if (is_obj(item) && identical(item[["provider"]], "copilot")) {
      # Projects saved by pre-release versions may contain "copilot" stages; they become
      # copy-and-paste stages with the same chat tool.
      item$provider <- "manual"
      item$chat_tool <- if (truthy(item[["chat_tool"]])) item[["chat_tool"]] else "Microsoft Copilot"
      item$model <- ""; item$workspace_id <- ""
      for (name in c("temperature", "top_p", "random_seed")) item <- jset(item, name, NULL)
      item$effort <- ""
    }
    provider <- if (is_obj(item)) item[["provider"]] else NULL
    if (!is_obj(item) || !is_str(provider) || !(provider %in% PROVIDERS)) {
      workflow_error(paste0("Invalid provider for ", stage))
    }
    chat <- provider == "manual"
    config <- list(
      provider = provider,
      model = text_value(jget(item, "model", ""), "model", 200, TRUE),
      workspace_id = text_value(jget(item, "workspace_id", ""), "workspace id", 150, TRUE),
      chat_tool = if (chat) text_value(jget(item, "chat_tool", ""), "Chat tool", 150, TRUE) else "",
      chat_model = if (chat) text_value(jget(item, "chat_model", ""), "Chat model", 150, TRUE) else "")
    for (name in c("temperature", "top_p")) {
      n <- jget(item, name)
      if (!is.null(n) && (!is_num(n) || !is.finite(n) || n < 0 || n > 1 || (name == "top_p" && n == 0))) {
        workflow_error(paste0(name, " must be between 0 and 1 (top-p must be greater than 0)."))
      }
      config <- jset(config, name, n)
    }
    seed <- jget(item, "random_seed")
    if (!is.null(seed) && (!is_int(seed) || seed < 0)) {
      workflow_error("Random seed must be an integer from 0 to 2147483647.")
    }
    config <- jset(config, "random_seed", seed)
    if (!is.null(config$temperature) && !is.null(config$top_p)) {
      workflow_error("Choose temperature or top-p, rather than setting both.")
    }
    effort <- jget(item, "effort")
    if (!truthy(effort)) effort <- ""
    if (!identical(effort, "")) {
      allowed <- EFFORT_LEVELS[[provider]]
      if (!is_str(effort) || !(effort %in% allowed)) {
        workflow_error(paste0("Reasoning effort ", py_repr(effort), " is not available for ", stage, " with this provider.",
                              if (!is.null(allowed)) paste0(" Choose one of: ", paste(allowed, collapse = ", "), ".") else ""))
      }
    }
    config$effort <- effort
    if (provider == "anthropic" && !is.null(config$random_seed)) {
      workflow_error("The Claude API has no random seed setting. Leave the seed empty for Claude.")
    }
    result$stages[[stage]] <- config
    result$prompt_additions[[stage]] <- text_value(jget(additions_in, stage, ""), "prompt addition", 5000, TRUE)
  }
  terms <- jget(value, "terminology", list())
  if (!is_arr(terms) || length(terms) > 300) workflow_error("The terminology glossary supports up to 300 entries.")
  result$terminology <- list()
  for (t in terms) {
    if (!is_obj(t) || !is_bool(jget(t, "required", TRUE))) workflow_error("Invalid terminology entry.")
    alternatives <- jget(t, "alternatives", list())
    if (!is_arr(alternatives) || length(alternatives) > 10) workflow_error("Use up to ten accepted alternatives per term.")
    result$terminology[[length(result$terminology) + 1L]] <- list(
      source = text_value(jget(t, "source"), "Source term", 300),
      target = text_value(jget(t, "target"), "Preferred translation", 300),
      alternatives = lapply(alternatives, function(a) text_value(a, "Alternative term", 300)),
      notes = text_value(jget(t, "notes", ""), "Terminology notes", 1500, TRUE),
      required = jget(t, "required", TRUE))
  }
  reporting <- jget(value, "reporting", empty_obj())
  if (!is_obj(reporting)) workflow_error("Invalid reporting information.")
  for (k in names(result$reporting)) {
    result$reporting[[k]] <- text_value(jget(reporting, k, result$reporting[[k]]), k, 10000, TRUE)
  }
  # Custom blind-back instructions are deliberately disallowed: they could leak source context.
  if (nzchar(py_strip(result$prompt_additions$back))) {
    workflow_error("Blind back-translation does not accept custom source context or instructions.")
  }
  result
}

new_segment <- function(id, speaker, source) {
  list(id = id, speaker = speaker, source = source, forward = NULL, back = NULL, reconcile = NULL, review = NULL)
}

parse_transcript <- function(text, split_speakers = TRUE) {
  text_value(text, "Transcript")
  text <- gsub("\r\n", "\n", text, fixed = TRUE)
  text <- gsub("\r", "\n", text, fixed = TRUE)
  text <- sub("^\ufeff+", "", text, perl = TRUE)
  lines <- py_split(text, "\n")
  matches <- if (split_speakers) {
    regmatches(lines, regexec(paste0("(*UCP)^", WS, "*([^:\\n]{1,60}):[ \\t]*(.*)$"), lines, perl = TRUE))
  } else NULL
  blank <- !nzchar(py_strip(lines))
  result <- list(); speaker <- ""; collected <- character()
  flush <- function() {
    body <- py_strip(paste(collected, collapse = "\n"))
    if (nzchar(body)) {
      if (nchar_py(body) > 24000) {
        workflow_error("A single turn exceeds 24,000 characters. Add paragraph breaks before importing.")
      }
      result[[length(result) + 1L]] <<- new_segment(sprintf("S%04d", length(result) + 1L), speaker, body)
    }
    collected <<- character()
  }
  # A line starting with a short label followed by a colon starts a new turn.
  # Speaker labels are metadata, never regenerated by the language model.
  for (i in seq_along(lines)) {
    m <- if (split_speakers) matches[[i]] else character()
    if (length(m) && !grepl("(*UCP)^(https?|\\d+)$", py_strip(m[2]), perl = TRUE, ignore.case = TRUE)) {
      flush()
      speaker <- py_strip(m[2])
      collected <- m[3]
    } else if (blank[i]) {
      flush()
    } else {
      collected <- c(collected, lines[i])
    }
  }
  flush()
  if (!length(result) || length(result) > MAX_SEGMENTS) workflow_error("Transcript must contain 1\u20134,000 segments.")
  result
}

parse_response <- function(raw, stage, ids) {
  text_value(raw, "Model response", 500000)
  cleaned <- py_strip(raw)
  fence <- "```"
  if (startsWith(cleaned, fence) && endsWith(cleaned, fence)) {
    n <- nchar_py(cleaned)
    cleaned <- py_strip(if (n > 6) substr(cleaned, 4, n - 3) else "")
    if (startsWith(cleaned, "json")) cleaned <- py_strip(substring(cleaned, 5))
  }
  value <- tryCatch(json_parse(cleaned), error = function(e) {
    workflow_error("The response is not valid JSON. Paste the complete JSON object, without commentary.")
  })
  if (!is_obj(value) || !is_arr(value[["segments"]])) workflow_error("Expected a JSON object with a segments array.")
  items <- value$segments
  got <- lapply(items, function(item) if (is_obj(item)) item[["id"]] else NULL)
  ok <- length(got) == length(ids) &&
    all(vapply(seq_along(got), function(i) is_str(got[[i]]) && identical(got[[i]], ids[[i]]), logical(1)))
  if (!ok) workflow_error("Segment ids are missing, duplicated, reordered, or unexpected. No results were applied.")
  out <- vector("list", length(items))
  for (k in seq_along(items)) {
    item <- items[[k]]
    segment <- list(id = item$id, text = text_value(jget(item, "text"), "Translated text", 80000))
    if (stage %in% c("forward", "back")) {
      notes <- jget(item, "notes", list())
      if (!is_arr(notes) || length(notes) > 30) workflow_error("notes must be an array of at most 30 strings.")
      segment$notes <- lapply(notes, function(x) text_value(x, "note", 5000))
    } else {
      segment$rationale <- text_value(jget(item, "rationale"), "Reconciliation rationale", 10000)
      issues <- jget(item, "issues")
      if (!is_arr(issues) || length(issues) > 350) workflow_error("Reconciliation requires an issues array.")
      segment$issues <- list()
      for (issue in issues) {
        category <- if (is_obj(issue)) issue[["category"]] else NULL
        sev <- if (is_obj(issue)) issue[["severity"]] else NULL
        if (!is_obj(issue) || !is_str(category) || !(category %in% prompt_data()$categories) ||
            !is_str(sev) || !(sev %in% c("minor", "major", "critical"))) {
          workflow_error("Invalid reconciliation issue category or severity.")
        }
        segment$issues[[length(segment$issues) + 1L]] <- list(
          category = category, severity = sev,
          detail = text_value(jget(issue, "detail"), "Issue detail", 10000), needs_human = TRUE, origin = "model")
      }
      segment$needs_human <- TRUE
    }
    out[[k]] <- segment
  }
  out
}

result_text <- function(segment, stage) {
  result <- segment[[stage]]
  if (truthy(result)) result$text else ""
}

segment_ids <- function(segments) vapply(segments, function(s) s$id, character(1))

make_batches <- function(segments, settings, stage) {
  batches <- list(); batch <- character(); count <- 0
  for (seg in segments) {
    if (identical(stage, "forward")) {
      cost <- nchar_py(seg$source)
    } else if (identical(stage, "back")) {
      if (!truthy(seg$forward)) workflow_error("Complete all forward translations before blind back-translation.")
      cost <- nchar_py(result_text(seg, "forward"))
    } else {
      if (!truthy(seg$forward) || !truthy(seg$back)) {
        workflow_error("Complete forward and blind back-translations before reconciliation.")
      }
      cost <- nchar_py(seg$source) + nchar_py(result_text(seg, "forward")) + nchar_py(result_text(seg, "back"))
    }
    if (length(batch) && (count + cost > settings$batch_chars || length(batch) >= settings$batch_segments)) {
      batches[[length(batches) + 1L]] <- batch
      batch <- character(); count <- 0
    }
    batch <- c(batch, seg$id)
    count <- count + cost
  }
  if (length(batch)) batches[[length(batches) + 1L]] <- batch
  batches
}

build_task <- function(project, stage, ids, include_context = TRUE) {
  if (!is_str(stage) || !(stage %in% STAGES)) workflow_error("Invalid stage.")
  settings <- project$settings; all_segments <- project$segments
  all_ids <- segment_ids(all_segments)
  ids <- unlist(ids)
  if (!length(ids) || !all(ids %in% all_ids)) workflow_error("Invalid segment selection.")
  rows <- all_segments[match(ids, all_ids)]
  indices <- which(all_ids %in% ids)
  lo <- min(indices); hi <- max(indices)
  before <- if (include_context && lo > 1L) all_segments[lo - 1L] else list()
  after <- if (include_context && hi < length(all_segments)) all_segments[hi + 1L] else list()
  if (stage == "back") {
    if (any(vapply(c(rows, before, after), function(s) !truthy(s$forward), logical(1)))) {
      workflow_error("Forward translations are incomplete.")
    }
    # Construct an allowlisted back-translation payload from target text only.
    # Never pass a source segment object, source glossary, source notes, or study context.
    payload <- list(
      task = "blind_back_translation", translated_language = settings$target_language,
      original_language = settings$source_language,
      segments = lapply(rows, function(s) list(id = s$id, text = result_text(s, "forward"))),
      context_only = lapply(c(before, after), function(s) list(text = result_text(s, "forward"))))
  } else if (stage == "forward") {
    payload <- list(source_language = settings$source_language, target_language = settings$target_language,
                    study_context = settings$study_context, glossary = settings$glossary,
                    segments = lapply(rows, function(s) list(id = s$id, text = s$source)),
                    context_only = lapply(c(before, after), function(s) list(text = s$source)))
  } else {
    if (any(vapply(rows, function(s) !truthy(s$forward) || !truthy(s$back), logical(1)))) {
      workflow_error("Forward or back-translation is missing.")
    }
    payload <- list(source_language = settings$source_language, target_language = settings$target_language,
                    study_context = settings$study_context, glossary = settings$glossary,
                    segments = lapply(rows, function(s) list(
                      id = s$id, original_source = s$source,
                      forward_translation = result_text(s, "forward"),
                      forward_notes = oget(s$forward, "notes", list()),
                      blind_back_translation = result_text(s, "back"),
                      back_notes = oget(s$back, "notes", list()))),
                    context_only = lapply(c(before, after), function(s) list(original_source = s$source)))
  }
  if (stage != "back") {
    payload$terminology_lexicon <- settings$terminology %||% list()
    payload$dialect_context <- oget(settings$reporting, "dialect_context", "")
  }
  system <- prompt_data()$prompts[[stage]]
  addition <- settings$prompt_additions[[stage]]
  if (stage != "back" && nzchar(py_strip(addition))) {
    system <- paste0(system, "\nADDITIONAL TEAM INSTRUCTIONS (subject to the fidelity and JSON requirements above):\n", addition)
  }
  list(task_id = uuid_hex(), stage = stage, ids = as.list(ids), revision = project$revision,
       prompt_version = prompt_version(), system = system, user = json_pretty(payload),
       created_at = now_iso(), configuration = settings$stages[[stage]],
       max_tokens = settings$max_tokens, context_omitted = !include_context)
}

sampling_application <- function(config) {
  provider <- config$provider
  if (provider == "manual") return("manual_user_reported")
  if (provider == "copilot") return("external_chat_not_controlled")
  for (n in c("temperature", "top_p", "random_seed", "effort")) {
    v <- config[[n]]
    if (!is.null(v) && !identical(v, "")) return("api_request")
  }
  "provider_default"
}
