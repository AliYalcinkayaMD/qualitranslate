# The in-memory project workspace: state, edits, reviews, the translation job and import.

DEMO_SOURCE <- paste(
  "Interviewer: Hvorn\u00e5r kan man vurdere, om behandlingen var en succes?",
  "Participant A: Jeg tror f\u00f8rst, man kan sige det, n\u00e5r man er tilbage i sin hverdag.",
  "Interviewer: Hvad var vigtigst for dig efter operationen?",
  "Participant A: Det var ikke kun smerterne. Jeg ville ogs\u00e5 kunne g\u00e5 en tur uden at planl\u00e6gge det hele.",
  "Participant B: Et \u00e5r efter operationen havde jeg stadig ondt, men jeg var glad for, at jeg havde gjort det.",
  "Participant A: [pause] Jeg ved ikke helt, hvad \"normal\" betyder l\u00e6ngere.",
  sep = "\n")
DEMO_FORWARD <- c(
  "When can we assess whether the treatment was a success?",
  "I think you can only say that once you are back to your everyday life.",
  "What mattered most to you after the operation?",
  "It was only the pain. I also wanted to be able to go for a walk without planning everything.",
  "A month after the operation, I was still in pain, but I was glad I had done it.",
  "[pause] I do not really know what \"normal\" means anymore.")
DEMO_BACK <- c(
  "Hvorn\u00e5r kan vi vurdere, om behandlingen var en succes?",
  "Jeg tror, at man f\u00f8rst kan sige det, n\u00e5r man er tilbage i sin hverdag.",
  "Hvad var vigtigst for dig efter operationen?",
  "Det var kun smerterne. Jeg ville ogs\u00e5 kunne g\u00e5 en tur uden at planl\u00e6gge alt.",
  "En m\u00e5ned efter operationen havde jeg stadig smerter, men jeg var glad for at have gjort det.",
  "[pause] Jeg ved ikke rigtig, hvad \"normal\" betyder l\u00e6ngere.")
DEMO_CORRECT <- "It was not only the pain. I also wanted to be able to go for a walk without planning everything."

new_job <- function(running = FALSE, message = "Ready") {
  list(running = running, message = message, error = "", error_code = "", done = 0L, total = 0L,
       stage = "", batch = 0L, batches = 0L, declined = list())
}

new_project <- function() {
  list(format = "qualitranslate-project", format_version = 1L, app_version = APP_VERSION,
       project_id = uuid_hex(), title = "Untitled interview", created_at = now_iso(),
       revision = 0L, settings = settings_defaults(), segments = list(), runs = list(), audit = list(), demo = FALSE)
}

new_workspace <- function(client = NULL, copilot = NULL) {
  ws <- new.env(parent = emptyenv())
  ws$client <- client %||% new_api_client()
  ws$copilot <- copilot %||% new_copilot_browser()
  ws$copilot_connection <- new_copilot_connection(ws$copilot)
  ws$keys <- list()
  ws$tasks <- list()
  ws$inspection <- NULL
  ws$identifier_cache <- list(key = NULL, value = NULL)
  ws$cancel <- qt_event()
  ws$job <- new_job()
  ws$project <- new_project()
  ws$pending_job <- NULL     # a validated job waiting for the main loop
  ws$deferred <- list()      # actions that must run outside request handlers
  class(ws) <- "qt_workspace"
  ws
}

ws_idle <- function(ws) {
  if (isTRUE(ws$job$running)) {
    workflow_error("A translation job is running. Stop it and wait for it to finish before editing.")
  }
}

ws_audit <- function(ws, action, details) {
  events <- ws$project$audit
  event <- list(at = now_iso(), action = action, details = if (length(details)) details else empty_obj(),
                previous_hash = if (length(events)) events[[length(events)]]$hash else "")
  event$hash <- digest(event)
  ws$project$audit[[length(events) + 1L]] <- event
}

ws_changed <- function(ws) {
  ws$project$revision <- ws$project$revision + 1L
  ws$tasks <- list()
}

seg_index <- function(ws, segment_id) {
  if (!is_str(segment_id)) return(NA_integer_)
  match(segment_id, segment_ids(ws$project$segments))
}

ws_identifier_check <- function(ws) {
  # Possible identifiers in the source (cached per project revision).
  key <- list(ws$project$project_id, ws$project$revision, length(ws$project$segments))
  if (!identical(ws$identifier_cache$key, key)) {
    ws$identifier_cache <- list(key = key, value = identifier_scan(ws$project$segments))
  }
  ws$identifier_cache$value
}

ws_snapshot <- function(ws) {
  list(project = ws$project, job = ws$job, identifier_check = ws_identifier_check(ws),
       credentials = list(mistral = truthy(ws$keys$mistral), anthropic = truthy(ws$keys$anthropic)),
       copilot = ws$copilot_connection$summary())
}

ws_set_key <- function(ws, provider, key) {
  ws_idle(ws)
  if (!is_str(provider) || !(provider %in% c("mistral", "anthropic"))) workflow_error("Unknown provider.")
  key <- py_strip(text_value(key, "API key", 1000, TRUE))
  if (nzchar(key)) ws$keys[[provider]] <- key else ws$keys[[provider]] <- NULL
}

ws_clear <- function(ws) {
  ws_idle(ws)
  ws$keys <- list()
  ws$tasks <- list()
  ws$inspection <- NULL
  ws$project <- new_project()
  ws$job <- new_job()
}

ws_load_text <- function(ws, text, title = "Untitled interview", demo = FALSE, split_speakers = TRUE) {
  segments <- parse_transcript(text, split_speakers = split_speakers)
  title <- text_value(title, "title", 200)
  ws_idle(ws)
  settings <- if (demo) settings_defaults() else ws$project$settings
  project <- new_project()
  project$segments <- segments; project$settings <- settings; project$title <- title; project$demo <- demo
  ws$project <- project
  ws$job$declined <- list()
  ws$tasks <- list()
  ws$inspection <- NULL
  ws_audit(ws, "import_transcript", list(segments = length(segments),
                                         source_sha256 = digest(lapply(segments, function(s) s$source)),
                                         synthetic = demo, split_speakers = split_speakers))
}

ws_inspect_document <- function(ws, text, blocks = NULL) {
  result <- scan_document(text, blocks)
  ws_idle(ws)
  ws$inspection <- c(result, list(inspection_id = uuid_hex(), original_text = text, original_sha256 = digest(text)))
  out <- ws$inspection
  out$original_text <- NULL
  out
}

ws_import_selected <- function(ws, inspection_id, included_ids, title, settings, split_speakers = TRUE) {
  settings <- validate_settings(settings)
  title <- text_value(title, "Title", 200)
  ws_idle(ws)
  d <- ws$inspection
  if (is.null(d) || !identical(d$inspection_id, inspection_id)) {
    workflow_error("Document preview expired. Check the transcript again.")
  }
  if (!is_arr(included_ids) || !length(included_ids) || !all(vapply(included_ids, is_str, logical(1))) ||
      anyDuplicated(unlist(included_ids))) {
    workflow_error("Select at least one interview block, without duplicate ids.")
  }
  chosen <- unlist(included_ids)
  block_ids <- vapply(d$blocks, function(b) b$id, character(1))
  if (!all(chosen %in% block_ids)) workflow_error("An unknown document block was selected.")
  keep <- block_ids %in% chosen
  text <- paste(vapply(d$blocks[keep], function(b) b$text, character(1)), collapse = "\n\n")
  ws_load_text(ws, text, title, split_speakers = split_speakers)
  ws$project$settings <- settings
  ws$project$source_document <- list(
    scan_version = d$scan_version, original_sha256 = d$original_sha256, confirmed_at = now_iso(),
    blocks = lapply(seq_along(d$blocks), function(i) c(d$blocks[[i]], list(included = keep[i]))))
  ws_audit(ws, "document_selection_confirmed", list(included_ids = as.list(block_ids[keep]),
                                                    excluded_ids = as.list(block_ids[!keep]),
                                                    scan_version = d$scan_version))
  ws$inspection <- NULL
}

ws_save_settings <- function(ws, value) {
  settings <- validate_settings(value)
  ws_idle(ws)
  old <- ws$project$settings
  if (json_equal(settings, old)) return(invisible())
  # Batch size and credentials do not alter completed translations. Any linguistic,
  # model, or prompt change invalidates outputs to prevent mixed, stale provenance.
  excluded <- c("batch_chars", "batch_segments", "max_tokens", "reporting", "copilot", "stages", "prompt_additions")
  shared <- !json_equal(settings[setdiff(names(settings), excluded)], old[setdiff(names(old), excluded)])
  shared <- shared || !identical(settings$reporting$dialect_context, oget(old$reporting, "dialect_context", ""))
  # A change that only affects a later stage (for example a different back-translation
  # model) keeps the earlier stages; everything from the first affected stage is cleared.
  changed <- STAGES[vapply(STAGES, function(st) {
    !json_equal(settings$stages[[st]], oget(old$stages, st)) ||
      !json_equal(settings$prompt_additions[[st]], oget(old$prompt_additions, st))
  }, logical(1))]
  first <- if (shared) STAGES[1] else if (length(changed)) changed[1] else NULL
  substantive <- !is.null(first)
  if (substantive) {
    cleared <- STAGES[match(first, STAGES):length(STAGES)]
    for (i in seq_along(ws$project$segments)) {
      seg <- ws$project$segments[[i]]
      for (stage in cleared) seg <- jset(seg, stage, NULL)
      seg <- jset(seg, "review", NULL)
      ws$project$segments[[i]] <- seg
    }
  }
  ws$project$settings <- settings
  ws_changed(ws)
  ws_audit(ws, "settings_changed", list(outputs_invalidated = substantive, invalidated_from_stage = first %||% "",
                                        settings_sha256 = digest(settings)))
}

ws_next_ids <- function(ws, stage) {
  segments <- ws$project$segments
  if (!length(segments)) workflow_error("Import a transcript first.")
  batches <- make_batches(segments, ws$project$settings, stage)
  done <- vapply(segments, function(s) is_str(stage) && truthy(oget(s, stage)), logical(1))
  names(done) <- segment_ids(segments)
  # Skip already completed ids, so partial runs can resume without charging twice.
  out <- list()
  for (batch in batches) {
    pending <- batch[!done[batch]]
    if (length(pending)) out[[length(out) + 1L]] <- pending
  }
  out
}

ws_prepare_task <- function(ws, stage) {
  ws_idle(ws)
  pending <- ws_next_ids(ws, stage)
  if (!length(pending)) workflow_error("This stage is already complete.")
  task <- build_task(ws$project, stage, pending[[1]])
  ws$tasks[[task$task_id]] <- task
  task
}

ws_apply <- function(ws, task, raw, provenance) {
  if (!identical(as.integer(task$revision), as.integer(ws$project$revision))) {
    workflow_error("The project changed after this prompt was created. Generate a new prompt.")
  }
  stage <- task$stage
  results <- parse_response(raw, stage, task$ids)
  run_id <- uuid_hex()
  ids <- segment_ids(ws$project$segments)
  downstream <- STAGES[-seq_len(match(stage, STAGES))]
  for (k in seq_along(results)) {
    item <- results[[k]]
    i <- match(item$id, ids)
    seg <- ws$project$segments[[i]]
    if (stage == "reconcile") {
      checks <- terminology_issues(seg$source, item$text, ws$project$settings$terminology %||% list())
      item$issues <- c(item$issues, checks)
      results[[k]] <- item     # the run record shows the same issues as the segment
    }
    result <- item[names(item) != "id"]
    result$run_id <- run_id
    seg[[stage]] <- result
    for (d in downstream) seg <- jset(seg, d, NULL)
    seg <- jset(seg, "review", NULL)
    ws$project$segments[[i]] <- seg
  }
  config <- task$configuration
  config$max_tokens <- task$max_tokens
  config$sampling_application <- sampling_application(task$configuration)
  run <- list(id = run_id, stage = stage, segment_ids = as.list(unlist(task$ids)),
              prompt_version = prompt_version(), system_prompt = task$system,
              user_prompt = task$user, response_sha256 = digest(raw),
              raw_response = raw, parsed_results = results,
              transport_prompt = task$transport_prompt %||% "",
              context_omitted = task$context_omitted %||% FALSE,
              request_configuration = config,
              provenance = provenance)
  ws$project$runs[[length(ws$project$runs) + 1L]] <- run
  ws_changed(ws)
  ws_audit(ws, "stage_completed", list(stage = stage, segment_ids = as.list(unlist(task$ids)), run_id = run_id,
                                       provenance = provenance))
}

ws_apply_manual <- function(ws, task_id, raw, model_label) {
  ws_idle(ws)
  task <- if (is_str(task_id) && task_id %in% names(ws$tasks)) ws$tasks[[task_id]] else NULL
  if (is.null(task)) workflow_error("Prompt expired. Generate a new prompt for the current project.")
  model_label <- text_value(model_label, "Tool / model used", 200)
  ws_apply(ws, task, raw, list(provider = "manual", tool_model_reported_by_user = model_label, completed_at = now_iso()))
}

ws_edit <- function(ws, segment_id, stage, text, notes = "") {
  if (!is_str(stage) || !(stage %in% c("source", STAGES))) workflow_error("Unknown stage.")
  text_value(text, "Edited text", 80000)
  ws_idle(ws)
  i <- seg_index(ws, segment_id)
  if (is.na(i)) workflow_error("Unknown segment.")
  seg <- ws$project$segments[[i]]
  if (stage == "reconcile" && truthy(seg$reconcile)) workflow_error("Use human review to change a reconciled translation.")
  if (stage != "source" && !truthy(seg[[stage]])) {
    # A section the tool could not translate (for example, Copilot declined it) can be
    # given a human translation. It is recorded as human work, not model output.
    prior <- if (stage != "forward") STAGES[match(stage, STAGES) - 1L] else NULL
    if (!is.null(prior) && !truthy(seg[[prior]])) workflow_error("Add the earlier stage for this section first.")
    entry <- list(at = now_iso(), notes = text_value(notes, "Edit notes", 5000, TRUE))
    if (stage == "reconcile") {
      seg[[stage]] <- list(text = text, issues = list(),
                           rationale = "Proposed by a person because the model did not produce a reconciliation for this section.",
                           needs_human = TRUE, run_id = "human_entry", human_entry = entry)
    } else {
      seg[[stage]] <- list(text = text, notes = list(), run_id = "human_entry", human_entry = entry)
    }
    for (d in STAGES[-seq_len(match(stage, STAGES))]) seg <- jset(seg, d, NULL)
    seg <- jset(seg, "review", NULL)
    ws$project$segments[[i]] <- seg
    ws$project$demo <- FALSE
    ws_changed(ws)
    ws_audit(ws, "human_translation_entered", list(segment_id = segment_id, stage = stage,
                                                   after_sha256 = digest(text), note = notes))
    return(invisible())
  }
  before <- if (stage == "source") seg$source else seg[[stage]]$text
  if (identical(before, text)) return(invisible())
  if (stage == "source") {
    seg$source <- text
    invalidated <- STAGES
    ws$job$declined <- list()
  } else {
    seg[[stage]]$text <- text
    seg[[stage]]$human_edit <- list(at = now_iso(), notes = text_value(notes, "Edit notes", 5000, TRUE))
    invalidated <- STAGES[-seq_len(match(stage, STAGES))]
  }
  ws$project$segments[[i]] <- seg
  # Neighbouring context may have influenced every batch, so invalidate all
  # dependent translations, not just this row.
  for (j in seq_along(ws$project$segments)) {
    other <- ws$project$segments[[j]]
    for (d in invalidated) other <- jset(other, d, NULL)
    other <- jset(other, "review", NULL)
    ws$project$segments[[j]] <- other
  }
  ws$project$demo <- FALSE
  ws_changed(ws)
  ws_audit(ws, "human_edit", list(segment_id = segment_id, stage = stage, before_sha256 = digest(before),
                                  after_sha256 = digest(text), note = notes))
}

ws_review <- function(ws, segment_id, final_text, reviewer, note, uncertainty_retained = FALSE, method = "individual") {
  if (!is_str(method) || !(method %in% c("individual", "quick_accept"))) workflow_error("Invalid review method.")
  ws_idle(ws)
  i <- seg_index(ws, segment_id)
  if (is.na(i) || !truthy(ws$project$segments[[i]]$reconcile)) workflow_error("Reconcile this segment before human review.")
  seg <- ws$project$segments[[i]]
  reviewer <- text_value(reviewer, "Reviewer", 150)
  final_text <- text_value(final_text, "Reviewed translation", 80000)
  note <- text_value(note, "Review note", 15000, TRUE)
  term_warnings <- terminology_issues(seg$source, final_text, ws$project$settings$terminology %||% list())
  significant <- any(vapply(term_warnings, function(x) x$severity == "major", logical(1))) ||
    any(vapply(seg$reconcile$issues, function(x) x$severity %in% c("major", "critical"), logical(1)))
  if (method == "quick_accept" && (severity(seg) %in% c("major", "critical") || uncertainty_retained ||
                                   !identical(final_text, seg$reconcile$text))) {
    workflow_error("Quick approval is available for unchanged, unflagged or minor-only drafts. Open review for other decisions.")
  }
  if ((significant || uncertainty_retained) && !nzchar(py_strip(note))) {
    workflow_error("Add a review note explaining the major issue or retained uncertainty.")
  }
  seg$review <- list(text = final_text, reviewer = reviewer, note = note, at = now_iso(),
                     status = if (uncertainty_retained) "reviewed_uncertainty_retained" else "reviewed",
                     based_on_run = seg$reconcile$run_id, method = method, terminology_warnings = term_warnings)
  ws$project$segments[[i]] <- seg
  ws_changed(ws)
  ws_audit(ws, "human_review", list(segment_id = segment_id, reviewer = reviewer, status = seg$review$status,
                                    translation_sha256 = digest(final_text), note = note, method = method))
}

ws_review_batch_minor <- function(ws, ids, reviewer, expected_revision) {
  reviewer <- text_value(reviewer, "Reviewer", 150)
  ws_idle(ws)
  if (!is_int(expected_revision) || expected_revision != ws$project$revision) {
    workflow_error("The project changed. Refresh the minor approval list before continuing.")
  }
  if (!is_arr(ids) || !length(ids) || !all(vapply(ids, is_str, logical(1))) || anyDuplicated(unlist(ids))) {
    workflow_error("Choose a non-empty list of unique minor-only sections.")
  }
  ids <- unlist(ids)
  idx <- match(ids, segment_ids(ws$project$segments))
  terms <- ws$project$settings$terminology %||% list()
  ok <- vapply(idx, function(i) {
    if (is.na(i)) return(FALSE)
    s <- ws$project$segments[[i]]
    truthy(s$reconcile) && !truthy(s$review) && severity(s) == "minor"
  }, logical(1))
  if (!all(ok)) {
    workflow_error("Batch approval accepts only pending minor-only sections. Major, critical, unflagged, and reviewed sections were not changed.")
  }
  if (any(vapply(idx, function(i) {
    s <- ws$project$segments[[i]]
    any(vapply(terminology_issues(s$source, s$reconcile$text, terms), function(w) w$severity == "major", logical(1)))
  }, logical(1)))) {
    workflow_error("A required terminology rule needs an individual decision. No batch approvals were applied.")
  }
  batch_id <- uuid_hex()
  for (i in idx) {
    seg <- ws$project$segments[[i]]
    seg$review <- list(text = seg$reconcile$text, reviewer = reviewer,
                       note = "Accepted unchanged in an explicit user-selected batch of minor-only sections.",
                       at = now_iso(), status = "reviewed", method = "batch_minor", batch_id = batch_id,
                       based_on_run = seg$reconcile$run_id)
    ws$project$segments[[i]] <- seg
  }
  ws_changed(ws)
  ws_audit(ws, "human_batch_review", list(method = "batch_minor", batch_id = batch_id, segment_ids = as.list(ids),
                                          reviewer = reviewer, count = length(ids)))
}

ws_clear_review <- function(ws, segment_id) {
  ws_idle(ws)
  i <- seg_index(ws, segment_id)
  if (is.na(i) || !truthy(ws$project$segments[[i]]$review)) workflow_error("No recorded decision to clear.")
  previous <- ws$project$segments[[i]]$review
  ws$project$segments[[i]] <- jset(ws$project$segments[[i]], "review", NULL)
  ws_changed(ws)
  ws_audit(ws, "review_returned_to_pending", list(segment_id = segment_id, previous_review = previous))
}

ws_demo_response <- function(ws, task) {
  sources <- vapply(ws$project$segments, function(s) s$source, character(1))
  expected <- vapply(parse_transcript(DEMO_SOURCE), function(s) s$source, character(1))
  if (!isTRUE(ws$project$demo) || !identical(unname(sources), unname(expected))) {
    workflow_error("The demonstration runs only on the unchanged synthetic sample.")
  }
  if (casefold_py(ws$project$settings$source_language) != "danish" || casefold_py(ws$project$settings$target_language) != "english") {
    workflow_error("The demonstration requires Danish \u2192 English.")
  }
  all_ids <- segment_ids(ws$project$segments)
  rows <- list()
  for (sid in unlist(task$ids)) {
    idx <- match(sid, all_ids)
    if (task$stage %in% c("forward", "back")) {
      rows[[length(rows) + 1L]] <- list(id = sid, text = (if (task$stage == "forward") DEMO_FORWARD else DEMO_BACK)[idx], notes = list())
    } else {
      issues <- list()
      proposed <- DEMO_FORWARD[idx]; rationale <- "No discrepancy identified in this synthetic example."
      if (idx == 4) {
        issues <- list(list(category = "negation", severity = "critical", detail = "The forward translation omitted 'ikke' (not), reversing the meaning. Restore 'not only'.", needs_human = TRUE))
        proposed <- DEMO_CORRECT; rationale <- "Restored the missing negation."
      } else if (idx == 5) {
        issues <- list(list(category = "temporality", severity = "major", detail = "The original says one year after surgery; the forward and back-translations say one month. Restore the original time point.", needs_human = TRUE))
        proposed <- "A year after the operation, I was still in pain, but I was glad I had done it."
        rationale <- "Restored one year rather than one month."
      } else if (idx == 6) {
        issues <- list(list(category = "source_ambiguity", severity = "minor", detail = "'Normal' is intentionally left undefined by the participant. Preserve the quotation marks and uncertainty; do not replace it with an objective recovery standard.", needs_human = TRUE))
        rationale <- "Retained the participant's unresolved meaning of normal."
      }
      rows[[length(rows) + 1L]] <- list(id = sid, text = proposed, issues = issues, rationale = rationale, needs_human = TRUE)
    }
  }
  json_str(list(segments = rows))
}

# Validates exactly like Python's start_job and queues the job. The main loop runs it
# (ws_run_job), because a request handler must return at once.
ws_start_job <- function(ws, stage, demo = FALSE, expected_project_id = NULL, expected_revision = NULL) {
  if (!is_str(stage) || !(stage %in% c(STAGES, "all"))) workflow_error("Unknown translation stage.")
  ws_idle(ws)
  if ((!is.null(expected_project_id) && !json_equal(expected_project_id, ws$project$project_id)) ||
      (!is.null(expected_revision) && !json_equal(expected_revision, ws$project$revision))) {
    workflow_error("The project changed while Copilot was connecting. Nothing was sent. Connect again to translate the current interview.")
  }
  stages <- if (stage == "all") STAGES else stage
  if (!length(ws$project$segments)) workflow_error("Import a transcript first.")
  if (!demo) {
    for (s in stages) {
      config <- ws$project$settings$stages[[s]]
      if (config$provider == "manual") {
        workflow_error("Configure API providers for these stages, or use the manual prompt workflow.")
      }
      if (config$provider == "copilot") {
        if (!isTRUE(ws$copilot_connection$summary()$ready)) {
          workflow_error("Open Copilot, sign in yourself, and check the connection before starting.")
        }
        next
      }
      if (!truthy(ws$keys[[config$provider]]) || !nzchar(py_strip(config$model))) {
        workflow_error(paste0("Set an API key and model id for ", s, " first."))
      }
    }
  } else {
    ws_demo_response(ws, build_task(ws$project, "forward", list(ws$project$segments[[1]]$id)))
  }
  # Validate prerequisites for a single-stage run before launching.
  if (stage != "all") ws_next_ids(ws, stage)
  ws$cancel$clear()
  ws$job <- new_job(running = TRUE, message = "Starting")
  ws$pending_job <- list(stages = stages, demo = demo)
  invisible()
}

ws_run_job <- function(ws) {
  pending <- ws$pending_job
  ws$pending_job <- NULL
  if (is.null(pending)) return(invisible())
  stages <- pending$stages; demo <- pending$demo
  tryCatch({
    for (stage in stages) {
      batches <- ws_next_ids(ws, stage)
      ws$job$total <- ws$job$total + length(batches)
      queue <- lapply(batches, function(ids) list(ids = ids, context = TRUE))
      number <- 0L; declined <- character()
      while (length(queue)) {
        item <- queue[[1]]; queue <- queue[-1]; number <- number + 1L
        ids <- item$ids; with_context <- item$context
        if (ws$cancel$is_set()) workflow_error("Stopped. Completed batches were retained.")
        task <- build_task(ws$project, stage, ids, include_context = with_context)
        planned <- number + length(queue)
        ws$job$stage <- stage; ws$job$batch <- number; ws$job$batches <- planned
        ws$job$message <- paste0(if (demo) "Demonstration: " else "", STAGE_LABELS[[stage]], " \u00b7 batch ", number,
                                 " of ", planned, " \u00b7 ", ids[1],
                                 if (length(ids) > 1) paste0("\u2013", ids[length(ids)]) else "")
        provider <- task$configuration$provider
        key <- if (provider %in% names(ws$keys)) ws$keys[[provider]] else ""
        if (provider == "copilot") ws$copilot$temporary_chat <- oget(ws$project$settings$copilot, "temporary_chat", TRUE)
        # The Copilot transport records what it typed (transport_prompt) and any refusal text
        # on the task, as the Python version does; the box makes those changes visible here.
        box <- new.env(parent = emptyenv())
        box$task <- task
        outcome <- tryCatch({
          if (demo) {
            list(raw = ws_demo_response(ws, task),
                 provenance = list(provider = "synthetic_demo", model = "deterministic-fixture", completed_at = now_iso()))
          } else if (provider == "copilot") {
            ws$copilot$complete(box, ws$cancel)
          } else {
            ws$client$complete(task, key)
          }
        }, qt_browser_error = function(e) {
          if (!identical(e$code, "declined")) stop(e)
          e
        })
        task <- box$task
        if (inherits(outcome, "qt_browser_error")) {
          # Copilot declined this batch. Split it to find the section(s) concerned and keep
          # translating the rest. Text is never rephrased or disguised to pass a filter.
          reply <- task$declined_reply %||% ""
          ws_audit(ws, "copilot_declined", list(stage = stage, segment_ids = as.list(ids), context_included = with_context,
                                                reply_excerpt = substr(reply, 1, 500)))
          if (length(ids) > 1) {
            half <- length(ids) %/% 2L
            queue <- c(list(list(ids = ids[seq_len(half)], context = with_context),
                            list(ids = ids[(half + 1L):length(ids)], context = with_context)), queue)
            ws$job$total <- ws$job$total + 1L
          } else if (with_context) {
            # A neighbouring section sent as context can be what Copilot objected to.
            # Retry this one section once without neighbours; its own text is unchanged.
            queue <- c(list(list(ids = ids, context = FALSE)), queue)
          } else {
            declined <- c(declined, ids[1])
            ws$job$declined[[length(ws$job$declined) + 1L]] <- list(
              stage = stage, id = ids[1], reply = substr(reply, 1, 300), project_id = ws$project$project_id)
            ws_audit(ws, "copilot_section_declined", list(stage = stage, segment_id = ids[1]))
            ws$job$done <- ws$job$done + 1L
          }
          next
        }
        # A reply that already arrived is kept even if Stop was pressed meanwhile.
        ws_apply(ws, task, outcome$raw, outcome$provenance)
        ws$job$done <- ws$job$done + 1L
        if (ws$cancel$is_set()) workflow_error("Stopped after the current request. Completed batches were retained.")
      }
      if (length(declined)) {
        workflow_error(paste0(
          "Copilot declined to translate ", length(declined), " section", if (length(declined) > 1) "s" else "",
          " in ", tolower(STAGE_LABELS[[stage]]), ": ", paste(declined, collapse = ", "),
          ". All other sections in this stage were completed. Add your own translation for ",
          if (length(declined) > 1) "these sections" else "this section",
          " under Translate \u2192 Sections that need a human translation, then click Resume to continue."))
      }
    }
    ws$job$message <- "Stages complete. Human review is still required."
  }, error = function(e) {
    ws$job$error <- if (is_qt_message_error(e)) conditionMessage(e) else "An unexpected error occurred. Completed batches were retained."
    ws$job$error_code <- if (inherits(e, "qt_browser_error")) (e$code %||% "") else ""
    ws$job$message <- "Stopped"
  }, finally = {
    ws$job$running <- FALSE
  })
  invisible()
}

ws_import_project <- function(ws, value) {
  fv <- if (is_obj(value)) value[["format_version"]] else NULL
  if (!is_obj(value) || !identical(value[["format"]], "qualitranslate-project") ||
      !((is_num(fv) || is_bool(fv)) && as.numeric(fv) == 1)) {
    workflow_error("This is not a supported QualiTranslate project.")
  }
  if (nchar_py(canonical(value)) > 20000000) workflow_error("Project exceeds the 20 MB import limit.")
  settings <- validate_settings(jget(value, "settings", empty_obj()))
  title <- text_value(jget(value, "title", "Imported interview"), "Title", 200)
  prior_project_id <- text_value(jget(value, "project_id", "unknown"), "Project id", 150)
  segments <- jget(value, "segments")
  if (!is_arr(segments) || length(segments) < 1 || length(segments) > MAX_SEGMENTS) {
    workflow_error("Invalid project segments.")
  }
  terms <- settings$terminology %||% list()
  clean <- vector("list", length(segments)); ids <- character()
  for (k in seq_along(segments)) {
    old <- segments[[k]]
    if (!is_obj(old)) workflow_error("Invalid segment.")
    sid <- text_value(jget(old, "id"), "Segment id", 20)
    if (!grepl("^S[0-9]{4,8}\\z", sid, perl = TRUE) || sid %in% ids) workflow_error("Invalid or duplicate segment id.")
    ids <- c(ids, sid)
    row <- list(id = sid, speaker = text_value(jget(old, "speaker", ""), "Speaker", 150, TRUE),
                source = text_value(jget(old, "source"), "Source", 80000), review = NULL)
    for (stage in STAGES) {
      stored <- jget(old, stage)
      if (truthy(stored)) {
        if (!is_obj(stored)) format_error()
        merged <- list(id = sid)
        for (nm in names(stored)) merged[nm] <- list(stored[[nm]])
        result <- parse_response(json_str(list(segments = list(merged))), stage, list(sid))[[1]]
        if (stage == "reconcile") {
          archived <- jget(stored, "issues", list())
          for (j in seq_len(min(length(result$issues), length(archived)))) {
            result$issues[[j]]$origin <- if (identical(oget(archived[[j]], "origin"), "local_glossary_check")) "local_glossary_check" else "model"
          }
        }
        entry <- result[names(result) != "id"]
        entry$run_id <- text_value(jget(stored, "run_id", "imported"), "run id", 150)
        if (truthy(jget(stored, "human_entry"))) {
          he <- stored$human_entry
          if (!is_obj(he)) format_error()
          entry$human_entry <- list(at = text_value(jget(he, "at", ""), "entry time", 100, TRUE),
                                    notes = text_value(jget(he, "notes", ""), "entry note", 5000, TRUE))
        }
        if (truthy(jget(stored, "human_edit"))) {
          he <- stored$human_edit
          if (!is_obj(he)) format_error()
          entry$human_edit <- list(at = text_value(jget(he, "at", ""), "edit time", 100, TRUE),
                                   notes = text_value(jget(he, "notes", ""), "edit note", 5000, TRUE))
        }
        row[[stage]] <- entry
      } else {
        row <- jset(row, stage, NULL)
      }
    }
    if ((truthy(row$back) && !truthy(row$forward)) || (truthy(row$reconcile) && !truthy(row$back))) {
      workflow_error("Imported stage prerequisites are inconsistent.")
    }
    r <- jget(old, "review")
    if (truthy(r) && truthy(row$reconcile)) {
      if (!is_obj(r)) format_error()
      status <- jget(r, "status")
      if (!is_str(status) || !(status %in% c("reviewed", "reviewed_uncertainty_retained"))) {
        workflow_error("Invalid review status.")
      }
      method <- jget(r, "method", "individual")
      row$review <- list(
        text = text_value(jget(r, "text"), "Reviewed text", 80000),
        reviewer = text_value(jget(r, "reviewer"), "Reviewer", 150),
        note = text_value(jget(r, "note", ""), "Review note", 15000, TRUE),
        at = text_value(jget(r, "at", ""), "Review date", 100, TRUE),
        status = status, based_on_run = row$reconcile$run_id,
        method = if (is_str(method) && method %in% c("individual", "quick_accept", "batch_minor")) method else "individual",
        batch_id = text_value(jget(r, "batch_id", ""), "Batch id", 100, TRUE),
        terminology_warnings = terminology_issues(row$source, jget(r, "text", ""), terms))
    }
    clean[[k]] <- row
  }
  # History is retained as an imported archive and never executed. Strip any unknown
  # history keys rather than copying arbitrary uploaded content into the active state.
  runs <- jget(value, "runs", list())
  if (!is_arr(runs) || length(runs) > 25000) workflow_error("Invalid run history.")
  allowed_run <- c("id", "stage", "segment_ids", "prompt_version", "system_prompt", "user_prompt", "transport_prompt",
                   "context_omitted", "response_sha256", "raw_response", "parsed_results", "provenance")
  allowed_provenance <- c("provider", "model", "requested_model", "returned_model", "request_id", "usage", "completed_at",
                          "tool_model_reported_by_user", "integration", "chat_isolation", "receipt_verified",
                          "copilot_model_selector_label", "temporary_chat_notice", "browser", "submitted_at")
  allowed_config <- c("provider", "model", "workspace_id", "max_tokens", "temperature", "top_p", "random_seed",
                      "effort", "sampling_application", "chat_tool", "chat_model")
  keep_keys <- function(x, allowed) {
    out <- empty_obj()
    for (nm in names(x)) if (nm %in% allowed) out[nm] <- list(x[[nm]])
    out
  }
  clean_runs <- vector("list", length(runs))
  for (k in seq_along(runs)) {
    run <- runs[[k]]
    st <- if (is_obj(run)) run[["stage"]] else NULL
    if (!is_obj(run) || !is_str(st) || !(st %in% STAGES)) workflow_error("Invalid run record.")
    kept <- keep_keys(run, allowed_run)
    # Archived runs are shown and summarised, never executed; normalise the fields that the
    # summaries read so an odd or foreign project file cannot break the exports (as Python).
    ids <- kept[["segment_ids"]]
    kept$segment_ids <- if (is_arr(ids)) Filter(is_str, ids) else list()
    for (nm in c("id", "prompt_version")) {
      if (!is.null(kept[[nm]]) && !is_str(kept[[nm]])) kept[[nm]] <- py_str(kept[[nm]])
    }
    provenance <- jget(run, "provenance", empty_obj())
    if (!is_obj(provenance)) provenance <- empty_obj()
    kept$provenance <- keep_keys(provenance, allowed_provenance)
    config <- jget(run, "request_configuration", empty_obj())
    if (is_obj(config)) kept$request_configuration <- keep_keys(config, allowed_config)
    clean_runs[[k]] <- kept
  }
  source_document <- jget(value, "source_document")
  clean_document <- NULL
  if (!is.null(source_document)) {
    blocks_in <- if (is_obj(source_document)) source_document[["blocks"]] else NULL
    if (!is_obj(source_document) || !is_arr(blocks_in) || length(blocks_in) > 10000) {
      workflow_error("Invalid document-selection record.")
    }
    blocks <- lapply(blocks_in, function(b) {
      if (!is_obj(b) || !is_bool(b[["included"]])) workflow_error("Invalid document block record.")
      list(id = text_value(jget(b, "id"), "Block id", 30), text = text_value(jget(b, "text"), "Block text", MAX_TEXT),
           kind = text_value(jget(b, "kind", "paragraph"), "Block kind", 40),
           reason = text_value(jget(b, "reason", ""), "Block reason", 2000, TRUE), included = b$included,
           needs_check = is_true(b[["needs_check"]]), suggested_exclude = is_true(b[["suggested_exclude"]]))
    })
    clean_document <- list(
      blocks = blocks,
      scan_version = text_value(jget(source_document, "scan_version", "unknown"), "Scanner version", 100),
      original_sha256 = text_value(jget(source_document, "original_sha256", "unknown"), "Document hash", 100),
      confirmed_at = text_value(jget(source_document, "confirmed_at", ""), "Confirmation time", 100, TRUE))
  }
  ws_idle(ws)
  project <- new_project()
  project$title <- title
  project$settings <- settings
  project$segments <- clean
  project$runs <- clean_runs
  project$demo <- is_true(value[["demo"]])
  project$imported_from_project_id <- prior_project_id
  if (!is.null(clean_document)) project$source_document <- clean_document
  ws$project <- project
  ws$tasks <- list()
  ws$inspection <- NULL
  ws$job$declined <- list()
  ws_audit(ws, "project_imported", list(imported_sha256 = digest(value), prior_audit = jget(value, "audit", list())))
}
