# Exports: CSV, parallel review HTML, methods record and the six-file research bundle.

csv_safe <- function(value) {
  value <- as.character(value)
  # A leading tab or carriage return is checked before trimming; formula characters after it.
  if (substr(value, 1, 1) %in% c("\t", "\r") || substr(py_lstrip(value), 1, 1) %in% c("=", "+", "-", "@")) paste0("'", value) else value
}

# csv.writer (excel dialect): quote fields containing a comma, quote or line break.
csv_field <- function(x) {
  if (grepl("[,\"\r\n]", x, perl = TRUE)) paste0("\"", gsub("\"", "\"\"", x, fixed = TRUE), "\"") else x
}

human_entered <- function(segment) {
  STAGES[vapply(STAGES, function(st) truthy(oget(segment[[st]], "human_entry")), logical(1))]
}

export_csv <- function(project) {
  header <- c("id", "speaker", "source", "forward", "blind_back_translation", "reconciled_draft", "reviewed_translation",
              "review_status", "reviewer", "review_note", "issues", "review_method", "human_entered_stages")
  lines <- paste(vapply(header, csv_field, character(1)), collapse = ",")
  for (s in project$segments) {
    r <- s$review %||% empty_obj()
    values <- c(s$id, s$speaker, s$source, result_text(s, "forward"), result_text(s, "back"),
                result_text(s, "reconcile"), oget(r, "text", ""), oget(r, "status", "pending"), oget(r, "reviewer", ""),
                oget(r, "note", ""), py_dumps(oget(s$reconcile, "issues", list())), oget(r, "method", ""),
                paste(human_entered(s), collapse = ";"))
    lines <- c(lines, paste(vapply(vapply(values, csv_safe, character(1)), csv_field, character(1)), collapse = ","))
  }
  text <- paste0("\ufeff", paste0(lines, "\r\n", collapse = ""))
  charToRaw(enc2utf8(text))
}

# All audit events, including those archived inside imported projects.
audit_events <- function(events) {
  out <- list()
  for (event in events %||% list()) {
    if (!is_obj(event)) next
    out[[length(out) + 1L]] <- event
    out <- c(out, audit_events(oget(oget(event, "details"), "prior_audit")))
  }
  out
}

# Describes runs recorded by a pre-release version that drove Copilot Chat through its web page,
# when they appear in an imported project.
COPILOT_HISTORY_LINES <- c(
  "Some runs in this project were made with the Copilot browser mode of an earlier QualiTranslate version, which controlled a dedicated Chrome/Edge window through the visible Copilot Chat page, not a Microsoft model API.",
  "For each of those batches that version loaded a new chat and confirmed on screen that it was empty before sending; each run's chat_isolation field states what was verified, and temporary_chat_notice records Copilot's own on-screen notice.",
  "A new or temporary chat does not prove that the service has no other context, retrieval or memory; independence equivalent to separate human translators is not claimed. Copilot does not report its underlying model; only the label shown on its model selector is recorded. Sampling settings were not controlled in Copilot mode.",
  "The per-request receipt verifies reply correlation, not translation correctness. The exact text placed in Copilot's message box is retained as transport_prompt.")

has_copilot_runs <- function(project) {
  any(vapply(project$runs %||% list(), function(r) {
    is_obj(r) && identical(oget(oget(r, "provenance"), "provider"), "copilot_browser")
  }, logical(1)))
}

has_copilot_refusals <- function(project) {
  any(vapply(audit_events(project$audit), function(e) {
    action <- oget(e, "action")
    is_str(action) && action %in% c("copilot_declined", "copilot_section_declined")
  }, logical(1)))
}

copilot_refusal_summary <- function(project) {
  events <- audit_events(project$audit)
  actions <- vapply(events, function(e) {
    a <- oget(e, "action", "")
    if (is_str(a)) a else ""
  }, character(1))
  declined <- events[actions == "copilot_section_declined"]
  pairs <- unique(lapply(declined, function(e) c(as.character(oget(e$details, "stage", "")), as.character(oget(e$details, "segment_id", "")))))
  if (length(pairs)) {
    o <- order(vapply(pairs, `[`, "", 1), vapply(pairs, `[`, "", 2), method = "radix")
    pairs <- pairs[o]
  }
  attempts <- sum(actions == "copilot_declined")
  entered <- lapply(stats_setnames(STAGES), function(st) {
    as.list(segment_ids(Filter(function(x) truthy(oget(x[[st]], "human_entry")), project$segments)))
  })
  paste0("Copilot refusals: ",
         if (length(pairs)) paste0(length(pairs), " section/stage pair(s) Copilot would not translate after isolation: ",
                                   paste(vapply(pairs, function(p) paste0(p[2], " (", p[1], ")"), ""), collapse = ", "))
         else "none recorded",
         "; ", attempts, " declined batch attempt(s) in total while isolating them (audit.json). Batches were split and a single section was retried once without neighbouring context; text was never rephrased to pass a filter. ",
         "Human-entered results (recorded as human work, not model output): ", py_dumps(entered), ".")
}

stats_setnames <- function(x) {
  names(x) <- x
  x
}

# A short display string from an imported record value (which may not be a string).
text_short <- function(value) if (is_str(value)) substr(py_strip(value), 1, 200) else ""

# Python str() of any JSON value.
py_str_any <- function(v) if (is_str(v)) v else if (is.list(v)) py_repr(v) else py_str(v)

sorted_set <- function(values) {
  values <- unique(as.character(unlist(values)))
  values <- values[nzchar(values)]
  if (!length(values)) return(character())
  sort(values, method = "radix")
}

# The runs whose results are still in the project for this stage (not superseded or cleared).
current_runs <- function(project, stage) {
  ids <- unlist(lapply(project$segments, function(s) {
    r <- oget(s[[stage]], "run_id")
    if (is_str(r)) r else NULL
  }))
  ids <- setdiff(unique(ids), c("", "human_entry"))
  Filter(function(r) is_obj(r) && identical(oget(r, "stage"), stage) && is_str(oget(r, "id")) && oget(r, "id") %in% ids,
         project$runs %||% list())
}

# How the current results of a stage were produced (from their run records, else settings).
tool_description <- function(project, stage) {
  runs <- current_runs(project, stage)
  provenance <- lapply(runs, function(r) { p <- oget(r, "provenance"); if (is_obj(p)) p else empty_obj() })
  configs <- lapply(runs, function(r) { cfg <- oget(r, "request_configuration"); if (is_obj(cfg)) cfg else empty_obj() })
  config <- project$settings$stages[[stage]]
  if (length(configs)) {
    last_config <- configs[[length(configs)]]
    for (nm in names(last_config)) config[nm] <- list(last_config[[nm]])
  }
  if (length(provenance) && all(vapply(provenance, function(p) identical(oget(p, "provider"), "synthetic_demo"), logical(1)))) {
    return("a built-in synthetic demonstration (no model)")
  }
  providers <- sorted_set(lapply(configs, function(cfg) text_short(oget(cfg, "provider"))))
  if (length(providers) > 1) {
    return(paste0("a mix of tools across batches (", paste(providers, collapse = ", "), "; see prompts_and_runs.json)"))
  }
  provider <- if (length(providers)) providers[1] else config$provider
  if (identical(provider, "anthropic") || identical(provider, "mistral")) {
    requested <- sorted_set(lapply(configs, function(cfg) text_short(oget(cfg, "model"))))
    if (!length(requested)) {
      fallback <- text_short(config$model)
      requested <- if (nzchar(fallback)) fallback else "[model id]"
    }
    returned <- sorted_set(lapply(provenance, function(p) text_short(oget(p, "returned_model"))))
    options <- character()
    sources <- if (length(configs)) configs else list(config)
    for (opt in list(c("temperature", "temperature"), c("top_p", "top-p"), c("random_seed", "seed"), c("effort", "reasoning effort"))) {
      values <- sorted_set(lapply(sources, function(cfg) {
        v <- oget(cfg, opt[1])
        if (is.null(v) || identical(v, "")) NULL else py_str_any(v)
      }))
      if (length(values)) options <- c(options, paste0(opt[2], " ", paste(values, collapse = "/")))
    }
    return(paste0(if (provider == "anthropic") "the Claude API (Anthropic)" else "the Mistral API", ", model ",
                  paste(requested, collapse = " / "),
                  if (length(returned) && !identical(returned, requested)) paste0(" (the API reported ", paste(returned, collapse = " / "), ")") else "",
                  if (length(options)) paste0(" (", paste(options, collapse = ", "), ")") else " (provider default sampling settings)"))
  }
  if (identical(provider, "copilot")) {
    labels <- sorted_set(lapply(provenance, function(p) text_short(oget(p, "copilot_model_selector_label"))))
    if (length(provenance)) {
      temporary <- sum(vapply(provenance, function(p) identical(oget(p, "chat_isolation"), "new_temporary_chat_verified"), logical(1)))
      chats <- if (temporary == length(provenance)) ", with each batch in a new temporary chat" else
        paste0(", with each batch in a new chat",
               if (temporary) paste0(" (a temporary chat for ", temporary, " of ", length(provenance), " batches)") else "")
    } else {
      chats <- ", with each batch in a new chat"
    }
    return(paste0("Microsoft 365 Copilot Chat under the institution's licence, operated automatically through its web interface", chats,
                  " (Copilot does not report its underlying model",
                  if (length(labels)) paste0("; its mode selector showed ", paste(vapply(labels, py_repr, ""), collapse = " / ")) else "",
                  ")"))
  }
  reported <- sorted_set(lapply(provenance, function(p) text_short(oget(p, "tool_model_reported_by_user"))))
  tool <- paste(reported, collapse = " / ")
  if (!nzchar(tool)) {
    parts <- c(text_short(oget(config, "chat_tool")), text_short(oget(config, "chat_model")))
    tool <- paste(parts[nzchar(parts)], collapse = " / ")
  }
  if (!nzchar(tool)) tool <- "[AI chat tool and model]"
  paste0(tool, " (used through its chat interface by copy and paste; model as reported by the user)")
}

# A draft methods paragraph, structured after the reporting items in Yunus et al. (2022):
# timing and object of translation, who translated and reviewed, procedure, review scope,
# and how discrepancies were handled. Every statement is derived from the current results
# and their run records; anything the app cannot know is left as a [bracketed] item.
methods_paragraph <- function(project) {
  settings <- project$settings; segs <- project$segments
  reporting <- settings$reporting %||% empty_obj()
  n <- length(segs)
  if (!n) return("[No transcript has been prepared yet.]")
  src <- settings$source_language; tgt <- settings$target_language
  done <- vapply(stats_setnames(STAGES), function(st) sum(vapply(segs, function(s) truthy(s[[st]]), logical(1))), integer(1))
  human <- vapply(stats_setnames(STAGES), function(st) sum(vapply(segs, function(s) truthy(oget(s[[st]], "human_entry")), logical(1))), integer(1))
  tools <- list()
  for (st in STAGES) if (done[[st]] > human[[st]]) tools[[st]] <- tool_description(project, st)
  runs <- lapply(stats_setnames(STAGES), function(st) current_runs(project, st))
  versions <- sorted_set(lapply(do.call(c, unname(runs)), function(r) text_short(oget(r, "prompt_version"))))
  coverage <- function(st) if (done[[st]] == n) "" else paste0(" for ", done[[st]], " of ", n, " sections")
  tool_or_person <- function(st) if (!is.null(tools[[st]])) tools[[st]] else "a person"

  parts <- character()
  if (truthy(project$demo)) parts <- c(parts, "[SYNTHETIC DEMONSTRATION \u2014 not a description of real research.]")
  scope <- oget(reporting, "translation_scope", "")
  parts <- c(parts, paste0("Interview material in ", src, " (", n, " transcript sections) was translated into ", tgt,
                           ". What was translated, and when: ",
                           sub("\\.+$", "", if (truthy(scope)) scope else "[what was translated, and at which point in the analysis]"), "."))
  dialect <- oget(reporting, "dialect_context", "")
  if (truthy(dialect)) parts <- c(parts, paste0("Dialect or cultural context given to the translation steps: ", sub("\\.+$", "", dialect), "."))
  parts <- c(parts, paste0("Translation followed a forward translation, blind back-translation and reconciliation procedure supported by QualiTranslate",
                           if (length(versions)) paste0(" (prompt version ", paste(versions, collapse = " / "), ")") else "", "."))
  if (!done[["forward"]]) {
    parts <- c(parts, "[Forward translation not yet run.]")
  } else {
    sizes <- vapply(runs$forward, function(r) {
      ids <- oget(r, "segment_ids")
      if (is_arr(ids)) length(ids) else 0L
    }, numeric(1))
    retried <- sum(vapply(do.call(c, unname(runs)), function(r) truthy(oget(r, "context_omitted")), logical(1)))
    parts <- c(parts, paste0("Forward translation into ", tgt, coverage("forward"), " was produced with ", tool_or_person("forward"),
                             if (length(sizes) && max(sizes) > 0) paste0(", in batches of up to ", max(sizes), " sections with the neighbouring section given as context only") else "",
                             if (retried) paste0(" (", retried, " batch(es) were retried as a single section without neighbouring context after the tool declined)") else "",
                             "."))
  }
  if (!done[["back"]]) {
    parts <- c(parts, "[Blind back-translation not yet run.]")
  } else {
    same <- !is.null(tools$back) && identical(tools$back, tools$forward)
    parts <- c(parts, paste0("The forward translation was then translated back into ", src, coverage("back"), " ",
                             if (same) "with the same tool as the forward translation" else paste0("with ", tool_or_person("back")),
                             ", in separate requests that contained only the forward translation (no original text, study context or glossary)."))
  }
  if (!done[["reconcile"]]) {
    parts <- c(parts, "[Reconciliation not yet run.]")
  } else {
    rec <- tool_or_person("reconcile")
    who <- if (identical(rec, tools$forward) && identical(rec, tools$back)) "the same tool" else
      if (identical(rec, tools$forward)) "the forward-translation tool" else
      if (identical(rec, tools$back)) "the back-translation tool" else rec
    parts <- c(parts, paste0("In a reconciliation step", coverage("reconcile"), ", ", who,
                             " compared the original, the forward translation and the back-translation, flagged possible discrepancies by category and severity, and proposed a revised translation."))
  }
  if (length(settings$terminology)) {
    parts <- c(parts, paste0("An agreed bilingual lexicon of ", length(settings$terminology),
                             " terms was supplied to the forward and reconciliation steps and checked locally."))
  }
  reconciled <- Filter(function(s) truthy(s$reconcile), segs)
  sevs <- vapply(reconciled, severity, character(1))
  flagged <- c(critical = sum(sevs == "critical"), major = sum(sevs == "major"), minor = sum(sevs == "minor"))
  categories <- integer()
  for (s in segs) {
    for (issue in oget(s$reconcile, "issues", list())) {
      k <- issue$category
      categories[k] <- if (k %in% names(categories)) categories[[k]] + 1L else 1L
    }
  }
  top <- ""
  if (length(categories)) {
    o <- order(-categories, names(categories), method = "radix")
    keys <- utils::head(names(categories)[o], 3)
    top <- paste(gsub("_", " ", gsub("idiom_metaphor", "idiom or metaphor", keys, fixed = TRUE), fixed = TRUE), collapse = ", ")
  }
  if (sum(flagged)) {
    parts <- c(parts, paste0("Reconciliation flagged ", sum(flagged), " of ", done[["reconcile"]], " sections (", flagged[["critical"]], " critical, ",
                             flagged[["major"]], " major, ", flagged[["minor"]], " minor)",
                             if (nzchar(top)) paste0("; categories flagged most often: ", top) else "", "."))
  }
  entered <- sum(vapply(segs, function(s) length(human_entered(s)) > 0, logical(1)))
  if (entered) {
    parts <- c(parts, paste0(entered, " section(s) that the AI tool declined to translate were translated by a person and are marked as human work."))
  }
  reviews <- lapply(Filter(function(s) truthy(s$review), segs), function(s) s$review)
  bg <- oget(reporting, "reviewer_background", "")
  background <- if (truthy(bg)) bg else "[name/role, language competence and domain expertise]"
  if (length(reviews)) {
    methods <- vapply(c(individual = "individual", quick_accept = "quick_accept", batch_minor = "batch_minor"), function(m) {
      sum(vapply(reviews, function(r) identical(oget(r, "method", "individual"), m), logical(1)))
    }, integer(1))
    people <- length(sorted_set(lapply(reviews, function(r) text_short(oget(r, "reviewer")))))
    retained <- sum(vapply(reviews, function(r) identical(oget(r, "status"), "reviewed_uncertainty_retained"), logical(1)))
    detail <- character()
    if (methods[["individual"]]) detail <- c(detail, paste0(methods[["individual"]], " after individual review against the original"))
    if (methods[["quick_accept"]]) detail <- c(detail, paste0(methods[["quick_accept"]], " approved unchanged after reading the section"))
    if (methods[["batch_minor"]]) {
      detail <- c(detail, paste0(methods[["batch_minor"]], " accepted unchanged in a batch of sections flagged only as minor, without individual review"))
    }
    parts <- c(parts, paste0("Decisions were recorded for ", length(reviews), " of ", n, " sections by ",
                             if (people <= 1) "one reviewer" else paste0(people, " reviewers"),
                             " (", background, "): ", paste(detail, collapse = "; "), ".",
                             if (retained) paste0(" For ", retained, " section(s) the reviewer recorded that uncertainty about the meaning remains.") else ""))
  } else {
    parts <- c(parts, paste0("[Human review not yet recorded: describe who (", background,
                             ") reviewed the proposed translations against the original, and how.]"))
  }
  decision <- oget(reporting, "decision_summary", "")
  parts <- c(parts, if (truthy(decision)) decision else "[How unresolved discrepancies and translation dilemmas were discussed and decided, and any that remain.]")
  parts <- c(parts, "Back-translation was used as a diagnostic aid, not as proof of equivalence. Prompts, model replies, edits and review decisions were kept in an audit record.")
  paste(parts, collapse = " ")
}

methods_record <- function(project) {
  settings <- project$settings
  counts <- lapply(stats_setnames(STAGES), function(s) sum(vapply(project$segments, function(x) truthy(x[[s]]), logical(1))))
  reviewed <- sum(vapply(project$segments, function(x) truthy(x$review), logical(1)))
  provenance <- lapply(project$runs, function(r) r$provenance)
  decision_methods <- lapply(stats_setnames(c("individual", "quick_accept", "batch_minor")), function(m) {
    sum(vapply(project$segments, function(x) truthy(x$review) && identical(oget(x$review, "method", "individual"), m), logical(1)))
  })
  stage_settings <- lapply(stats_setnames(STAGES), function(st) {
    cfg <- settings$stages[[st]]
    keep <- vapply(cfg, function(v) !is.null(v) && !identical(v, ""), logical(1))
    out <- cfg[keep]
    if (!length(out)) empty_obj() else out
  })
  document <- if (!is.null(project$source_document)) {
    excluded <- sum(!vapply(project$source_document$blocks, function(b) isTRUE(b$included), logical(1)))
    paste0(excluded, " blocks excluded by user confirmation; excluded text retained only in project record, not translation prompts.")
  } else {
    "No scanner selection record; source entered directly or demonstration."
  }
  lines <- c(
    "DRAFT METHODS PARAGRAPH (generated from this project; check every statement and complete the [bracketed] items before use)",
    methods_paragraph(project), "",
    "QualiTranslate methods record \u2014 factual workflow summary, not a validation claim",
    if (truthy(project$demo)) "SYNTHETIC DEMONSTRATION" else paste0("Research project: ", project$title),
    paste0("Created: ", project$created_at), paste0("Exported: ", now_iso()), paste0("Application version: ", APP_VERSION),
    paste0("Prompt version: ", prompt_version()),
    paste0("Languages: ", settings$source_language, " -> ", settings$target_language),
    paste0("Segments: ", length(project$segments)),
    paste0("Completed stages: ", py_dumps(counts)), paste0("Recorded review decisions: ", reviewed),
    paste0("Decision methods: ", py_dumps(decision_methods)),
    "Batch-minor acceptance is an explicit reviewer decision, not evidence of individual bilingual examination.",
    paste0("Team reporting: ", py_dumps(settings$reporting %||% empty_obj())),
    paste0("Terminology lexicon: ", py_dumps(settings$terminology %||% list())),
    paste0("Local identifier check of the source sections (warning only; the app does not remove or change text, and it cannot detect names or places): ",
           identifier_summary(identifier_scan(project$segments)), "."),
    paste0("Stage settings: ", py_dumps(stage_settings), "."),
    paste0("Document selection: ", document),
    "Forward translation, blind back-translation, and reconciliation are separate operations.",
    "API calls are stateless; each batch is a separate request.",
    if (has_copilot_runs(project)) COPILOT_HISTORY_LINES,
    if (has_copilot_refusals(project)) copilot_refusal_summary(project),
    "Back-translation requests contain only target-language translations and the language pair; no original-source text, source glossary, study context, or translator notes.",
    "In manual mode, this blindness depends on using a fresh conversation without source files/history for every back-translation batch.",
    "Reconciliation compares the source, forward translation and blind back-translation. It remains a draft until a named reviewer records a review.",
    "Back-translation is a diagnostic aid; this workflow does not establish psychometric validity, semantic equivalence, or inter-translator independence.",
    "Actual model / tool records (manual model labels are user reported):", json_pretty(provenance),
    "Exact prompts and responses are retained in project.json and prompts_and_runs.json.",
    "There is no universal prescribed percentage of bilingual checking in this application. Record the review scope actually performed.")
  paste0(paste(lines, collapse = "\n"), "\n")
}

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  gsub("'", "&#x27;", x, fixed = TRUE)
}

export_html <- function(project) {
  esc <- function(x) html_escape(as.character(x))
  rows <- character()
  for (s in project$segments) {
    review <- s$review %||% empty_obj()
    issues <- oget(s$reconcile, "issues", list())
    issue_text <- paste(vapply(issues, function(x) paste0(toupper(x$severity), " / ", x$category, ": ", x$detail), character(1)), collapse = "\n")
    entered <- human_entered(s)
    values <- c(paste0(s$id, "\n", s$speaker), s$source,
                vapply(STAGES, function(st) paste0(if (st %in% entered) "[HUMAN-ENTERED] " else "", result_text(s, st)), character(1)),
                oget(review, "text", ""),
                paste0(oget(review, "status", "PENDING HUMAN REVIEW"), " / ", oget(review, "method", ""), "\n",
                       oget(review, "reviewer", ""), "\n", oget(review, "note", ""), "\n", issue_text))
    rows <- c(rows, paste0("<tr>", paste0("<td>", gsub("\n", "<br>", vapply(values, esc, character(1)), fixed = TRUE), "</td>", collapse = ""), "</tr>"))
  }
  banner <- if (truthy(project$demo)) "SYNTHETIC DEMONSTRATION \u2014 NOT A LIVE TRANSLATION" else
    "Drafts remain provisional until an explicit reviewer decision; batch acceptance is identified separately."
  headers <- c("Segment / speaker", "Source", "Forward", "Blind back-translation", "Reconciled draft", "Accepted / reviewed text", "Decision method / issues")
  page <- paste0(
    "<!doctype html><html lang='en'><meta charset='utf-8'><meta name='viewport' content='width=device-width'><title>", esc(project$title),
    "</title><style>body{font:15px/1.55 system-ui;color:#152e34;margin:28px}h1{font-size:28px}table{border-collapse:collapse;width:100%}th,td{border:1px solid #ced8d7;padding:12px;text-align:left;vertical-align:top;min-width:160px}th{background:#eaf3ef}pre{white-space:pre-wrap;font:12px/1.5 monospace}.notice{background:#fff0d8;padding:12px}@media print{body{margin:0;font-size:10px}th,td{min-width:0;padding:6px}tr{break-inside:avoid}@page{size:landscape}}</style><h1>",
    esc(project$title), "</h1><p class='notice'>", esc(banner), "</p><p>", esc(project$settings$source_language), " \u2192 ",
    esc(project$settings$target_language), "</p><table><thead><tr>", paste0("<th>", headers, "</th>", collapse = ""),
    "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table><h2>Workflow record</h2><pre>",
    esc(methods_record(project)), "</pre></html>")
  charToRaw(enc2utf8(page))
}

export_bundle <- function(project) {
  dir <- tempfile("qt-bundle-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)
  write_raw <- function(name, bytes) writeBin(bytes, file.path(dir, name))
  write_raw("project.json", charToRaw(json_pretty(project)))
  write_raw("translations.csv", export_csv(project))
  write_raw("parallel_review.html", export_html(project))
  write_raw("methods_record.txt", charToRaw(enc2utf8(methods_record(project))))
  write_raw("prompts_and_runs.json", charToRaw(json_pretty(project$runs %||% list())))
  write_raw("audit.json", charToRaw(json_pretty(project$audit %||% list())))
  files <- c("project.json", "translations.csv", "parallel_review.html", "methods_record.txt", "prompts_and_runs.json", "audit.json")
  archive <- file.path(dir, "bundle.zip")
  zip::zip(archive, files = files, root = dir, mode = "cherry-pick")
  readBin(archive, "raw", file.size(archive))
}
