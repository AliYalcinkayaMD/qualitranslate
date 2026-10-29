# Port of python/tests/test_research_workflow.py (document selection, review decisions,
# settings and terminology).

completed_demo <- function() {
  ws <- qt("new_workspace")(fake_client(), scripted_copilot())
  qt("ws_load_text")(ws, qt("DEMO_SOURCE"), "Synthetic research demo", demo = TRUE)
  start_and_run(ws, "all", demo = TRUE)
  expect_identical(ws$job$error, "")
  ws
}

doc_text <- "Study ID: ADMIN_SECRET_91\nDate: ADMIN_SECRET_92\n\nInterviewer: What mattered?\nParticipant A: Less pain.\nAppendix: CODING_SECRET_93\nCoding interpretation follows."
new_ws <- function() qt("new_workspace")(fake_client(), scripted_copilot())

test_that("local detection and confirmed exclusions do not enter prompts", {
  ws <- new_ws()
  inspected <- qt("ws_inspect_document")(ws, doc_text)
  expect_gte(inspected$suspected, 4)
  chosen <- lapply(Filter(function(b) !b$needs_check, inspected$blocks), function(b) b$id)
  expect_length(chosen, 2)
  qt("ws_import_selected")(ws, inspected$inspection_id, chosen, "Interview", qt("settings_defaults")())
  prompt <- qt("ws_prepare_task")(ws, "forward")$user
  for (secret in c("ADMIN_SECRET_91", "ADMIN_SECRET_92", "CODING_SECRET_93", "Coding interpretation follows")) {
    expect_false(grepl(secret, prompt, fixed = TRUE))
  }
  expect_match(prompt, "Less pain.", fixed = TRUE)
  expect_true(any(!vapply(ws$project$source_document$blocks, function(b) b$included, logical(1))))
  expect_match(qt("methods_record")(ws$project), "blocks excluded by user confirmation", fixed = TRUE)
  expect_null(ws$inspection)
})

test_that("scanning never changes the current project", {
  ws <- new_ws()
  qt("ws_load_text")(ws, "P: Existing interview", "Existing")
  before <- ws$project
  qt("ws_inspect_document")(ws, doc_text)
  expect_identical(ws$project, before)
})

test_that("the user can keep suspected material", {
  ws <- new_ws()
  d <- qt("ws_inspect_document")(ws, doc_text)
  qt("ws_import_selected")(ws, d$inspection_id, lapply(d$blocks, function(b) b$id), "Keep all", qt("settings_defaults")())
  expect_match(qt("json_str")(ws$project$segments), "ADMIN_SECRET_91", fixed = TRUE)
})

test_that("an invalid selection is atomic", {
  ws <- new_ws()
  qt("ws_load_text")(ws, "P: Existing", "Existing"); before <- ws$project
  d <- qt("ws_inspect_document")(ws, doc_text)
  for (selected in list(list(), list("B0001", "B0001"), list("B9999"))) {
    expect_error(qt("ws_import_selected")(ws, d$inspection_id, selected, "New", qt("settings_defaults")()), class = "qt_workflow_error")
    expect_identical(ws$project, before)
  }
})

test_that("an expired scan cannot replace work", {
  ws <- new_ws()
  qt("ws_load_text")(ws, "P: Existing", "Existing"); before <- ws$project
  d <- qt("ws_inspect_document")(ws, doc_text); qt("ws_inspect_document")(ws, "P: Another")
  expect_error(qt("ws_import_selected")(ws, d$inspection_id, list("B0001"), "New", qt("settings_defaults")()), "expired", class = "qt_workflow_error")
  expect_identical(ws$project, before)
})

test_that("plain narrative is not removed and spoken markers stay", {
  d <- qt("scan_document")("I had pain after surgery.\n\nIt slowly improved, but I was unsure.")
  expect_false(any(vapply(d$blocks, function(b) b$suggested_exclude, logical(1))))
  d <- qt("scan_document")("Participant A: [00:12:04] [pause] I do not know.\nIt is hard to explain.")
  expect_identical(d$suspected, 0L)
  expect_match(d$blocks[[1]]$text, "[pause]", fixed = TRUE)
})

test_that("Word tables are suggested separately", {
  data <- fixture_docx(word_body(paste0("<w:p><w:r><w:t>Participant: My pain improved.</w:t></w:r></w:p>",
                                        "<w:tbl><w:tr><w:tc><w:p><w:r><w:t>Age</w:t></w:r></w:p></w:tc><w:tc><w:p><w:r><w:t>42</w:t></w:r></w:p></w:tc></w:tr></w:tbl>")))
  doc <- qt("read_document")("interview.docx", data)
  d <- qt("scan_document")(doc$text, doc$blocks)
  last <- d$blocks[[length(d$blocks)]]
  expect_identical(last$kind, "table"); expect_true(last$suggested_exclude)
  expect_false(d$blocks[[1]]$suggested_exclude)
})

test_that("a Word question-answer table can remain interview", {
  data <- fixture_docx(word_body("<w:tbl><w:tr><w:tc><w:p><w:r><w:t>Participant A</w:t></w:r></w:p></w:tc><w:tc><w:p><w:r><w:t>It was difficult.</w:t></w:r></w:p></w:tc></w:tr></w:tbl>"))
  doc <- qt("read_document")("interview.docx", data)
  expect_identical(doc$blocks[[1]]$kind, "dialogue_table")
  d <- qt("scan_document")(doc$text, doc$blocks)
  expect_identical(d$suggested_exclusions, 0L)
  expect_match(doc$text, "Participant A: It was difficult.", fixed = TRUE)
})

test_that("document decisions survive a project round trip", {
  ws <- new_ws()
  d <- qt("ws_inspect_document")(ws, doc_text)
  qt("ws_import_selected")(ws, d$inspection_id, lapply(Filter(function(b) !b$needs_check, d$blocks), function(b) b$id), "Interview", qt("settings_defaults")())
  restored <- new_ws()
  qt("ws_import_project")(restored, qt("json_parse")(qt("json_str")(ws$project)))
  expect_true(qt("json_equal")(restored$project$source_document$blocks, ws$project$source_document$blocks))
})

test_that("the demonstration has distinct severity examples", {
  ws <- completed_demo()
  expect_identical(vapply(ws$project$segments, qt("severity"), ""), c("none", "none", "none", "critical", "major", "minor"))
})

test_that("minor batch approval is explicit and separate from individual review", {
  ws <- completed_demo()
  qt("ws_review_batch_minor")(ws, list("S0006"), "AY", ws$project$revision)
  review <- ws$project$segments[[6]]$review
  expect_identical(review$method, "batch_minor"); expect_match(review$note, "explicit user-selected batch", fixed = TRUE)
  expect_null(ws$project$segments[[4]]$review); expect_null(ws$project$segments[[5]]$review)
  expect_match(qt("methods_record")(ws$project), '"batch_minor": 1', fixed = TRUE)
  expect_match(rawToChar(qt("export_csv")(ws$project)), "review_method", fixed = TRUE)
  restored <- new_ws()
  qt("ws_import_project")(restored, qt("json_parse")(qt("json_str")(ws$project)))
  expect_identical(restored$project$segments[[6]]$review$method, "batch_minor")
})

test_that("a mixed-severity batch is rejected without partial approval", {
  ws <- completed_demo()
  for (ids in list(list("S0006", "S0005"), list("S0006", "S0004"), list("S0006", "S0001"))) {
    before <- ws$project
    expect_error(qt("ws_review_batch_minor")(ws, ids, "AY", ws$project$revision), class = "qt_workflow_error")
    expect_identical(ws$project, before)
  }
})

test_that("a stale batch and a blank reviewer are rejected", {
  ws <- completed_demo()
  expect_error(qt("ws_review_batch_minor")(ws, list("S0006"), "AY", -1L), "project changed", class = "qt_workflow_error")
  expect_error(qt("ws_review_batch_minor")(ws, list("S0006"), "", ws$project$revision), "Reviewer", class = "qt_workflow_error")
  expect_null(ws$project$segments[[6]]$review)
})

test_that("quick approval is only for low-severity unchanged drafts", {
  ws <- completed_demo()
  for (i in c(4, 5)) {
    seg <- ws$project$segments[[i]]
    expect_error(qt("ws_review")(ws, seg$id, seg$reconcile$text, "AY", "Accepted", method = "quick_accept"), class = "qt_workflow_error")
  }
  seg <- ws$project$segments[[6]]
  qt("ws_review")(ws, "S0006", seg$reconcile$text, "AY", "Accepted", method = "quick_accept")
  expect_identical(ws$project$segments[[6]]$review$method, "quick_accept")
})

test_that("a decision can return to pending with history", {
  ws <- completed_demo()
  qt("ws_review_batch_minor")(ws, list("S0006"), "AY", ws$project$revision)
  qt("ws_clear_review")(ws, "S0006")
  expect_null(ws$project$segments[[6]]$review)
  expect_identical(ws$project$audit[[length(ws$project$audit)]]$action, "review_returned_to_pending")
})

terms_ws <- function() {
  ws <- new_ws()
  qt("ws_load_text")(ws, "Participant: The fixator was removed.", "Terms")
  settings <- qt("settings_defaults")()
  settings$terminology <- list(list(source = "fixator", target = "external fixator", alternatives = list("frame"), notes = "Team-agreed device term", required = TRUE))
  settings$reporting$dialect_context <- "DIALECT_SENTINEL_92"
  qt("ws_save_settings")(ws, settings)
  ws
}

apply_text <- function(ws, stage, text) manual_answer(ws, stage, function(id) text)

test_that("a required terminology mismatch adds a major local flag", {
  ws <- terms_ws()
  apply_text(ws, "forward", "The cast was removed."); apply_text(ws, "back", "The cast was removed."); apply_text(ws, "reconcile", "The cast was removed.")
  issues <- ws$project$segments[[1]]$reconcile$issues
  expect_identical(issues[[1]]$severity, "major"); expect_identical(issues[[1]]$origin, "local_glossary_check")
  run <- ws$project$runs[[3]]
  expect_identical(run$parsed_results[[1]]$issues[[1]]$origin, "local_glossary_check")
  restored <- new_ws()
  qt("ws_import_project")(restored, qt("json_parse")(qt("json_str")(ws$project)))
  expect_identical(restored$project$segments[[1]]$reconcile$issues[[1]]$origin, "local_glossary_check")
  expect_error(qt("ws_review")(ws, "S0001", "The cast was removed.", "AY", ""), class = "qt_workflow_error")
})

test_that("an accepted alternative does not get a mismatch flag", {
  ws <- terms_ws()
  apply_text(ws, "forward", "The frame was removed."); apply_text(ws, "back", "The frame was removed."); apply_text(ws, "reconcile", "The frame was removed.")
  expect_length(ws$project$segments[[1]]$reconcile$issues, 0)
})

test_that("glossary and dialect are excluded from the blind back-translation", {
  ws <- terms_ws()
  apply_text(ws, "forward", "The frame was removed.")
  task <- qt("ws_prepare_task")(ws, "back")
  for (secret in c("DIALECT_SENTINEL_92", "Team-agreed", "terminology_lexicon")) expect_false(grepl(secret, task$user, fixed = TRUE))
})

test_that("reporting changes keep outputs but dialect changes invalidate them", {
  ws <- terms_ws()
  apply_text(ws, "forward", "The frame was removed.")
  s <- ws$project$settings; s$reporting$reviewer_background <- "Bilingual researcher"
  qt("ws_save_settings")(ws, s)
  expect_false(is.null(ws$project$segments[[1]]$forward))
  s$reporting$dialect_context <- "Different dialect"
  qt("ws_save_settings")(ws, s)
  expect_null(ws$project$segments[[1]]$forward)
})

capture <- function(reply) {
  client <- qt("new_api_client")()
  client$payloads <- list()
  client$request <- function(provider, key, workspace_id, path, payload = NULL) {
    client$payloads[[length(client$payloads) + 1L]] <- payload
    list(result = reply, request_id = "request-fixture")
  }
  client
}

test_that("Mistral sampling is sent and defaults are omitted", {
  ws <- terms_ws()
  settings <- qt("settings_defaults")()
  settings$stages$forward$provider <- "mistral"; settings$stages$forward$model <- "fixture"
  settings$stages$forward$temperature <- 0.2; settings$stages$forward$random_seed <- 42L
  qt("ws_save_settings")(ws, settings)
  client <- capture(list(model = "fixture", choices = list(list(finish_reason = "stop", message = list(content = "{}")))))
  client$complete(qt("ws_prepare_task")(ws, "forward"), "synthetic-key")
  payload <- client$payloads[[1]]
  expect_identical(payload$temperature, 0.2); expect_identical(payload$random_seed, 42L); expect_null(payload$top_p)
  expect_match(qt("json_str")(payload), '"temperature":0.2', fixed = TRUE)
  settings$stages$forward["temperature"] <- list(NULL); settings$stages$forward["random_seed"] <- list(NULL)
  qt("ws_save_settings")(ws, settings)
  client$complete(qt("ws_prepare_task")(ws, "forward"), "synthetic-key")
  expect_null(client$payloads[[2]]$temperature); expect_null(client$payloads[[2]]$random_seed)
})

test_that("invalid sampling and unsupported controls fail early", {
  bad <- list(list("temperature", TRUE), list("top_p", 0L), list("random_seed", -1L), list("temperature", NaN), list("temperature", 1.5),
              list("random_seed", 4.5))
  for (case in bad) {
    s <- qt("settings_defaults")(); s$stages$forward[[case[[1]]]] <- case[[2]]
    expect_error(qt("validate_settings")(s), class = "qt_workflow_error")
  }
  s <- qt("settings_defaults")(); s$stages$forward$provider <- "anthropic"; s$stages$forward$random_seed <- 3L
  expect_error(qt("validate_settings")(s), "random seed", class = "qt_workflow_error")
  for (case in list(c("anthropic", "none"), c("mistral", "max"), c("copilot", "high"), c("manual", "low"))) {
    s <- qt("settings_defaults")(); s$stages$forward$provider <- case[1]; s$stages$forward$effort <- case[2]
    expect_error(qt("validate_settings")(s), "Reasoning effort", class = "qt_workflow_error")
  }
  s <- qt("settings_defaults")(); s$stages$forward$provider <- "copilot"; s$stages$forward$temperature <- 0.3; s$stages$forward$random_seed <- 4L
  expect_null(qt("validate_settings")(s)$stages$forward$temperature)
  s <- qt("settings_defaults")(); s$stages$forward$provider <- "mistral"; s$stages$forward$temperature <- 0.2; s$stages$forward$top_p <- 0.8
  expect_error(qt("validate_settings")(s), "temperature or top-p", class = "qt_workflow_error")
  s <- qt("settings_defaults")(); s$batch_chars <- 5000
  expect_error(qt("validate_settings")(s), "batch_chars", class = "qt_workflow_error")
  s <- qt("settings_defaults")(); s$target_language <- "danish "
  expect_error(qt("validate_settings")(s), "different source and target", class = "qt_workflow_error")
})

test_that("a chat tool choice is saved before any translation", {
  ws <- terms_ws()
  settings <- qt("settings_defaults")()
  for (st in names(settings$stages)) { settings$stages[[st]]$chat_tool <- "Institutional Copilot"; settings$stages[[st]]$chat_model <- "" }
  qt("ws_save_settings")(ws, settings)
  restored <- new_ws()
  qt("ws_import_project")(restored, qt("json_parse")(qt("json_str")(ws$project)))
  expect_length(restored$project$runs, 0)
  expect_length(restored$keys, 0)
  for (config in restored$project$settings$stages) {
    expect_identical(config$provider, "manual"); expect_identical(config$chat_tool, "Institutional Copilot")
  }
})

test_that("the manual packet contains the instructions and the selected text", {
  ws <- terms_ws()
  task <- qt("ws_prepare_task")(ws, "forward")
  packet <- paste0(task$system, "\n\nINPUT JSON:\n", task$user)
  expect_match(packet, "TASK: FORWARD TRANSLATION", fixed = TRUE)
  expect_match(packet, "The fixator was removed.", fixed = TRUE)
  expect_match(packet, "S0001", fixed = TRUE)
  expect_length(ws$keys, 0)
})
