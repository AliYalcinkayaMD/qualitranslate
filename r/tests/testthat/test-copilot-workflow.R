# Port of python/tests/test_copilot_workflow.py: workflow rules around Copilot browser mode
# with a scripted stand-in for Copilot (no browser needed).

test_that("a declined batch is split and neighbours are retried without context", {
  text <- paste(sprintf("P: line %d%s", 1:8, ifelse(1:8 == 4, " BLOCKED_WORD", "")), collapse = "\n")
  copilot <- scripted_copilot(); ws <- copilot_workspace(copilot, text)
  start_and_run(ws, "all")
  expect_match(ws$job$error, "S0004", fixed = TRUE); expect_match(ws$job$error, "forward translation", fixed = TRUE)
  expect_identical(vapply(ws$job$declined, function(d) d$id, ""), "S0004")
  expect_identical(vapply(Filter(function(s) is.null(s$forward), ws$project$segments), function(s) s$id, ""), "S0004")
  retried <- Filter(function(r) isTRUE(r$context_omitted), ws$project$runs)
  retried_ids <- vapply(retried, function(r) paste(unlist(r$segment_ids), collapse = ","), "")
  expect_true(all(c("S0003", "S0005") %in% retried_ids))
  expect_false(any(vapply(ws$project$runs, function(r) grepl("BLOCKED_WORD", r$user_prompt, fixed = TRUE), logical(1))))
  expect_true(any(vapply(ws$project$audit, function(e) e$action == "copilot_declined", logical(1))))
  # The marker is never altered or removed from the participant's own text.
  blocked <- Filter(function(r) grepl("BLOCKED_WORD", r$user, fixed = TRUE), copilot$requests)
  expect_gt(length(blocked), 0)
  expect_true(all(vapply(blocked, function(r) grepl("line 4 BLOCKED_WORD", r$user, fixed = TRUE), logical(1))))
  expect_identical(ws$job$done, ws$job$total)
})

test_that("human entries complete declined sections and are recorded", {
  copilot <- scripted_copilot(); ws <- copilot_workspace(copilot, "P: one\nP: two BLOCKED_WORD\nP: three")
  start_and_run(ws, "all")
  expect_error(qt("ws_edit")(ws, "S0002", "back", "x", "n"), "earlier stage", class = "qt_workflow_error")
  qt("ws_edit")(ws, "S0002", "forward", "Human forward two", "Fictional reviewer, bilingual, Copilot declined")
  expect_identical(ws$project$segments[[2]]$forward$run_id, "human_entry")
  expect_false(is.null(ws$project$segments[[1]]$forward))
  start_and_run(ws, "all")
  expect_match(ws$job$error, "reconciliation", fixed = TRUE)   # the original still contains the word
  qt("ws_edit")(ws, "S0002", "reconcile", "Human proposal two", "Fictional reviewer")
  expect_true(all(vapply(ws$project$segments, function(s) !is.null(s$reconcile), logical(1))))
  expect_error(qt("ws_edit")(ws, "S0002", "reconcile", "again", ""), "human review", class = "qt_workflow_error")
  record <- qt("methods_record")(ws$project)
  expect_match(record, '"forward": ["S0002"]', fixed = TRUE)
  expect_match(record, "S0002 (forward)", fixed = TRUE)
  expect_match(record, "text was never rephrased", fixed = TRUE)
  restored <- qt("new_workspace")(fake_client(), scripted_copilot())
  qt("ws_import_project")(restored, qt("json_parse")(qt("json_str")(ws$project)))
  expect_false(is.null(restored$project$segments[[2]]$forward$human_entry))
  expect_gt(length(qt("export_bundle")(ws$project)), 0)
})

test_that("other browser errors stop without splitting", {
  copilot <- scripted_copilot()
  copilot$complete <- function(box, cancel = NULL) {
    copilot$requests[[length(copilot$requests) + 1L]] <- box$task
    qt("browser_error")("Copilot reports a usage limit.", "usage_limit")
  }
  ws <- copilot_workspace(copilot, "P: one\nP: two")
  start_and_run(ws, "all")
  expect_match(ws$job$error, "usage limit", fixed = TRUE)
  expect_identical(ws$job$error_code, "usage_limit")
  expect_length(copilot$requests, 1)
})

test_that("an unexpected error is reported without internal details", {
  copilot <- scripted_copilot()
  copilot$complete <- function(box, cancel = NULL) stop("SECRET internal detail")
  ws <- copilot_workspace(copilot, "P: one")
  start_and_run(ws, "forward")
  expect_identical(ws$job$error, "An unexpected error occurred. Completed batches were retained.")
  expect_identical(ws$job$message, "Stopped")
})

test_that("the declined list does not follow into another project", {
  ws <- copilot_workspace(scripted_copilot(), "P: one\nP: two BLOCKED_WORD")
  start_and_run(ws, "all")
  expect_identical(vapply(ws$job$declined, function(d) d$id, ""), "S0002")
  qt("ws_load_text")(ws, "Q: alpha\nQ: beta", "Another project")
  expect_length(ws$job$declined, 0)
  ws2 <- copilot_workspace(scripted_copilot(), "P: one\nP: two BLOCKED_WORD")
  start_and_run(ws2, "all")
  saved <- qt("json_parse")(qt("json_str")(ws2$project))
  qt("ws_import_project")(ws2, saved)
  expect_length(ws2$job$declined, 0)
})

test_that("refusal and human-entry counts are per section and survive import", {
  ws <- copilot_workspace(scripted_copilot(), "P: one\nP: two BLOCKED_WORD\nP: three")
  settings <- ws$project$settings; settings$batch_segments <- 1L
  qt("ws_save_settings")(ws, settings)
  start_and_run(ws, "all")
  expect_identical(vapply(Filter(function(s) is.null(s$forward), ws$project$segments), function(s) s$id, ""), "S0002")
  expect_identical(ws$job$done, ws$job$total)
  qt("ws_edit")(ws, "S0002", "forward", "Human two", "Fictional reviewer")
  start_and_run(ws, "all")
  qt("ws_edit")(ws, "S0002", "reconcile", "Human proposal two", "Fictional reviewer")
  record <- qt("methods_record")(ws$project)
  expect_match(record, "2 section/stage pair(s)", fixed = TRUE)
  expect_match(record, "S0002 (forward)", fixed = TRUE); expect_match(record, "S0002 (reconcile)", fixed = TRUE)
  expect_match(record, '"reconcile": ["S0002"]', fixed = TRUE); expect_match(record, '"forward": ["S0002"]', fixed = TRUE)
  restored <- qt("new_workspace")(fake_client(), scripted_copilot())
  qt("ws_import_project")(restored, qt("json_parse")(qt("json_str")(ws$project)))
  expect_match(qt("methods_record")(restored$project), "2 section/stage pair(s)", fixed = TRUE)
  csv <- rawToChar(qt("export_csv")(ws$project)); Encoding(csv) <- "UTF-8"
  first_line <- strsplit(csv, "\r\n", fixed = TRUE)[[1]][1]
  expect_match(first_line, "human_entered_stages", fixed = TRUE)
  expect_match(csv, "forward;reconcile", fixed = TRUE)
  expect_match(rawToChar(qt("export_html")(ws$project)), "[HUMAN-ENTERED] Human two", fixed = TRUE)
})

test_that("the temporary-chat setting is validated, passed on, and does not clear work", {
  expect_true(qt("settings_defaults")()$copilot$temporary_chat)
  bad <- qt("settings_defaults")(); bad$copilot <- list(temporary_chat = "yes")
  expect_error(qt("validate_settings")(bad), "Invalid Copilot settings", class = "qt_workflow_error")
  legacy <- qt("settings_defaults")(); legacy$copilot <- NULL
  expect_true(qt("validate_settings")(legacy)$copilot$temporary_chat)
  copilot <- scripted_copilot(); ws <- copilot_workspace(copilot, "P: one")
  start_and_run(ws, "forward")
  settings <- ws$project$settings; settings$copilot$temporary_chat <- FALSE
  qt("ws_save_settings")(ws, settings)
  expect_false(is.null(ws$project$segments[[1]]$forward))
  start_and_run(ws, "back")
  expect_identical(copilot$temporary_values, c(TRUE, FALSE))
})

test_that("a reply that arrives after Stop is kept and nothing further is sent", {
  holder <- new.env()
  copilot <- scripted_copilot(on_complete = function(task) holder$ws$cancel$set())
  ws <- copilot_workspace(copilot, "P: one\nP: two"); holder$ws <- ws
  settings <- ws$project$settings; settings$batch_segments <- 1L
  qt("ws_save_settings")(ws, settings)
  start_and_run(ws, "all")
  expect_match(ws$job$error, "Stopped after the current request", fixed = TRUE)
  expect_length(copilot$requests, 1)
  expect_false(is.null(ws$project$segments[[1]]$forward)); expect_null(ws$project$segments[[2]]$forward)
})

test_that("job progress names the stage and the batch", {
  seen <- list(); holder <- new.env()
  copilot <- scripted_copilot(on_complete = function(task) seen[[length(seen) + 1L]] <<- holder$ws$job)
  ws <- copilot_workspace(copilot, "P: one\nP: two"); holder$ws <- ws
  start_and_run(ws, "all")
  expect_true(startsWith(seen[[1]]$message, "Forward translation \u00b7 batch 1 of 1"))
  expect_identical(seen[[2]]$stage, "back")
})

test_that("the transport keeps what was typed and the run records it", {
  copilot <- scripted_copilot(); ws <- copilot_workspace(copilot, "Interviewer: ORIGINAL_SOURCE_91\nParticipant: Less pain.")
  start_and_run(ws, "all")
  expect_identical(ws$job$error, "")
  expect_identical(vapply(copilot$requests, function(r) r$stage, ""), c("forward", "back", "reconcile"))
  expect_false(grepl("ORIGINAL_SOURCE_91", copilot$requests[[2]]$user, fixed = TRUE))
  expect_true(all(vapply(ws$project$runs, function(r) startsWith(r$transport_prompt, "VISIBLE_UI_PACKET"), logical(1))))
  expect_true(all(vapply(ws$project$runs, function(r) r$request_configuration$sampling_application == "external_chat_not_controlled", logical(1))))
  imported <- qt("new_workspace")(fake_client(), scripted_copilot())
  qt("ws_import_project")(imported, qt("json_parse")(qt("json_str")(ws$project)))
  expect_identical(imported$project$runs[[1]]$transport_prompt, ws$project$runs[[1]]$transport_prompt)
  expect_true(imported$project$runs[[1]]$provenance$receipt_verified)
})

test_that("a browser failure keeps completed work and resumes without repeating forward", {
  copilot <- scripted_copilot(); failed <- FALSE
  plain <- copilot$complete
  copilot$complete <- function(box, cancel = NULL) {
    if (box$task$stage == "back" && !failed) {
      failed <<- TRUE
      qt("browser_error")("Fixture browser interruption")
    }
    plain(box, cancel)
  }
  ws <- copilot_workspace(copilot, "P: Original source")
  start_and_run(ws, "all")
  expect_match(ws$job$error, "browser interruption", fixed = TRUE)
  expect_length(ws$project$runs, 1)
  start_and_run(ws, "all")
  expect_identical(ws$job$error, "")
  expect_identical(vapply(copilot$requests, function(r) r$stage, ""), c("forward", "back", "reconcile"))
})

test_that("Copilot mode needs a verified connection and runs without API keys", {
  copilot <- scripted_copilot(); copilot$ready <- FALSE
  ws <- copilot_workspace(copilot, "P: one")
  ws$copilot_connection$phase <- "idle"
  expect_error(qt("ws_start_job")(ws, "all"), "Open Copilot", class = "qt_workflow_error")
  copilot$ready <- TRUE
  start_and_run(ws, "all")
  expect_identical(ws$job$error, ""); expect_length(ws$keys, 0); expect_length(ws$client$requests, 0)
})

test_that("a stale project or revision cannot start an automatic translation", {
  for (replace_project in c(FALSE, TRUE)) {
    copilot <- scripted_copilot(); ws <- copilot_workspace(copilot, "P: Fictional original")
    pid <- ws$project$project_id; revision <- ws$project$revision
    if (replace_project) {
      qt("ws_load_text")(ws, "P: Different source", "Different project")
    } else {
      settings <- ws$project$settings; settings$study_context <- "Changed while connecting"
      qt("ws_save_settings")(ws, settings)
    }
    expect_error(qt("ws_start_job")(ws, "all", expected_project_id = pid, expected_revision = revision), "project changed", class = "qt_workflow_error")
    expect_length(copilot$requests, 0); expect_false(ws$job$running)
  }
})

test_that("editor zero-width marks do not count as changes", {
  packet <- 'Line one\n\nLine "two" {json}'
  expect_identical(qt("normalise")(paste0(packet, "\u200b\u200c")), qt("normalise")(packet))
  expect_false(identical(qt("normalise")(substr(packet, 1, nchar(packet) - 3)), qt("normalise")(packet)))
  # ZWNJ inside the text is meaningful; only the trailing editor artefact is ignored.
  expect_false(identical(qt("normalise")("a\u200cb"), qt("normalise")("ab")))
  expect_identical(qt("normalise")("a\u200bb\u2060"), "ab")
})

test_that("a receipt reply inside a Copilot code block is found", {
  reply <- 'Copilot said:\nJSON\n{"qt_receipt":"QT_RECEIPT_x","segments":[{"id":"S0001","text":"He said \\"normal\\" {braces}","notes":[]}]}'
  found <- qt("extract_receipt_json")(reply, "QT_RECEIPT_x")
  expect_identical(qt("json_parse")(found)$segments[[1]]$text, 'He said "normal" {braces}')
})

test_that("an input echo without the matching receipt is not a response", {
  text <- 'TASK\n{"segments":[{"id":"S0001","text":"Original"}]}\nAdd a root qt_receipt field equal to QT_RECEIPT_EXPECTED.'
  expect_null(qt("extract_receipt_json")(text, "QT_RECEIPT_EXPECTED"))
  bad <- qt("json_str")(list(qt_receipt = "another request", segments = list()))
  good <- qt("json_str")(list(qt_receipt = "expected", segments = list(list(id = "S0001", text = paste0("Transl", intToUtf8(0xe6), "ted")))))
  expect_identical(qt("extract_receipt_json")(paste0(bad, "\n```json\n", good, "\n```"), "expected"), good)
  expect_null(qt("extract_receipt_json")(substr(good, 1, nchar(good) - 2), "expected"))
})
