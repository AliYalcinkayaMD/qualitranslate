# Port of python/tests/test_model_settings.py: per-stage model options, a separate
# back-translation model, the identifier check and the draft methods paragraph.

capture_reply <- function(reply) {
  client <- qt("new_api_client")()
  client$payloads <- list()
  client$request <- function(provider, key, workspace_id, path, payload = NULL) {
    client$payloads[[length(client$payloads) + 1L]] <- payload
    list(result = reply, request_id = "rid")
  }
  client
}

option_task <- function(...) {
  ws <- qt("new_workspace")(fake_client()); qt("ws_load_text")(ws, "P: one")
  settings <- qt("settings_defaults")()
  settings$stages$forward$model <- "m"
  extra <- list(...)
  for (nm in names(extra)) settings$stages$forward[nm] <- list(extra[[nm]])
  qt("ws_save_settings")(ws, settings)
  qt("ws_prepare_task")(ws, "forward")
}

test_that("Claude sends temperature and effort only when set, and never a seed", {
  client <- capture_reply(list(content = list(list(type = "text", text = "{}")), stop_reason = "end_turn"))
  client$complete(option_task(provider = "anthropic"), "k")
  last <- client$payloads[[1]]
  expect_null(last$temperature); expect_null(last$output_config)
  client$complete(option_task(provider = "anthropic", temperature = 0.2, effort = "high"), "k")
  last <- client$payloads[[2]]
  expect_identical(last$temperature, 0.2); expect_identical(last$output_config, list(effort = "high"))
  expect_null(last$random_seed)
})

test_that("Mistral reasoning effort is sent only when chosen", {
  client <- capture_reply(list(choices = list(list(finish_reason = "stop", message = list(content = "{}")))))
  client$complete(option_task(provider = "mistral", effort = "high", top_p = 0.9), "k")
  expect_identical(client$payloads[[1]]$reasoning_effort, "high"); expect_identical(client$payloads[[1]]$top_p, 0.9)
  client$complete(option_task(provider = "mistral"), "k")
  expect_null(client$payloads[[2]]$reasoning_effort)
})

test_that("old projects without effort still load", {
  s <- qt("settings_defaults")()
  for (st in names(s$stages)) s$stages[[st]]$effort <- NULL
  expect_identical(qt("validate_settings")(s)$stages$back$effort, "")
})

mistral_all <- function(ws, model = "model-a") {
  settings <- qt("settings_defaults")()
  for (st in names(settings$stages)) { settings$stages[[st]]$provider <- "mistral"; settings$stages[[st]]$model <- model }
  settings
}

test_that("changing only the back-translation model keeps forward work", {
  ws <- qt("new_workspace")(fake_client()); qt("ws_load_text")(ws, "P: one\nP: two")
  qt("ws_save_settings")(ws, mistral_all(ws)); ws$keys$mistral <- "k"
  start_and_run(ws, "all")
  expect_true(all(vapply(ws$project$segments, function(s) !is.null(s$reconcile), logical(1))))
  changed <- ws$project$settings; changed$stages$back$model <- "model-b"
  qt("ws_save_settings")(ws, changed)
  expect_true(all(vapply(ws$project$segments, function(s) !is.null(s$forward), logical(1))))
  expect_true(all(vapply(ws$project$segments, function(s) is.null(s$back) && is.null(s$reconcile), logical(1))))
  expect_identical(ws$project$audit[[length(ws$project$audit)]]$details$invalidated_from_stage, "back")
  changed2 <- ws$project$settings; changed2$target_language <- "German"
  qt("ws_save_settings")(ws, changed2)
  expect_true(all(vapply(ws$project$segments, function(s) is.null(s$forward), logical(1))))
})

test_that("an equal value of another JSON number type is not a change", {
  ws <- qt("new_workspace")(fake_client()); qt("ws_load_text")(ws, "P: one")
  settings <- mistral_all(ws); settings$stages$forward$temperature <- 1
  qt("ws_save_settings")(ws, settings); ws$keys$mistral <- "k"
  start_and_run(ws, "forward")
  revision <- ws$project$revision
  same <- ws$project$settings; same$stages$forward$temperature <- 1L
  qt("ws_save_settings")(ws, same)
  expect_identical(ws$project$revision, revision)
  expect_false(is.null(ws$project$segments[[1]]$forward))
})

test_that("the paragraph names a different back-translation model", {
  ws <- qt("new_workspace")(fake_client()); qt("ws_load_text")(ws, "P: one")
  settings <- mistral_all(ws); settings$stages$back$model <- "model-b"; settings$stages$reconcile$effort <- "high"
  qt("ws_save_settings")(ws, settings); ws$keys$mistral <- "k"
  start_and_run(ws, "all")
  text <- qt("methods_paragraph")(ws$project)
  expect_match(text, "back into Danish with the Mistral API", fixed = TRUE)
  expect_match(text, "reasoning effort high", fixed = TRUE)
  expect_match(text, "[Human review not yet recorded", fixed = TRUE)
  expect_match(qt("methods_record")(ws$project), "DRAFT METHODS PARAGRAPH", fixed = TRUE)
})

test_that("identifier patterns flag common identifiers but not timestamps", {
  kinds <- function(t) vapply(qt("find_identifiers")(t), function(h) h$kind, "")
  expect_identical(kinds("Mail anna@example.dk or +45 12 34 56 78"), c("email", "phone"))
  expect_identical(kinds("CPR 010203-1234"), "cpr")
  expect_identical(kinds("[inaudible 00:12:31] in 2019, 45 minutes"), character())
  expect_identical(kinds("born 12.03.1985, see www.example.org"), c("date", "url"))
  # Unicode digits count, as with Python's \d.
  arabic <- intToUtf8(0x0660 + c(1, 2, 3, 4, 5, 6, 7, 8))
  expect_identical(kinds(paste("call", arabic)), "phone")
})

test_that("the identifier scan is a warning and never changes text", {
  ws <- qt("new_workspace")(fake_client()); qt("ws_load_text")(ws, "P: call me on 12345678\nP: fine")
  before <- qt("json_str")(ws$project$segments)
  check <- qt("ws_snapshot")(ws)$identifier_check
  expect_identical(check$segments, list(list(id = "S0001", kinds = list("phone"))))
  expect_identical(qt("json_str")(ws$project$segments), before)
  expect_match(qt("methods_record")(ws$project), "1 phone-like number in 1 section(s): S0001", fixed = TRUE)
  expect_identical(qt("identifier_scan")(list(list(id = "S1", source = "nothing here")))$total, 0L)
  expect_identical(qt("json_str")(qt("identifier_scan")(list(list(id = "S1", source = "x")))),
                   '{"total":0,"counts":{},"segments":[],"labels":{},"phrases":[]}')
})

test_that("identifier phrases are plural aware", {
  scan <- qt("identifier_scan")(list(list(id = "S1", source = "a@b.dk c@d.dk 12345678")))
  expect_identical(scan$phrases, list("2 e-mail addresses", "1 phone-like number"))
})

test_that("the demo paragraph is labelled synthetic", {
  ws <- qt("new_workspace")(fake_client())
  qt("ws_load_text")(ws, qt("DEMO_SOURCE"), "demo", demo = TRUE)
  start_and_run(ws, "all", demo = TRUE)
  expect_true(startsWith(qt("methods_paragraph")(ws$project), "[SYNTHETIC DEMONSTRATION"))
  expect_gt(length(qt("export_bundle")(ws$project)), 0)
})

# ----- ParagraphAccuracyTests: statements must match the results currently in the project.

api_workspace <- function(text = "P: one\nP: two") {
  ws <- qt("new_workspace")(fake_client()); qt("ws_load_text")(ws, text)
  ws$keys$mistral <- "k"; ws$keys$anthropic <- "k"
  ws
}

test_that("superseded runs are not credited", {
  ws <- api_workspace(); settings <- qt("settings_defaults")()
  for (st in names(settings$stages)) { settings$stages[[st]]$provider <- "anthropic"; settings$stages[[st]]$model <- "claude-a" }
  qt("ws_save_settings")(ws, settings); start_and_run(ws, "all")
  changed <- ws$project$settings; changed$stages$back$provider <- "mistral"; changed$stages$back$model <- "mistral-b"
  qt("ws_save_settings")(ws, changed)
  start_and_run(ws, "all")
  text <- qt("methods_paragraph")(ws$project)
  expect_match(text, "back into Danish with the Mistral API, model mistral-b", fixed = TRUE)
  expect_false(grepl("claude-a mistral-b", text, fixed = TRUE)); expect_false(grepl("model claude-a / mistral-b", text, fixed = TRUE))
  expect_match(text, "the forward-translation tool compared", fixed = TRUE)
})

test_that("stages not run are not described as done", {
  ws <- api_workspace(); settings <- qt("settings_defaults")()
  for (st in names(settings$stages)) { settings$stages[[st]]$provider <- "mistral"; settings$stages[[st]]$model <- "m" }
  qt("ws_save_settings")(ws, settings); start_and_run(ws, "forward")
  text <- qt("methods_paragraph")(ws$project)
  expect_match(text, "[Blind back-translation not yet run.]", fixed = TRUE)
  expect_match(text, "[Reconciliation not yet run.]", fixed = TRUE)
  expect_identical(qt("methods_paragraph")(qt("new_workspace")(fake_client())$project), "[No transcript has been prepared yet.]")
})

test_that("batch acceptance is not called review", {
  ws <- api_workspace(); settings <- qt("settings_defaults")()
  for (st in names(settings$stages)) { settings$stages[[st]]$provider <- "mistral"; settings$stages[[st]]$model <- "m" }
  qt("ws_save_settings")(ws, settings); start_and_run(ws, "all")
  for (i in seq_along(ws$project$segments)) {
    ws$project$segments[[i]]$reconcile$issues <- list(list(category = "other", severity = "minor", detail = "x", needs_human = TRUE, origin = "model"))
  }
  qt("ws_review_batch_minor")(ws, list("S0001"), "Reviewer one", ws$project$revision)
  qt("ws_review_batch_minor")(ws, list("S0002"), "Reviewer two", ws$project$revision)
  text <- qt("methods_paragraph")(ws$project)
  expect_match(text, "by 2 reviewers", fixed = TRUE); expect_match(text, "without individual review", fixed = TRUE)
  expect_false(grepl("compared the proposed translations", text, fixed = TRUE))
})

test_that("mixed temporary chats and odd imported values do not break the paragraph or exports", {
  ws <- api_workspace("P: one")
  project <- ws$project
  project$segments[[1]]$forward <- list(text = "x", notes = list(), run_id = "r1")
  project$runs <- list(list(id = "r1", stage = "forward", segment_ids = list("S0001"), prompt_version = "1.1.0",
                            request_configuration = list(provider = "copilot", model = NULL),
                            provenance = list(provider = "copilot_browser", chat_isolation = "new_empty_chat_verified", copilot_model_selector_label = 7L)))
  text <- qt("methods_paragraph")(project)
  expect_match(text, "with each batch in a new chat (Copilot", fixed = TRUE)
  expect_match(text, "prompt version 1.1.0", fixed = TRUE)
  restored <- qt("new_workspace")(fake_client())
  qt("ws_import_project")(restored, qt("json_parse")(qt("json_str")(project)))
  expect_gt(length(qt("export_bundle")(restored$project)), 0)
})

test_that("a project saved by the Python version imports and exports", {
  path <- test_path("fixtures", "python_project.json")
  skip_if_not(file.exists(path))
  ws <- qt("new_workspace")(fake_client())
  qt("ws_import_project")(ws, qt("json_parse")(qt("read_utf8_file")(path)))
  expect_identical(ws$project$segments[[4]]$review$reviewer, "Fictional reviewer")
  expect_match(qt("methods_paragraph")(ws$project), "1 after individual review", fixed = TRUE)
  expect_identical(ws$project$audit[[1]]$action, "project_imported")
  expect_gt(length(ws$project$audit[[1]]$details$prior_audit), 0)
  expect_gt(length(qt("export_bundle")(ws$project)), 0)
})
