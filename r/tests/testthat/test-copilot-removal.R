# The automatic Copilot browser mode was removed (Microsoft's terms forbid data extraction from
# its online services). Copilot can still be used by copy and paste, and projects made with the
# browser mode of earlier versions must still import and export with their records intact.
# Mirrors CopilotRemovalTests in python/tests/test_model_settings.py.

# Mistral API stages with a key, answered by the fake client.
mistral_stages <- function(ws, batch_segments = 12L) {
  settings <- ws$project$settings
  settings$batch_segments <- batch_segments
  for (st in names(settings$stages)) settings$stages[[st]] <- list(provider = "mistral", model = "fixture-model", workspace_id = "")
  qt("ws_save_settings")(ws, settings)
  qt("ws_set_key")(ws, "mistral", "NEVER_EXPORT_THIS_KEY")
}

# A project in the shape saved by version 1.4: Copilot stages and settings, Copilot run records,
# a human entry for a section Copilot declined, and the refusal events in the audit.
old_copilot_project <- function(refusals = TRUE) {
  ws <- qt("new_workspace")(fake_client())
  qt("ws_load_text")(ws, "P: one\nP: two\nP: three", "Old Copilot project")
  mistral_stages(ws, batch_segments = 1L)
  qt("ws_edit")(ws, "S0002", "forward", "Human forward two", "Fictional reviewer; Copilot declined")
  start_and_run(ws, "all")
  expect_identical(ws$job$error, "")
  project <- qt("json_parse")(qt("json_str")(ws$project))
  for (st in names(project$settings$stages)) {
    project$settings$stages[[st]]$provider <- "copilot"
    project$settings$stages[[st]]$chat_tool <- "Institutional Copilot"
    project$settings$stages[[st]]$model <- ""
  }
  project$settings$copilot <- list(temporary_chat = TRUE)
  for (i in seq_along(project$runs)) {
    project$runs[[i]]$request_configuration$provider <- "copilot"
    project$runs[[i]]$request_configuration$sampling_application <- "external_chat_not_controlled"
    project$runs[[i]]$transport_prompt <- paste0("VISIBLE_UI_PACKET ", project$runs[[i]]$id)
    project$runs[[i]]$provenance <- list(
      provider = "copilot_browser", model = "not reported by Copilot Chat", integration = "visible_web_ui",
      chat_isolation = "new_temporary_chat_verified", receipt_verified = TRUE, copilot_model_selector_label = "Auto",
      temporary_chat_notice = "Temporary chat notice", browser = "Chrome/fixture", submitted_at = "2026-10-27T10:00:00+00:00",
      completed_at = "2026-10-27T10:01:00+00:00", unexpected_field = "dropped on import")
  }
  if (refusals) {
    n <- length(project$audit)
    project$audit[[n + 1L]] <- list(at = "t", action = "copilot_declined",
                                    details = list(stage = "forward", segment_ids = list("S0002"), context_included = TRUE, reply_excerpt = "Sorry"))
    project$audit[[n + 2L]] <- list(at = "t", action = "copilot_declined",
                                    details = list(stage = "forward", segment_ids = list("S0002"), context_included = FALSE, reply_excerpt = "Sorry"))
    project$audit[[n + 3L]] <- list(at = "t", action = "copilot_section_declined", details = list(stage = "forward", segment_id = "S0002"))
  }
  qt("json_parse")(qt("json_str")(project))
}

bundle_files <- function(bytes) {
  zipfile <- tempfile(fileext = ".zip"); dir <- tempfile("bundle-")
  on.exit(unlink(c(zipfile, dir), recursive = TRUE))
  writeBin(bytes, zipfile)
  zip::unzip(zipfile, exdir = dir)
  files <- sort(list.files(dir))
  out <- lapply(files, function(f) qt("read_utf8_file")(file.path(dir, f)))
  names(out) <- files
  out
}

test_that("a copilot stage becomes a copy-and-paste stage and the copilot settings key is ignored", {
  s <- qt("settings_defaults")()
  expect_null(s$copilot)
  s$stages$forward <- list(provider = "copilot", chat_tool = "", chat_model = "Auto", model = "m", workspace_id = "w",
                           temperature = 0.3, top_p = NULL, random_seed = 4L, effort = "high")
  s$stages$back <- list(provider = "copilot", chat_tool = "Institutional Copilot", temperature = 1.5, effort = "max")
  s$stages$reconcile <- list(provider = "copilot")
  s$copilot <- list(temporary_chat = "not even a boolean")
  out <- qt("validate_settings")(s)
  expect_null(out$copilot)
  expect_false("copilot" %in% names(out))
  forward <- out$stages$forward
  expect_identical(forward$provider, "manual"); expect_identical(forward$chat_tool, "Microsoft Copilot")
  expect_identical(forward$chat_model, "Auto"); expect_identical(forward$model, ""); expect_identical(forward$workspace_id, "")
  expect_null(forward$temperature); expect_null(forward$top_p); expect_null(forward$random_seed); expect_identical(forward$effort, "")
  expect_true(all(c("temperature", "top_p", "random_seed") %in% names(forward)))
  expect_identical(out$stages$back$provider, "manual"); expect_identical(out$stages$back$chat_tool, "Institutional Copilot")
  expect_null(out$stages$back$temperature); expect_identical(out$stages$back$effort, "")
  expect_identical(out$stages$reconcile$chat_tool, "Microsoft Copilot")
  expect_setequal(names(out$stages$forward), names(qt("settings_defaults")()$stages$forward))
  # Only "copilot" is converted; other unknown providers are still refused.
  s$stages$reconcile <- list(provider = "copilot_browser")
  expect_error(qt("validate_settings")(s), "Invalid provider for reconcile", class = "qt_workflow_error")
  # Saving the converted settings again is not a change, and an old copilot key changes nothing.
  ws <- qt("new_workspace")(fake_client()); qt("ws_load_text")(ws, "P: one")
  qt("ws_save_settings")(ws, out)
  revision <- ws$project$revision
  again <- out; again$copilot <- list(temporary_chat = FALSE)
  qt("ws_save_settings")(ws, again)
  expect_identical(ws$project$revision, revision)
  expect_false("copilot" %in% names(ws$project$settings))
  expect_error(qt("ws_start_job")(ws, "all"), "Configure API providers", class = "qt_workflow_error")
})

test_that("a project saved by the Python version imports, exports and does not mention Copilot", {
  path <- test_path("fixtures", "python_project.json")
  value <- qt("json_parse")(qt("read_utf8_file")(path))
  expect_false(is.null(value$settings$copilot))        # saved by a version that still had the key
  ws <- qt("new_workspace")(fake_client())
  qt("ws_import_project")(ws, value)
  expect_false("copilot" %in% names(ws$project$settings))
  files <- bundle_files(qt("export_bundle")(ws$project))
  expect_identical(names(files), c("audit.json", "methods_record.txt", "parallel_review.html", "project.json",
                                   "prompts_and_runs.json", "translations.csv"))
  record <- files[["methods_record.txt"]]
  expect_match(record, "API calls are stateless; each batch is a separate request.", fixed = TRUE)
  expect_false(grepl("Copilot", record, fixed = TRUE))
  expect_false(grepl("Copilot", qt("methods_paragraph")(ws$project), fixed = TRUE))
  expect_false("copilot" %in% names(qt("json_parse")(files[["project.json"]])$settings))
})

test_that("a project made with Copilot browser mode imports with its records and exports", {
  ws <- qt("new_workspace")(fake_client())
  qt("ws_import_project")(ws, old_copilot_project())
  for (config in ws$project$settings$stages) {
    expect_identical(config$provider, "manual"); expect_identical(config$chat_tool, "Institutional Copilot")
  }
  expect_false("copilot" %in% names(ws$project$settings))
  run <- ws$project$runs[[1]]
  expect_identical(run$transport_prompt, paste0("VISIBLE_UI_PACKET ", run$id))
  expect_identical(run$request_configuration$provider, "copilot")
  expect_setequal(names(run$provenance), c("provider", "model", "integration", "chat_isolation", "receipt_verified",
                                           "copilot_model_selector_label", "temporary_chat_notice", "browser",
                                           "submitted_at", "completed_at"))
  expect_identical(ws$project$segments[[2]]$forward$human_entry$notes, "Fictional reviewer; Copilot declined")
  paragraph <- qt("methods_paragraph")(ws$project)
  expect_match(paragraph, "Microsoft 365 Copilot Chat under the institution's licence, operated automatically through its web interface, with each batch in a new temporary chat (Copilot does not report its underlying model; its mode selector showed 'Auto')", fixed = TRUE)
  files <- bundle_files(qt("export_bundle")(ws$project))
  expect_length(files, 6)
  record <- files[["methods_record.txt"]]
  for (line in qt("COPILOT_HISTORY_LINES")) expect_match(record, line, fixed = TRUE)
  expect_match(record, "API calls are stateless; each batch is a separate request.\nSome runs in this project were made with the Copilot browser mode", fixed = TRUE)
  expect_match(record, "Copilot refusals: 1 section/stage pair(s) Copilot would not translate after isolation: S0002 (forward); 2 declined batch attempt(s)", fixed = TRUE)
  expect_match(record, 'Human-entered results (recorded as human work, not model output): {"forward": ["S0002"], "back": [], "reconcile": []}.', fixed = TRUE)
  # Exported and imported again, the refusal events sit one level deeper (prior_audit) and still count.
  again <- qt("new_workspace")(fake_client())
  qt("ws_import_project")(again, qt("json_parse")(files[["project.json"]]))
  record2 <- qt("methods_record")(again$project)
  expect_match(record2, "Copilot refusals: 1 section/stage pair(s)", fixed = TRUE)
  expect_match(record2, qt("COPILOT_HISTORY_LINES")[1], fixed = TRUE)
})

test_that("Copilot lines appear only for Copilot runs and recorded refusals", {
  ws <- qt("new_workspace")(fake_client())
  qt("ws_import_project")(ws, old_copilot_project(refusals = FALSE))
  record <- qt("methods_record")(ws$project)
  expect_match(record, qt("COPILOT_HISTORY_LINES")[4], fixed = TRUE)
  expect_false(grepl("Copilot refusals", record, fixed = TRUE))
  # Refusal events without Copilot runs: the refusal line only.
  project <- old_copilot_project()
  for (i in seq_along(project$runs)) project$runs[[i]]$provenance$provider <- "manual"
  qt("ws_import_project")(ws, project)
  record <- qt("methods_record")(ws$project)
  expect_false(grepl("Copilot browser mode", record, fixed = TRUE))
  expect_match(record, "Copilot refusals: 1 section/stage pair(s)", fixed = TRUE)
  # A project run here with an API: no Copilot wording anywhere in the methods record.
  fresh <- qt("new_workspace")(fake_client())
  qt("ws_load_text")(fresh, "P: one\nP: two", "Fixture"); mistral_stages(fresh)
  start_and_run(fresh, "all")
  record <- qt("methods_record")(fresh$project)
  expect_false(grepl("Copilot", record, fixed = TRUE))
  expect_match(record, "Forward translation, blind back-translation, and reconciliation are separate operations.\nAPI calls are stateless; each batch is a separate request.\nBack-translation requests contain only", fixed = TRUE)
})

test_that("the Copilot routes are gone and the state has no copilot entry", {
  ws <- qt("new_workspace")(fake_client())
  srv <- start_test_server(ws); on.exit(httpuv::stopServer(srv$server))
  for (route in c("open", "check", "cancel", "close", "forget")) {
    res <- api(srv, paste0("copilot/", route), qt("empty_obj")())
    expect_identical(res$status, 404L, info = route)
    expect_identical(res$json, list(error = "Unknown endpoint."), info = route)
  }
  expect_identical(api(srv, "demo", qt("empty_obj")())$status, 200L)
  state <- api(srv, "state")$json
  expect_setequal(names(state), c("project", "job", "identifier_check", "credentials"))
  expect_null(state$copilot)
  expect_setequal(names(state$job), c("running", "message", "error", "error_code", "done", "total", "stage", "batch", "batches"))
  expect_false("copilot" %in% names(state$project$settings))
  # An API call with an old copilot stage is accepted and stored as copy and paste.
  settings <- state$project$settings
  settings$stages$forward$provider <- "copilot"; settings$stages$forward$chat_tool <- ""
  settings$copilot <- list(temporary_chat = TRUE)
  expect_identical(api(srv, "settings", list(settings = settings))$status, 200L)
  stored <- api(srv, "state")$json$project$settings
  expect_identical(stored$stages$forward$provider, "manual")
  expect_identical(stored$stages$forward$chat_tool, "Microsoft Copilot")
  expect_false("copilot" %in% names(stored))
})
