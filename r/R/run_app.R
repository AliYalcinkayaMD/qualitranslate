#' Start the QualiTranslate workspace
#'
#' Starts the local QualiTranslate server on `127.0.0.1` and, by default, opens its
#' interface in the web browser. The workspace guides forward translation, blind
#' back-translation, reconciliation and named human review of an interview transcript,
#' and exports the results with a methods record.
#'
#' The function blocks while the app runs, like `shiny::runApp()`. Stop it with Escape
#' (RStudio) or Control+C. On exit it stops the server, cancels a running translation
#' job, closes the Copilot window if the app opened or used one, and forgets any API keys
#' entered during the session. The project itself stays only in memory: export it from
#' the interface before stopping.
#'
#' Text is sent only to the provider chosen for each stage: none in the manual
#' copy-and-paste mode, the Mistral or Anthropic API in API mode, or the user's own
#' institutional Microsoft 365 Copilot Chat in Copilot mode. Copilot mode uses a
#' dedicated Chrome or Edge profile in `tools::R_user_dir("qualitranslate", "data")`,
#' created only when the user clicks Connect.
#'
#' @param port Port to listen on. If it is busy, the next ten ports are tried. Use `0` for
#'   a random free port.
#' @param launch_browser Open the interface in the default web browser.
#' @return Invisibly, the address the app was served at (after the app has stopped), or
#'   `NULL` if no port was free.
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
#' @export
run_app <- function(port = 8765, launch_browser = interactive()) {
  if (!is.numeric(port) || length(port) != 1L || is.na(port) || port != round(port) || port < 0 || port > 65535) {
    stop("Port must be between 0 and 65535.", call. = FALSE)
  }
  port <- as.integer(port)
  ws <- new_workspace()
  token <- new_token()
  candidates <- if (port == 0L) httpuv::randomPort() else port + 0:10
  candidates <- candidates[candidates <= 65535L]
  server <- NULL; bound <- NULL
  for (p in candidates) {
    server <- tryCatch(qt_start_server(ws, token, p), error = function(e) NULL)
    if (!is.null(server)) {
      bound <- p
      break
    }
    other <- running_version(p)
    if (nzchar(other)) {
      message("Note: QualiTranslate ", other, " is already running at http://127.0.0.1:", p,
              " (probably in another R session or Terminal window). This new copy will use a different address.",
              if (other != APP_VERSION) " Close the older window when you no longer need it, so you do not use the old version by mistake." else "")
    }
  }
  if (is.null(server)) {
    message("No free local port was found. Close other QualiTranslate windows and try again.")
    return(invisible(NULL))
  }
  url <- paste0("http://127.0.0.1:", bound)
  message("QualiTranslate ", APP_VERSION, "\nOpen ", url,
          "\nKeep this R session open. Press Escape (RStudio) or Control+C to stop.",
          "\nProject data stays in memory until you explicitly export it. API mode sends selected text to the chosen provider.")
  if (isTRUE(launch_browser)) later::later(function() utils::browseURL(url), 0.7)
  on.exit({
    ws$cancel$set()
    ws$pending_job <- NULL
    ws$copilot_connection$cancel()
    try(ws$copilot$close(), silent = TRUE)
    ws$keys <- list()
    httpuv::stopServer(server)
  }, add = TRUE)
  tryCatch({
    repeat {
      # A failure in one step must not take the server down with the user's unsaved project.
      tryCatch(qt_loop_once(ws, 0.1), error = function(e) {
        message("QualiTranslate: ", conditionMessage(e))
      })
    }
  }, interrupt = function(e) {
    message("\nStopping. In-memory project and credentials will be cleared.")
  })
  invisible(url)
}
