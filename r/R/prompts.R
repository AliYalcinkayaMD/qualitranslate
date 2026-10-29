# Versioned prompts and other shared assets. They live in inst/ (copied from the
# repository's shared/ folder) and are read at run time, so both implementations send
# identical instructions and serve the same interface.

asset_path <- function(...) {
  path <- system.file(..., package = "qualitranslate")
  if (!nzchar(path)) stop("QualiTranslate asset not found: ", file.path(...), call. = FALSE)
  path
}

asset_text <- function(...) read_utf8_file(asset_path(...))

prompt_data <- function() {
  path <- asset_path("prompts.json")
  stamp <- paste(file.mtime(path), file.size(path))
  if (!identical(.qt$prompt_stamp, stamp)) {
    data <- json_parse(read_utf8_file(path))
    .qt$prompt_cache <- list(
      version = data$prompt_version,
      prompts = list(forward = paste0(data$common, data$forward),
                     back = paste0(data$common, data$back),
                     reconcile = paste0(data$common, data$reconcile)),
      categories = unlist(data$categories),
      reply_format = data$reply_format)
    .qt$prompt_stamp <- stamp
  }
  .qt$prompt_cache
}

prompt_version <- function() prompt_data()$version

#' Show the translation instructions
#'
#' Prints the versioned system prompts that QualiTranslate sends for forward
#' translation, blind back-translation and reconciliation. The same text is used by
#' the copy-and-paste workflow and the API providers.
#'
#' @param stage Which prompt to show: `"all"` (the default), `"forward"`, `"back"`
#'   or `"reconcile"`.
#' @return Invisibly, a named character vector with the selected prompts.
#' @examples
#' show_prompts("back")
#' @export
show_prompts <- function(stage = c("all", "forward", "back", "reconcile")) {
  stage <- match.arg(stage)
  data <- prompt_data()
  chosen <- if (stage == "all") STAGES else stage
  out <- vapply(chosen, function(s) data$prompts[[s]], character(1))
  for (s in chosen) {
    cat("== ", STAGE_LABELS[[s]], " (prompt version ", data$version, ") ==\n", sep = "")
    cat(out[[s]], "\n", sep = "")
  }
  invisible(out)
}
