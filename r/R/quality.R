# Review priorities and exact, team-supplied terminology checks.

SEVERITY_RANK <- c(none = 0L, minor = 1L, major = 2L, critical = 3L)

severity <- function(segment) {
  issues <- oget(segment$reconcile, "issues", list())
  if (!length(issues)) return("none")
  sev <- vapply(issues, function(i) i$severity, character(1))
  sev[which.max(SEVERITY_RANK[sev])]
}

term_present <- function(text, term) {
  grepl(paste0("(*UCP)(?<!\\w)", rx_escape(term), "(?!\\w)"), text, perl = TRUE, ignore.case = TRUE)
}

terminology_issues <- function(source, target, terms) {
  issues <- list()
  for (t in terms) {
    if (term_present(source, t$source) &&
        !any(vapply(c(list(t$target), t$alternatives), function(x) term_present(target, x), logical(1)))) {
      issues[[length(issues) + 1L]] <- list(
        category = "terminology",
        severity = if (isTRUE(t$required)) "major" else "minor",
        detail = paste0("Terminology check: source contains \"", t$source, "\", but the preferred target \"",
                        t$target, "\"", if (length(t$alternatives)) " or an approved alternative" else "",
                        " was not found. Check context before changing the participant\u2019s words."),
        needs_human = TRUE, origin = "local_glossary_check")
    }
  }
  issues
}
