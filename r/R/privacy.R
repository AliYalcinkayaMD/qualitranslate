#' @include utils.R
NULL

# Local check for common identifier patterns before text is sent anywhere.
#
# It only warns. It never removes or changes text, and it cannot find names, places or
# other indirect identifiers, so it does not replace pseudonymisation by the research team.

ID_KINDS <- list(email = c("e-mail address", "e-mail addresses"),
                 url = c("web address", "web addresses"),
                 cpr = c("CPR-like number (DDMMYY-XXXX)", "CPR-like numbers (DDMMYY-XXXX)"),
                 phone = c("phone-like number", "phone-like numbers"),
                 date = c("full date (could be a birth date)", "full dates (could be birth dates)"))

id_phrase <- function(kind, n) paste0(n, " ", ID_KINDS[[kind]][if (n == 1) 1L else 2L])

# Python's re module treats \w and \d as Unicode classes; (*UCP) does the same in PCRE.
RX_EMAIL <- "(*UCP)[\\w.+-]+@[\\w-]+(?:\\.[\\w-]+)+"
RX_URL <- paste0("(*UCP)\\b(?:https?://|www\\.)[^<>\"'", substring(WS, 2, nchar(WS) - 1L), "]+")
RX_CPR <- "(*UCP)(?<![\\d:])([0-3]\\d)([01]\\d)(\\d{2})[- ]?(\\d{4})(?![\\d:])"
RX_PHONE <- "(*UCP)(?<![\\w:.])(?:\\+\\d{1,3}[ -]?)?\\(?\\d{2,4}\\)?(?:[ -]?\\d{2,4}){1,4}(?![\\w:])"
RX_DATE <- "(*UCP)(?<![\\d:])(?:[0-3]?\\d[./-][01]?\\d[./-](?:19|20)\\d{2}|(?:19|20)\\d{2}-[01]\\d-[0-3]\\d)(?![\\d:])"

rx_matches <- function(pattern, text, ignore.case = FALSE) {
  m <- gregexpr(pattern, text, perl = TRUE, ignore.case = ignore.case)[[1]]
  if (m[1] == -1L) return(list())
  starts <- as.integer(m); lens <- attr(m, "match.length")
  cs <- attr(m, "capture.start"); cl <- attr(m, "capture.length")
  lapply(seq_along(starts), function(i) {
    groups <- if (!is.null(cs)) substring(text, cs[i, ], cs[i, ] + cl[i, ] - 1L) else character()
    list(start = starts[i] - 1L, end = starts[i] - 1L + lens[i],
         text = substr(text, starts[i], starts[i] + lens[i] - 1L), groups = groups)
  })
}

# Returns a list of list(kind, start, end) matches in one piece of text.
find_identifiers <- function(text) {
  found <- list(); taken_a <- numeric(); taken_b <- numeric()
  free <- function(a, b) all(b <= taken_a | a >= taken_b)
  add <- function(kind, m) {
    if (free(m$start, m$end)) {
      found[[length(found) + 1L]] <<- list(kind = kind, start = m$start, end = m$end)
      taken_a <<- c(taken_a, m$start); taken_b <<- c(taken_b, m$end)
    }
  }
  for (m in rx_matches(RX_EMAIL, text)) add("email", m)
  for (m in rx_matches(RX_URL, text, ignore.case = TRUE)) add("url", m)
  for (m in rx_matches(RX_CPR, text)) {
    day <- int_from_digits(m$groups[1]); month <- int_from_digits(m$groups[2])
    if (day >= 1 && day <= 31 && month >= 1 && month <= 12) add("cpr", m)
  }
  for (m in rx_matches(RX_DATE, text)) add("date", m)
  for (m in rx_matches(RX_PHONE, text)) {
    digits <- lengths(regmatches(m$text, gregexpr("(*UCP)\\d", m$text, perl = TRUE)))
    if (digits >= 8 && digits <= 15) add("phone", m)
  }
  if (!length(found)) return(found)
  found[order(vapply(found, function(f) f$start, numeric(1)), method = "radix")]
}

identifier_scan <- function(segments) {
  counts <- integer(); rows <- list()
  for (seg in segments) {
    hits <- find_identifiers(seg$source %||% "")
    kinds <- vapply(hits, function(h) h$kind, character(1))
    for (k in kinds) counts[k] <- if (k %in% names(counts)) counts[[k]] + 1L else 1L
    if (length(kinds)) {
      rows[[length(rows) + 1L]] <- list(id = seg$id, kinds = as.list(sort(unique(kinds), method = "radix")))
    }
  }
  count_obj <- if (length(counts)) as.list(counts) else empty_obj()
  labels <- if (length(counts)) lapply(ID_KINDS[names(counts)], function(k) k[1]) else empty_obj()
  ks <- if (length(counts)) sort(names(counts), method = "radix") else character()
  phrases <- lapply(ks, function(k) id_phrase(k, counts[[k]]))
  list(total = as.integer(sum(counts)), counts = count_obj, segments = rows, labels = labels, phrases = phrases)
}

identifier_summary <- function(scan) {
  if (!scan$total) return("none of the checked patterns found")
  ids <- vapply(scan$segments, function(r) r$id, character(1))
  paste0(paste(unlist(scan$phrases), collapse = ", "), " in ", length(scan$segments), " section(s): ",
         paste(utils::head(ids, 40), collapse = ", "), if (length(ids) > 40) "\u2026" else "")
}
