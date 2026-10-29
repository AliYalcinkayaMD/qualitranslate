#' @include utils.R
NULL

# Local document reading and conservative interview-boundary suggestions. No model calls.

SCAN_VERSION <- "1.0.0"
RX_META <- paste0("(*UCP)^(?:interview", WS, "*(?:date|id|number|location|duration)|date|project|study", WS,
                  "*(?:id|title)|file", WS, "*name|transcriber|location|country|site|recording", WS,
                  "*(?:id|date)|duration|page|participant", WS, "*(?:id|code|details)|demographic", WS,
                  "*(?:data|details))", WS, "*:")
RX_SECTION <- paste0("(*UCP)^(?:appendix|references|demographics|participant characteristics|coding", WS,
                     "*(?:table|notes|framework)|codebook|researcher notes|interview guide|instructions for|administrative|consent form|end of (?:the )?(?:interview|transcript))\\b")
RX_SPEAKER <- paste0("(*UCP)^", WS, "*([^:\\n]{1,60}):", WS, "*[^\t\n\v\f\r\u001c-\u001f \u0085\u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000]")
RX_KNOWN_SPEAKER <- "(*UCP)^(?:interviewer|participant|respondent|moderator|patient|speaker|parent|caregiver|interviewee|p\\d+|r\\d+|i\\d*|f\\d+)(?:[ _-]*[\\w-]{1,20})?\\z"
RX_PAGE <- paste0("(*UCP)^(?:page", WS, "*)?\\d+", WS, "*(?:of|/)", WS, "*\\d+\\z")

rx_match <- function(pattern, x) grepl(pattern, x, perl = TRUE, ignore.case = TRUE)

normalise_document_text <- function(value) {
  if (!is_str(value) || !nzchar(py_strip(value)) || is.na(nchar_py(value)) || nchar_py(value) > MAX_TEXT) {
    workflow_error("Document text is empty, invalid, or too large.")
  }
  value <- gsub("\r\n", "\n", value, fixed = TRUE)
  value <- gsub("\r", "\n", value, fixed = TRUE)
  sub("^\ufeff+", "", value, perl = TRUE)
}

# base64.b64decode(value, validate=True)
b64_strict <- function(value) {
  if (!is_str(value) || !grepl("^[A-Za-z0-9+/]*={0,2}$", value, perl = TRUE) || nchar(value) %% 4L != 0L) {
    return(NULL)
  }
  if (!nzchar(value)) return(raw(0))
  tryCatch(jsonlite::base64_dec(value), error = function(e) NULL)
}

read_document <- function(filename, encoded) {
  if (!is_str(filename) || nchar_py(filename) > 240) workflow_error("Invalid filename.")
  raw <- b64_strict(encoded)
  if (is.null(raw)) workflow_error("Invalid upload encoding.")
  if (length(raw) > 8000000) workflow_error("File exceeds the 8 MB limit.")
  lower <- tolower(filename)
  if (endsWith(lower, ".txt") || endsWith(lower, ".md")) {
    if (length(raw) >= 3 && identical(raw[1:3], as.raw(c(0xef, 0xbb, 0xbf)))) raw <- raw[-(1:3)]
    probe <- raw
    probe[probe == as.raw(0)] <- as.raw(0x20)   # a null byte is valid UTF-8; it is rejected below
    if (length(probe) && !validUTF8(rawToChar(probe))) workflow_error("Text files must use UTF-8 encoding.")
    if (any(raw == as.raw(0))) workflow_error("Document text is empty, invalid, or too large.")
    return(list(text = normalise_document_text(raw_to_utf8(raw)), blocks = list()))
  }
  if (!endsWith(lower, ".docx")) workflow_error("Supported transcript files: .txt, .md, .docx")
  cannot <- "Cannot read this DOCX. Save a checked copy as UTF-8 TXT."
  archive <- tempfile(fileext = ".docx")
  extract <- tempfile("qt-docx-")
  on.exit(unlink(c(archive, extract), recursive = TRUE, force = TRUE), add = TRUE)
  writeBin(raw, archive)
  entries <- tryCatch(zip::zip_list(archive), error = function(e) NULL)
  if (is.null(entries)) workflow_error(cannot)
  if (nrow(entries) > 2000) workflow_error("The document has too many archive entries.")
  idx <- which(entries$filename == "word/document.xml")
  if (!length(idx)) workflow_error(cannot)
  idx <- idx[length(idx)]
  if (entries$uncompressed_size[idx] > 5000000) workflow_error("Document text is too large.")
  xml <- tryCatch({
    zip::unzip(archive, files = "word/document.xml", exdir = extract)
    path <- file.path(extract, "word", "document.xml")
    if (file.size(path) > 5000000) stop("too large")
    readBin(path, "raw", file.size(path))
  }, error = function(e) NULL)
  if (is.null(xml)) workflow_error(cannot)
  if (length(grepRaw("<!DOCTYPE", xml, ignore.case = TRUE)) || length(grepRaw("<!ENTITY", xml, ignore.case = TRUE))) {
    workflow_error("XML entity declarations are not supported.")
  }
  parsed <- tryCatch(docx_blocks(xml), error = function(e) {
    if (inherits(e, "qt_workflow_error")) stop(e)
    workflow_error(cannot)
  })
  texts <- vapply(parsed, function(b) b$text, character(1))
  list(text = normalise_document_text(paste(texts, collapse = "\n\n")), blocks = parsed)
}

docx_blocks <- function(xml) {
  ns <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")
  # NONET only: no network access, no entity substitution, and blank text is kept.
  doc <- xml2::read_xml(xml, options = "NONET")
  body <- xml2::xml_find_first(doc, "/*/w:body", ns)
  if (inherits(body, "xml_missing")) workflow_error("The Word document has no main body.")
  para <- function(p) {
    nodes <- xml2::xml_find_all(p, "descendant-or-self::*[self::w:t or self::w:tab or self::w:br or self::w:cr]", ns)
    if (!length(nodes)) return("")
    nm <- xml2::xml_name(nodes)
    parts <- ifelse(nm == "t", xml2::xml_text(nodes), ifelse(nm == "tab", "\t", "\n"))
    paste(parts, collapse = "")
  }
  blocks <- list()
  for (element in xml2::xml_find_all(body, "./*[self::w:p or self::w:tbl]", ns)) {
    if (xml2::xml_name(element) == "p") {
      content <- py_strip(para(element))
      if (nzchar(content)) blocks[[length(blocks) + 1L]] <- list(text = content, kind = "paragraph")
    } else {
      rows <- character(); dialogue <- TRUE
      for (row in xml2::xml_find_all(element, "./w:tr", ns)) {
        cells <- vapply(xml2::xml_find_all(row, "./w:tc", ns), function(cell) {
          py_strip(paste(vapply(xml2::xml_find_all(cell, "./w:p", ns), para, character(1)), collapse = " "))
        }, character(1))
        label <- if (length(cells)) sub(":+$", "", cells[1]) else ""
        if (length(cells) == 2L && rx_match(RX_KNOWN_SPEAKER, label)) {
          rows <- c(rows, paste0(label, ": ", cells[2]))
        } else {
          dialogue <- FALSE
          rows <- c(rows, paste(cells, collapse = "\t"))
        }
      }
      content <- py_strip(paste(rows, collapse = "\n"))
      if (nzchar(content)) {
        blocks[[length(blocks) + 1L]] <- list(text = content, kind = if (dialogue) "dialogue_table" else "table")
      }
    }
  }
  blocks
}

scan_document <- function(text, structured_blocks = NULL) {
  text <- normalise_document_text(text)
  structured <- is_arr(structured_blocks) && length(structured_blocks) <= 10000 &&
    all(vapply(structured_blocks, function(b) {
      is_obj(b) && is_str(b$text) && is_str(b$kind) && b$kind %in% c("paragraph", "table", "dialogue_table")
    }, logical(1)))
  if (structured && !identical(paste(vapply(structured_blocks, function(b) b$text, ""), collapse = "\n\n"), text)) {
    structured <- FALSE
  }
  raw <- list()
  source_blocks <- if (structured) structured_blocks else list(list(text = text, kind = "paragraph"))
  for (block in source_blocks) {
    if (block$kind == "table") {
      raw[[length(raw) + 1L]] <- list(text = block$text, kind = "table")
      next
    }
    collected <- character()
    flush <- function() {
      if (length(collected)) {
        raw[[length(raw) + 1L]] <<- list(text = py_strip(paste(collected, collapse = "\n")), kind = "paragraph")
        collected <<- character()
      }
    }
    for (line in py_split(block$text, "\n")) {
      speaker <- rx_match(RX_SPEAKER, line)
      if (!nzchar(py_strip(line))) {
        flush()
      } else if (rx_match(RX_META, line) || rx_match(RX_SECTION, line) || rx_match(RX_PAGE, line)) {
        flush()
        raw[[length(raw) + 1L]] <- list(text = py_strip(line), kind = "paragraph")
      } else if (!speaker && (grepl("\t", line, fixed = TRUE) || lengths(regmatches(line, gregexpr("|", line, fixed = TRUE))) >= 2)) {
        flush()
        if (length(raw) && raw[[length(raw)]]$kind == "table") {
          raw[[length(raw)]]$text <- paste0(raw[[length(raw)]]$text, "\n", line)
        } else {
          raw[[length(raw) + 1L]] <- list(text = line, kind = "table")
        }
      } else if (speaker) {
        flush()
        collected <- c(collected, line)
      } else {
        collected <- c(collected, line)
      }
    }
    flush()
  }
  if (!length(raw) || length(raw) > 10000) workflow_error("Document must contain 1\u201310,000 blocks.")
  contents <- vapply(raw, function(b) b$text, character(1))
  is_speaker <- rx_match(RX_SPEAKER, contents)
  is_meta <- rx_match(RX_META, contents)
  is_section <- rx_match(RX_SECTION, contents)
  spoken <- which(is_speaker & !is_meta & !is_section)
  result <- vector("list", length(raw))
  trailing_section <- NULL
  for (i in seq_along(raw)) {
    content <- contents[i]; reason <- ""; exclude <- FALSE
    pipes <- lengths(regmatches(content, gregexpr("|", content, fixed = TRUE)))
    if (raw[[i]]$kind == "table" || (grepl("\t", content, fixed = TRUE) && !is_speaker[i]) ||
        (pipes >= 4 && grepl(paste0("\\|", WS, "*[-:]{2,}"), content, perl = TRUE))) {
      reason <- "Possible table or administrative data. Check whether this belongs to the interview."; exclude <- TRUE
    } else if (is_meta[i]) {
      reason <- "Possible document metadata rather than spoken interview content."; exclude <- TRUE
    } else if (is_section[i]) {
      reason <- "Possible heading, appendix, coding notes, or end marker."; exclude <- TRUE
    } else if (rx_match(RX_PAGE, content)) {
      reason <- "Possible page numbering."; exclude <- TRUE
    } else if (length(spoken) && i < spoken[1] && !is_speaker[i]) {
      reason <- "Text before the first speaker turn. Confirm whether this is interview content."
    } else if (!is.null(trailing_section) && length(spoken) && i > spoken[length(spoken)] && !is_speaker[i]) {
      reason <- "Text after a closing or appendix heading. Confirm whether it is part of the interview."
    }
    if (is_section[i] && length(spoken) && i > spoken[length(spoken)]) trailing_section <- i
    result[[i]] <- list(id = sprintf("B%04d", i), text = content, kind = raw[[i]]$kind, reason = reason,
                        suggested_exclude = exclude, needs_check = nzchar(reason))
  }
  list(scan_version = SCAN_VERSION, blocks = result,
       suspected = sum(vapply(result, function(b) b$needs_check, logical(1))),
       suggested_exclusions = sum(vapply(result, function(b) b$suggested_exclude, logical(1))))
}
