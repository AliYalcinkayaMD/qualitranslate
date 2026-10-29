# JSON reading and writing with the same data model as the Python implementation.

json_parse <- function(text) {
  # R strings cannot hold a null character and jsonlite would silently cut a string at an
  # escaped \u0000, so such JSON is refused (Python rejects these texts as well).
  if (grepl("(?<!\\\\)(?:\\\\\\\\)*\\\\u0000", text, perl = TRUE)) workflow_error("The text contains a null character.")
  jsonlite::parse_json(text, simplifyVector = FALSE)
}

# Compact JSON. Doubles keep a decimal point (1.0) so that Python reads them back as floats,
# and integers stay integers, so files move between the R and Python versions unchanged.
json_str <- function(x, pretty = FALSE) {
  out <- jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", digits = NA,
                          always_decimal = TRUE, pretty = pretty)
  enc2utf8(as.character(out))
}

# json.dumps(x, ensure_ascii=False, indent=2): jsonlite's pretty layout is identical.
json_pretty <- function(x) json_str(x, pretty = TRUE)

# json.dumps(x) with Python's default separators (", " and ": ").
py_dumps <- function(x) {
  s <- json_str(x)
  m <- gregexpr('"(?:[^"\\\\]|\\\\.)*"', s, perl = TRUE, useBytes = TRUE)[[1]]
  bytes <- charToRaw(s)
  space <- function(seg) gsub(":", ": ", gsub(",", ", ", seg, fixed = TRUE), fixed = TRUE)
  if (m[1] == -1L) return(space(s))
  starts <- as.integer(m); ends <- starts + attr(m, "match.length") - 1L
  pieces <- character()
  pos <- 1L
  for (i in seq_along(starts)) {
    if (starts[i] > pos) pieces <- c(pieces, space(rawToChar(bytes[pos:(starts[i] - 1L)])))
    pieces <- c(pieces, rawToChar(bytes[starts[i]:ends[i]]))
    pos <- ends[i] + 1L
  }
  if (pos <= length(bytes)) pieces <- c(pieces, space(rawToChar(bytes[pos:length(bytes)])))
  out <- paste(pieces, collapse = "")
  Encoding(out) <- "UTF-8"
  out
}

sort_keys <- function(x) {
  if (!is.list(x)) return(x)
  nm <- names(x)
  out <- lapply(x, sort_keys)
  if (!is.null(nm)) {
    o <- order(nm, method = "radix")
    out <- out[o]
    names(out) <- nm[o]
    if (!length(out)) out <- empty_obj()
  }
  out
}

# Canonical sorted-key JSON for hashing (the bytes need not match Python's exactly).
canonical <- function(x) json_str(sort_keys(x))

sha256_hex <- function(text) {
  h <- openssl::sha256(charToRaw(enc2utf8(text)))
  paste(sprintf("%02x", as.integer(unclass(h))), collapse = "")
}

digest <- function(x) sha256_hex(canonical(x))

read_utf8_file <- function(path) {
  s <- readChar(path, file.size(path), useBytes = TRUE)
  Encoding(s) <- "UTF-8"
  s
}

raw_to_utf8 <- function(bytes) {
  if (!length(bytes)) return("")
  s <- rawToChar(bytes)
  Encoding(s) <- "UTF-8"
  s
}
