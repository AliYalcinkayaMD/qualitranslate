# Shared constants, error conditions and small helpers that mirror Python behaviour.

APP_VERSION <- "1.5.0"
STAGES <- c("forward", "back", "reconcile")
STAGE_LABELS <- c(forward = "Forward translation", back = "Blind back-translation",
                  reconcile = "Reconciliation")
MAX_TEXT <- 1500000
MAX_SEGMENTS <- 4000
PROVIDERS <- c("manual", "mistral", "anthropic")
# Reasoning effort is a per-model API option. Claude: output_config.effort (Opus 4.5 and later);
# Mistral: reasoning_effort on models with adjustable reasoning. "" means provider default.
EFFORT_LEVELS <- list(anthropic = c("low", "medium", "high", "xhigh", "max"),
                      mistral = c("none", "high"))

# Package-level state: the curl pool used for asynchronous requests, the prompt cache, and a
# guard that detects waits inside request handlers.
.qt <- new.env(parent = emptyenv())
.qt$in_handler <- FALSE

# Python's str.isspace() characters, used where the Python code calls strip() or uses \s.
# Written with real characters (not \x{...}) so that the pattern is UTF-8 and PCRE runs
# in UTF mode in every locale.
WS <- "[\t\n\v\f\r\u001c-\u001f \u0085\u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000]"

`%||%` <- function(a, b) if (is.null(a)) b else a

# ----- conditions -----------------------------------------------------------------------
# qt_workflow_error carries user-facing text (Python WorkflowError). qt_format_error stands
# for Python's TypeError/AttributeError on malformed input, which the server reports as
# "Invalid request format.".

qt_condition <- function(class, message, ...) {
  structure(class = c(class, "error", "condition"),
            list(message = message, call = NULL, ...))
}

workflow_error <- function(message) stop(qt_condition("qt_workflow_error", message))

format_error <- function() stop(qt_condition("qt_format_error", "Invalid request format."))

is_qt_message_error <- function(e) inherits(e, "qt_workflow_error")

# ----- JSON-shaped values -----------------------------------------------------------------
# Objects are named lists, arrays are unnamed lists, null is NULL and scalars are atomic
# vectors of length one. Arrays are never stored as atomic vectors.

empty_obj <- function() structure(list(), names = character(0))
is_obj <- function(x) is.list(x) && !is.null(names(x))
is_arr <- function(x) is.list(x) && is.null(names(x))
is_str <- function(x) is.character(x) && length(x) == 1L && !is.na(x)
is_int <- function(x) is.integer(x) && length(x) == 1L && !is.na(x)
is_num <- function(x) (is.integer(x) || is.double(x)) && length(x) == 1L && !is.na(x)
is_bool <- function(x) is.logical(x) && length(x) == 1L && !is.na(x)
is_true <- function(x) isTRUE(x) && is.logical(x)

has_key <- function(x, key) is_obj(x) && key %in% names(x)

# Python dict.get(): the stored value (even null) when the key exists, else the default.
# Calling .get on something that is not an object is a format error, as in Python.
jget <- function(x, key, default = NULL) {
  if (!is_obj(x)) format_error()
  if (key %in% names(x)) x[[key]] else default
}

# Same, but for values that may be objects or null (Python: (x or {}).get()).
oget <- function(x, key, default = NULL) {
  if (is_obj(x) && key %in% names(x)) x[[key]] else default
}

# Assign a value that may be NULL without deleting the key.
jset <- function(x, key, value) {
  x[key] <- list(value)
  x
}

# Python truthiness.
truthy <- function(x) {
  if (is.null(x)) return(FALSE)
  if (is.list(x)) return(length(x) > 0L)
  if (length(x) != 1L || is.na(x)) return(length(x) > 0L)
  if (is.logical(x)) return(x)
  if (is.numeric(x)) return(x != 0)
  if (is.character(x)) return(nzchar(x))
  TRUE
}

# Python equality for JSON values (dict comparison ignores key order; 1 == 1.0 == True).
json_equal <- function(a, b) {
  if (is.null(a) || is.null(b)) return(is.null(a) && is.null(b))
  if (is_obj(a) || is_obj(b)) {
    if (!(is_obj(a) && is_obj(b)) || length(a) != length(b)) return(FALSE)
    if (!setequal(names(a), names(b))) return(FALSE)
    for (k in names(a)) if (!json_equal(a[[k]], b[[k]])) return(FALSE)
    return(TRUE)
  }
  if (is.list(a) || is.list(b)) {
    if (!(is.list(a) && is.list(b)) || length(a) != length(b)) return(FALSE)
    for (i in seq_along(a)) if (!json_equal(a[[i]], b[[i]])) return(FALSE)
    return(TRUE)
  }
  if ((is.numeric(a) || is.logical(a)) && (is.numeric(b) || is.logical(b))) {
    return(length(a) == length(b) && isTRUE(all(as.numeric(a) == as.numeric(b))))
  }
  identical(a, b)
}

# ----- text helpers -------------------------------------------------------------------------

py_strip <- function(x) {
  x <- sub(paste0("^", WS, "+"), "", x, perl = TRUE)
  sub(paste0(WS, "+$"), "", x, perl = TRUE)
}
py_lstrip <- function(x) sub(paste0("^", WS, "+"), "", x, perl = TRUE)

# str.split(sep) that keeps empty pieces, including a trailing one.
py_split <- function(x, sep) {
  strsplit(paste0(x, sep), sep, fixed = TRUE)[[1]]
}

utf8 <- function(x) {
  x <- enc2utf8(as.character(x))
  x
}

nchar_py <- function(x) nchar(x, type = "chars", allowNA = TRUE)

substr_py <- function(x, start, stop) {
  # Python slice x[start:stop] with 0-based start and exclusive stop, for non-negative indexes.
  substr(x, start + 1L, stop)
}

startswith <- function(x, prefix) startsWith(x, prefix)

# Python casefold() is approximated by tolower() plus the common full foldings.
casefold_py <- function(x) {
  x <- tolower(x)
  gsub("\u00df", "ss", x, fixed = TRUE)
}

text_value <- function(value, label, maximum = MAX_TEXT, allow_empty = FALSE) {
  if (!is_str(value)) workflow_error(paste0(label, " is missing, invalid, or too long."))
  n <- nchar_py(value)
  if (is.na(n) || n > maximum || (!allow_empty && !nzchar(py_strip(value)))) {
    workflow_error(paste0(label, " is missing, invalid, or too long."))
  }
  # R strings cannot contain a null character; JSON with an escaped null is refused in json_parse().
  value
}

now_iso <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S+00:00", tz = "UTC")

qt_clock <- function() proc.time()[["elapsed"]]

random_hex <- function(n_bytes) paste(as.character(openssl::rand_bytes(n_bytes)), collapse = "")
uuid_hex <- function() random_hex(16)

# secrets.token_urlsafe(32)
new_token <- function() {
  b <- openssl::base64_encode(openssl::rand_bytes(32))
  b <- chartr("+/", "-_", b)
  sub("=+$", "", b)
}

# secrets.compare_digest
constant_time_equal <- function(a, b) {
  if (!is_str(a) || !is_str(b)) return(FALSE)
  x <- charToRaw(enc2utf8(a)); y <- charToRaw(enc2utf8(b))
  if (length(x) != length(y)) return(FALSE)
  sum(as.integer(xor(x, y))) == 0L
}

# Python repr() of a short value, used in a few user-facing messages.
py_repr <- function(x) {
  if (is.null(x)) return("None")
  if (is_bool(x)) return(if (x) "True" else "False")
  if (is.integer(x) && length(x) == 1L) return(as.character(x))
  if (is.double(x) && length(x) == 1L) return(py_float(x))
  if (is_str(x)) {
    s <- x
    quote <- if (grepl("'", s, fixed = TRUE) && !grepl("\"", s, fixed = TRUE)) "\"" else "'"
    s <- gsub("\\", "\\\\", s, fixed = TRUE)
    if (quote == "'") s <- gsub("'", "\\'", s, fixed = TRUE)
    s <- gsub("\n", "\\n", s, fixed = TRUE)
    s <- gsub("\r", "\\r", s, fixed = TRUE)
    s <- gsub("\t", "\\t", s, fixed = TRUE)
    cps <- utf8ToInt(s)
    if (length(cps) && any(cps < 32L | cps == 127L)) {
      s <- paste(vapply(cps, function(cp) {
        if (cp < 32L || cp == 127L) sprintf("\\x%02x", cp) else intToUtf8(cp)
      }, ""), collapse = "")
    }
    return(paste0(quote, s, quote))
  }
  if (is.list(x)) {
    if (is_obj(x)) {
      inner <- vapply(names(x), function(k) paste0(py_repr(k), ": ", py_repr(x[[k]])), "")
      return(paste0("{", paste(inner, collapse = ", "), "}"))
    }
    return(paste0("[", paste(vapply(x, py_repr, ""), collapse = ", "), "]"))
  }
  as.character(x)[1]
}

# Python float repr(): the shortest decimal string that round-trips.
py_float <- function(x) {
  if (is.na(x)) return("nan")
  if (is.infinite(x)) return(if (x > 0) "inf" else "-inf")
  if (x == 0) return(if (1 / x < 0) "-0.0" else "0.0")
  d <- 1L
  repeat {
    s <- sprintf(paste0("%.", d - 1L, "e"), x)
    if (as.numeric(s) == x || d >= 17L) break
    d <- d + 1L
  }
  parts <- strsplit(s, "e", fixed = TRUE)[[1]]
  mantissa <- parts[1]; exponent <- as.integer(parts[2])
  negative <- startsWith(mantissa, "-")
  digits <- gsub("[-.]", "", mantissa)
  digits <- sub("0+$", "", digits)
  if (!nzchar(digits)) digits <- "0"
  sign <- if (negative) "-" else ""
  if (exponent < -4 || exponent >= 16) {
    m <- if (nchar(digits) > 1) paste0(substr(digits, 1, 1), ".", substring(digits, 2)) else digits
    return(sprintf("%s%se%s%02d", sign, m, if (exponent < 0) "-" else "+", abs(exponent)))
  }
  if (exponent < 0) {
    return(paste0(sign, "0.", strrep("0", -exponent - 1L), digits))
  }
  intlen <- exponent + 1L
  if (nchar(digits) <= intlen) {
    return(paste0(sign, digits, strrep("0", intlen - nchar(digits)), ".0"))
  }
  paste0(sign, substr(digits, 1, intlen), ".", substring(digits, intlen + 1L))
}

# Python str() of a JSON scalar, as used in messages and the methods paragraph.
py_str <- function(x) {
  if (is.double(x) && length(x) == 1L) return(py_float(x))
  if (is_bool(x)) return(if (x) "True" else "False")
  if (is.null(x)) return("None")
  as.character(x)
}

# Escape text so that it is matched literally by PCRE.
rx_escape <- function(x) paste0("\\Q", gsub("\\E", "\\E\\\\E\\Q", x, fixed = TRUE), "\\E")

# Digit values of Unicode decimal digits (Python int() accepts every Nd digit).
digit_value <- function(ch) {
  cp <- utf8ToInt(ch)
  if (cp >= 48L && cp <= 57L) return(cp - 48L)
  start <- cp
  while (start > 0L && grepl("^\\p{Nd}$", intToUtf8(start - 1L), perl = TRUE)) start <- start - 1L
  (cp - start) %% 10L
}

int_from_digits <- function(s) {
  chars <- strsplit(s, "", fixed = TRUE)[[1]]
  value <- 0
  for (ch in chars) value <- value * 10 + digit_value(ch)
  value
}

# Lightweight threading.Event equivalent.
qt_event <- function() {
  self <- new.env(parent = emptyenv())
  self$flag <- FALSE
  self$set <- function() self$flag <- TRUE
  self$clear <- function() self$flag <- FALSE
  self$is_set <- function() isTRUE(self$flag)
  self
}
