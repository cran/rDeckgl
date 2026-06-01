# R/ggsql_parser.R
#
# Lightweight parser for the ggsql dialect introduced by Posit.
#
# Grammar (case-insensitive keywords):
#
#   [<sql>] VISUALIZE <map> [FROM <source>] [<clause> ...]
#
#   <map>    := <expr> AS <aesthetic> (, <expr> AS <aesthetic>)*
#   <clause> := DRAW <layer> [<SETTING block>]
#             | PLACE <layer> [<SETTING block>]
#             | SCALE <aesthetic> TO <palette>
#             | LABEL <kv list>
#             | SETTING <kv list>           # top-level (viz options)
#   <SETTING block> := SETTING k => v, k => v
#   <kv list>       := k => v, k => v
#
# This parser is intentionally tolerant: it preserves the user's SQL
# expressions and palette names verbatim and only enforces structural
# boundaries.
#
# IR shape:
#   list(
#     base_sql   = "<SQL before VISUALIZE, or NULL>",
#     from       = "<source table after FROM in the ggsql block, or NULL>",
#     aesthetics = list(x = "...", y = "...", color = "..."),
#     layers     = list(
#       list(kind = "draw"|"place", type = "point", settings = list(...))
#     ),
#     scales     = list(list(aesthetic = "fill", palette = "accent")),
#     labels     = list(title = "...", x = "...", y = "..."),
#     settings   = list(coord = "cartesian", basemap = "dark", ...)
#   )
#
# Not exported; both rMosaic and rDeckgl ship an identical copy of this
# file so neither depends on the other.

# Top-level clause keywords. FROM is handled specially because it can
# appear either as part of the base SELECT or after VISUALIZE.
.ggsql_clause_kw <- c("VISUALIZE", "DRAW", "PLACE", "SCALE", "LABEL", "SETTING")

# Parse a ggsql query into the IR documented at the top of this file.
# Returns NULL if the input contains no VISUALIZE keyword.
.parse_ggsql <- function(sql) {
  if (!is.character(sql) || length(sql) != 1L || !nzchar(sql)) {
    stop("ggsql input must be a non-empty character scalar.")
  }

  masked <- .ggsql_mask(sql)
  if (!grepl("\\bVISUALIZE\\b", masked$text, ignore.case = TRUE)) {
    return(NULL)
  }

  pos <- regexpr("\\bVISUALIZE\\b", masked$text, ignore.case = TRUE)
  vstart <- as.integer(pos)
  base_sql <- if (vstart > 1L) {
    .ggsql_unmask(substr(masked$text, 1L, vstart - 1L), masked)
  } else NULL
  base_sql <- .ggsql_trim_trailing_semi(.ggsql_trim(base_sql))
  if (!is.null(base_sql) && !nzchar(base_sql)) base_sql <- NULL

  body <- substr(masked$text, vstart, nchar(masked$text))
  chunks <- .ggsql_split_clauses(body)

  ir <- list(
    base_sql   = base_sql,
    from       = NULL,
    aesthetics = list(),
    layers     = list(),
    scales     = list(),
    labels     = list(),
    settings   = list()
  )

  for (ch in chunks) {
    kw   <- toupper(ch$keyword)
    body <- ch$body
    if (kw == "VISUALIZE") {
      parsed <- .ggsql_parse_visualize(body, masked)
      ir$aesthetics <- parsed$aesthetics
      ir$from <- parsed$from
    } else if (kw == "DRAW" || kw == "PLACE") {
      ir$layers <- c(
        ir$layers,
        list(.ggsql_parse_layer(body, kind = tolower(kw), masked = masked))
      )
    } else if (kw == "SCALE") {
      ir$scales <- c(ir$scales, list(.ggsql_parse_scale(body)))
    } else if (kw == "LABEL") {
      ir$labels <- utils::modifyList(
        ir$labels,
        .ggsql_parse_kvlist(body, masked)
      )
    } else if (kw == "SETTING") {
      ir$settings <- utils::modifyList(
        ir$settings,
        .ggsql_parse_kvlist(body, masked)
      )
    }
  }

  if (!length(ir$layers)) {
    stop("ggsql query must include at least one DRAW or PLACE clause.")
  }

  ir
}

# Mask string and comment content so keyword matching only sees code.
.ggsql_mask <- function(sql) {
  replacements <- list()
  out <- sql
  # Block comments
  out <- .ggsql_replace_pattern(out, "/\\*.*?\\*/", replacements,
                                kind = "comment")
  # Line comments
  out <- .ggsql_replace_pattern(out, "--[^\n]*", replacements,
                                kind = "comment")
  # Single-quoted strings (allow doubled '' for escaping)
  out <- .ggsql_replace_pattern(out, "'(?:''|[^'])*'", replacements,
                                kind = "string")
  # Double-quoted identifiers
  out <- .ggsql_replace_pattern(out, "\"(?:\"\"|[^\"])*\"", replacements,
                                kind = "ident")
  list(text = out$text, replacements = out$replacements %||% replacements)
}

.ggsql_replace_pattern <- function(text, pattern, replacements, kind) {
  if (is.list(text)) {
    replacements <- text$replacements
    text <- text$text
  }
  out <- ""
  remaining <- text
  while (nzchar(remaining)) {
    m <- regexpr(pattern, remaining, perl = TRUE)
    if (m == -1L) {
      out <- paste0(out, remaining)
      break
    }
    start <- as.integer(m)
    len <- attr(m, "match.length")
    before <- substr(remaining, 1L, start - 1L)
    matched <- substr(remaining, start, start + len - 1L)
    token <- sprintf("\x01%s_%d\x02", kind, length(replacements) + 1L)
    replacements[[token]] <- matched
    out <- paste0(out, before, token)
    remaining <- substr(remaining, start + len, nchar(remaining))
  }
  list(text = out, replacements = replacements)
}

.ggsql_unmask <- function(text, masked) {
  if (is.null(text) || !nzchar(text)) return(text)
  out <- text
  for (token in names(masked$replacements)) {
    out <- gsub(token, masked$replacements[[token]], out, fixed = TRUE)
  }
  out
}

.ggsql_trim <- function(x) {
  if (is.null(x)) return(NULL)
  sub("[[:space:]]+$", "", sub("^[[:space:]]+", "", x))
}

.ggsql_trim_trailing_semi <- function(x) {
  if (is.null(x)) return(NULL)
  sub(";[[:space:]]*$", "", x)
}

.ggsql_split_clauses <- function(body) {
  # Find offsets of clause keywords as whole words.
  pattern <- paste0(
    "\\b(", paste(.ggsql_clause_kw, collapse = "|"), ")\\b"
  )
  m <- gregexpr(pattern, body, ignore.case = TRUE, perl = TRUE)[[1]]
  if (m[1] == -1L) {
    stop("ggsql parse error: no recognised clauses found.")
  }
  starts <- as.integer(m)
  ends <- c(starts[-1] - 1L, nchar(body))
  raw <- vector("list", length(starts))
  for (i in seq_along(starts)) {
    s <- starts[i]
    chunk <- substr(body, s, ends[i])
    kw_match <- regexpr(pattern, chunk, ignore.case = TRUE, perl = TRUE)
    kw_len <- attr(kw_match, "match.length")
    kw <- substr(chunk, 1L, kw_len)
    rest <- .ggsql_trim(substr(chunk, kw_len + 1L, nchar(chunk)))
    raw[[i]] <- list(keyword = kw, body = rest, raw = chunk)
  }
  # The first SETTING immediately after a DRAW/PLACE attaches to that
  # layer; further SETTINGs (or SETTINGs that follow VISUALIZE/SCALE/LABEL)
  # are treated as top-level viz options.
  out <- list()
  attached <- logical()
  for (ch in raw) {
    prev_kw <- if (length(out)) toupper(out[[length(out)]]$keyword) else ""
    cur_kw  <- toupper(ch$keyword)
    prev_attached <- length(attached) && attached[length(attached)]
    if (cur_kw == "SETTING" &&
        prev_kw %in% c("DRAW", "PLACE") &&
        !prev_attached) {
      prev <- out[[length(out)]]
      prev$body <- paste(prev$body, ch$raw, sep = " ")
      out[[length(out)]] <- prev
      attached[length(attached)] <- TRUE
    } else {
      out[[length(out) + 1L]] <- ch
      attached <- c(attached, FALSE)
    }
  }
  out
}

.ggsql_parse_visualize <- function(body, masked) {
  # Split off optional FROM <source> at the end. FROM here binds tighter
  # than other clauses because clauses have already been peeled away.
  from <- NULL
  m <- regexpr("\\bFROM\\b", body, ignore.case = TRUE, perl = TRUE)
  if (m != -1L) {
    fstart <- as.integer(m)
    flen <- attr(m, "match.length")
    map_part <- .ggsql_trim(substr(body, 1L, fstart - 1L))
    from_part <- .ggsql_trim(substr(body, fstart + flen, nchar(body)))
    from <- .ggsql_unmask(from_part, masked)
  } else {
    map_part <- body
  }
  list(
    aesthetics = .ggsql_parse_mappings(map_part, masked),
    from = from
  )
}

.ggsql_parse_mappings <- function(body, masked) {
  parts <- .ggsql_split_commas(body)
  if (!length(parts)) {
    stop("VISUALIZE clause requires at least one `<expr> AS <aesthetic>` mapping.")
  }
  out <- list()
  for (p in parts) {
    if (!grepl("\\bAS\\b", p, ignore.case = TRUE, perl = TRUE)) {
      stop(sprintf("VISUALIZE mapping '%s' is missing 'AS <aesthetic>'.", p))
    }
    pieces <- strsplit(p, "(?i)\\bAS\\b", perl = TRUE)[[1]]
    if (length(pieces) != 2L) {
      stop(sprintf("VISUALIZE mapping '%s' is malformed.", p))
    }
    expr <- .ggsql_trim(.ggsql_unmask(pieces[[1]], masked))
    aes_name <- tolower(.ggsql_trim(pieces[[2]]))
    if (!nzchar(aes_name)) {
      stop("Empty aesthetic name in VISUALIZE mapping.")
    }
    out[[aes_name]] <- expr
  }
  out
}

.ggsql_parse_layer <- function(body, kind, masked) {
  # Split off SETTING tail
  m <- regexpr("\\bSETTING\\b", body, ignore.case = TRUE, perl = TRUE)
  if (m == -1L) {
    type_part <- body
    settings <- list()
  } else {
    sstart <- as.integer(m)
    slen <- attr(m, "match.length")
    type_part <- substr(body, 1L, sstart - 1L)
    setting_part <- substr(body, sstart + slen, nchar(body))
    settings <- .ggsql_parse_kvlist(setting_part, masked)
  }
  type <- .ggsql_trim(type_part)
  if (!nzchar(type)) {
    stop(sprintf("%s clause requires a layer type.", toupper(kind)))
  }
  # Layer type is a bare identifier; drop trailing punctuation.
  type <- sub("[[:space:],;]+$", "", type)
  list(kind = kind, type = tolower(type), settings = settings)
}

.ggsql_parse_scale <- function(body) {
  if (!grepl("\\bTO\\b", body, ignore.case = TRUE, perl = TRUE)) {
    stop(sprintf("SCALE clause '%s' is missing 'TO <palette>'.", body))
  }
  pieces <- strsplit(body, "(?i)\\bTO\\b", perl = TRUE)[[1]]
  if (length(pieces) != 2L) {
    stop(sprintf("SCALE clause '%s' is malformed.", body))
  }
  list(
    aesthetic = tolower(.ggsql_trim(pieces[[1]])),
    palette = .ggsql_trim(pieces[[2]])
  )
}

.ggsql_parse_kvlist <- function(body, masked) {
  parts <- .ggsql_split_commas(body)
  out <- list()
  for (p in parts) {
    if (!grepl("=>", p, fixed = TRUE)) {
      stop(sprintf("Expected 'key => value' in '%s'.", p))
    }
    pieces <- strsplit(p, "=>", fixed = TRUE)[[1]]
    if (length(pieces) != 2L) {
      stop(sprintf("Malformed key/value pair '%s'.", p))
    }
    key <- tolower(.ggsql_trim(pieces[[1]]))
    raw_val <- .ggsql_trim(pieces[[2]])
    out[[key]] <- .ggsql_decode_value(raw_val, masked)
  }
  out
}

.ggsql_decode_value <- function(raw, masked) {
  raw <- .ggsql_trim(raw)
  # Parenthesised list?
  if (startsWith(raw, "(") && endsWith(raw, ")")) {
    inner <- substr(raw, 2L, nchar(raw) - 1L)
    items <- .ggsql_split_commas(inner)
    return(lapply(items, function(x) .ggsql_decode_scalar(x, masked)))
  }
  .ggsql_decode_scalar(raw, masked)
}

.ggsql_decode_scalar <- function(raw, masked) {
  raw <- .ggsql_trim(raw)
  if (!nzchar(raw)) return(NULL)
  # Reconstruct string literals if the token was masked.
  unmasked <- .ggsql_unmask(raw, masked)
  if (grepl("^'.*'$", unmasked)) {
    inner <- substr(unmasked, 2L, nchar(unmasked) - 1L)
    return(gsub("''", "'", inner, fixed = TRUE))
  }
  if (grepl("^\".*\"$", unmasked)) {
    return(substr(unmasked, 2L, nchar(unmasked) - 1L))
  }
  # Booleans
  if (tolower(unmasked) %in% c("true", "false")) {
    return(identical(tolower(unmasked), "true"))
  }
  # Numeric
  num <- suppressWarnings(as.numeric(unmasked))
  if (!is.na(num) && grepl("^-?[0-9]+(\\.[0-9]+)?([eE][-+]?[0-9]+)?$",
                           unmasked)) {
    return(num)
  }
  # Bare identifier (palette name, layer flag, etc.) - keep as string.
  unmasked
}

# Split on commas at depth zero (ignoring commas inside parentheses).
.ggsql_split_commas <- function(body) {
  body <- .ggsql_trim(body)
  if (!nzchar(body)) return(character())
  chars <- strsplit(body, "", fixed = TRUE)[[1]]
  depth <- 0L
  start <- 1L
  out <- character()
  for (i in seq_along(chars)) {
    ch <- chars[[i]]
    if (ch == "(") depth <- depth + 1L
    else if (ch == ")") depth <- depth - 1L
    else if (ch == "," && depth == 0L) {
      out <- c(out, .ggsql_trim(substr(body, start, i - 1L)))
      start <- i + 1L
    }
  }
  tail <- .ggsql_trim(substr(body, start, length(chars)))
  if (nzchar(tail)) out <- c(out, tail)
  out
}
