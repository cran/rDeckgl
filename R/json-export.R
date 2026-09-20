# Preserve the live-query contract: unlike hydration, list cells are not
# unwrapped and factors are not converted. Only ordinary atomic frames bypass
# data-frame row subsetting; methods, attributes and name repair use base R.
.deckgl_data_frame_rows <- function(df) {
  simple <- identical(class(df), "data.frame") && length(df) > 0L &&
    setequal(names(attributes(df)), c("names", "row.names", "class")) &&
    !anyDuplicated(names(df)) && !anyNA(names(df)) &&
    all(nzchar(names(df))) &&
    all(vapply(df, function(col) is.null(attributes(col)) &&
      typeof(col) %in% c("logical", "integer", "double", "character"), logical(1)))
  if (!simple) {
    return(lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE])))
  }
  columns <- as.list(df)
  lapply(seq_len(nrow(df)), function(i) lapply(columns, `[`, i))
}

# Hydration unwraps list cells and removes NULL fields, unlike live-query rows.
# Preallocate rows only when that distinction cannot matter. Keep the original
# named-assignment behavior for all other shapes.
# Extra frame attributes do not affect positional extraction of plain columns.
.deckgl_hydrate_rows <- function(df) {
  columns <- unclass(df)
  fields <- names(columns)
  if (identical(class(df), "data.frame") && length(fields) > 0L &&
      !anyDuplicated(fields) && !anyNA(fields) && all(nzchar(fields)) &&
      all(vapply(columns, function(col) is.null(attributes(col)) &&
        typeof(col) %in% c("logical", "integer", "double", "character"), logical(1)))) {
    template <- setNames(vector("list", length(fields)), fields)
    indices <- seq_along(columns)
    return(lapply(seq_len(nrow(df)), function(i) {
      row <- template
      for (j in indices) row[[j]] <- columns[[j]][i]
      row
    }))
  }
  lapply(seq_len(nrow(df)), function(i) {
    row <- list()
    for (col_name in fields) {
      val <- columns[[col_name]][i]
      if (is.list(val) && length(val) == 1) row[[col_name]] <- val[[1]] else row[[col_name]] <- val
    }
    row
  })
}

# Extract using primitive callbacks, then validate before any coercion. In
# particular, a logical edit in a numeric column must not become JSON 0/1.
.deckgl_scalar_column <- function(rows, field, prototype) {
  values <- lapply(rows, `[[`, field)
  if (any(lengths(values) != 1L) ||
      any(lengths(lapply(values, attributes)) != 0L) ||
      any(vapply(values, typeof, character(1)) != typeof(prototype))) return(NULL)
  unlist(values, recursive = FALSE, use.names = FALSE)
}

# htmlwidgets' supported serializer hook, with its default JSON options.
# Only marked SQL-result row lists take the fast path. The R-facing lists stay
# editable; columns are rebuilt from current rows, never a stale cached copy.
.deckgl_to_json <- function(x, ..., strict_atomic = TRUE, .prepare_only = FALSE) {
  defaults <- list(dataframe = "columns", null = "null", na = "null",
    auto_unbox = TRUE, digits = getOption("shiny.json.digits", 16),
    use_signif = TRUE, force = TRUE, POSIXt = "ISO8601", UTC = TRUE,
    rownames = FALSE, keep_vec_names = TRUE, json_verbatim = TRUE)
  overrides <- list(...)
  args <- c(defaults[setdiff(names(defaults), names(overrides))], overrides)
  encode <- function(value, options = args) {
    if (strict_atomic) value <- I(value)
    do.call(jsonlite::toJSON, c(list(x = value), options))
  }
  plain_scalar <- function(value) {
    length(value) == 1L && is.null(attributes(value)) &&
      typeof(value) %in% c("double", "integer", "logical", "character")
  }
  compact <- function(node) {
    if (isTRUE(attr(node, "rdeckgl_json_rows", exact = TRUE))) {
      if (!length(node) || !is.null(names(node)) ||
          !isTRUE(args$auto_unbox) || !isTRUE(args$json_verbatim)) return(node)
      prototype <- node[[1]]
      fields <- names(prototype)
      if (!is.list(prototype) || !length(fields) || anyNA(fields) ||
          any(!nzchar(fields)) || anyDuplicated(fields) ||
          !all(vapply(prototype, plain_scalar, logical(1)))) return(node)
      row_attributes <- list(names = fields)
      if (!all(vapply(node, function(row) is.list(row) &&
          identical(attributes(row), row_attributes), logical(1)))) return(node)
      columns <- tryCatch(lapply(fields, function(field) {
        column <- .deckgl_scalar_column(node, field, prototype[[field]])
        if (is.null(column)) stop("incompatible JSON row scalar")
        column
      }), error = function(e) NULL)
      # Nested/attributed/heterogeneous values keep the original serializer.
      if (is.null(columns)) return(node)
      names(columns) <- fields
      frame <- as.data.frame(columns, optional = TRUE, stringsAsFactors = FALSE)
      row_args <- args
      row_args$dataframe <- "rows"
      row_args$rownames <- FALSE
      return(encode(frame, row_args))
    }
    if (is.list(node) && !is.object(node)) node[] <- lapply(node, compact)
    node
  }
  prepared <- compact(x)
  if (.prepare_only) prepared else encode(prepared)
}

# Compact before createPayload/JSEvals constructs names for every row and cell.
# Keep the original widget editable, and leave user-supplied serializers alone.
.deckgl_pre_render <- function(widget) {
  if (!identical(attr(widget$x, "TOJSON_FUNC"), .deckgl_to_json)) return(widget)
  args <- attr(widget$x, "TOJSON_ARGS", exact = TRUE)
  if (is.null(args)) args <- getOption("htmlwidgets.TOJSON_ARGS")
  widget$x <- do.call(.deckgl_to_json,
    c(list(x = widget$x, .prepare_only = TRUE), args))
  widget
}
