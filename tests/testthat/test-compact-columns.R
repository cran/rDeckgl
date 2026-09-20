test_that("bulk scalar extraction preserves exact types and values", {
  expect_true(exists(".deckgl_scalar_column", mode = "function"))
  if (!exists(".deckgl_scalar_column", mode = "function")) return(invisible(NULL))
  for (values in list(c(1L, NA_integer_, -2L), c(1.234567890123456, NA, Inf, -Inf, NaN),
                      c(TRUE, FALSE, NA), c("quote\"", "<script>\\text", NA, ""))) {
    rows <- lapply(values, function(value) list(x = value))
    expect_identical(.deckgl_scalar_column(rows, "x", values[[1]]), values)
    expect_identical(rows, lapply(values, function(value) list(x = value)))
  }
})

test_that("serializer performs bulk extraction once per field", {
  local <- new.env(parent = environment(.deckgl_to_json))
  calls <- 0L
  local$.deckgl_scalar_column <- function(...) {
    calls <<- calls + 1L
    .deckgl_scalar_column(...)
  }
  encode <- .deckgl_to_json
  environment(encode) <- local
  rows <- lapply(1:50, function(i) list(x = i, active = TRUE))
  attr(rows, "rdeckgl_json_rows") <- TRUE
  expect_identical(encode(rows), jsonlite::toJSON(rows, auto_unbox = TRUE))
  expect_identical(calls, 2L)
})

test_that("bulk extraction rejects edited scalar shapes and types", {
  for (bad in list(TRUE, 2L, NULL, numeric(), c(1, 2), list(1),
                   structure(1, names = "x"), I(1), as.Date("2020-01-01"),
                   structure(1, unit = "custom"), htmlwidgets::JS("x"))) {
    expect_null(.deckgl_scalar_column(list(list(x = 1.5), list(x = bad)), "x", 1.5))
  }
})

test_that("incompatible first field stops compaction before remaining fields", {
  local <- new.env(parent = environment(.deckgl_to_json))
  fields <- character()
  local$.deckgl_scalar_column <- function(rows, field, prototype) {
    fields <<- c(fields, field)
    .deckgl_scalar_column(rows, field, prototype)
  }
  encode <- .deckgl_to_json
  environment(encode) <- local
  rows <- list(list(x = 1.5, y = 2), list(x = TRUE, y = 3))
  attr(rows, "rdeckgl_json_rows") <- TRUE
  expect_identical(encode(rows, .prepare_only = TRUE), rows)
  expect_identical(fields, "x")
})
