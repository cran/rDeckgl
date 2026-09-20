make_json_test_widget <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "points", data.frame(
    x = c(1.234567890123456, NA_real_, Inf, -Inf), y = c(2, 3, 4, 5),
    label = c("quote\"", "<script>\\text", NA_character_, ""),
    active = c(TRUE, FALSE, NA, TRUE)))
  deckgl(list(layers = list(list(`@@type` = "ScatterplotLayer",
    data = list(type = "duckdb", query = "SELECT * FROM points"),
    getPosition = "@@=[x,y,0]"))), con = con)
}

serialize_test_widget <- function(widget) {
  getFromNamespace("toJSON", "htmlwidgets")(
    getFromNamespace("createPayload", "htmlwidgets")(widget))
}

expect_legacy_json <- function(widget) {
  legacy <- widget
  attr(legacy$x, "TOJSON_FUNC") <- NULL
  expect_identical(serialize_test_widget(widget), serialize_test_widget(legacy))
}

test_that("query rows use columnar serialization without changing JSON", {
  w <- make_json_test_widget()
  expect_true(is.function(attr(w$x, "TOJSON_FUNC")))
  expect_legacy_json(w)
  expect_true(is.list(w$x$spec$layers[[1]]$data[[1]]))
  for (options in list(list(digits = 4), list(auto_unbox = FALSE),
                       list(rownames = TRUE), list(na = "string"))) {
    attr(w$x, "TOJSON_ARGS") <- options
    expect_legacy_json(w)
  }
})

test_that("edited, nested and attributed rows preserve legacy behavior", {
  for (value in list(list(c(1, 2), c(3, 4)), as.Date("2026-01-01"),
                     htmlwidgets::JS("function() {return 1;}"),
                     c(1,2), structure(1, names = "named"), NULL)) {
    w <- make_json_test_widget()
    w$x$spec$layers[[1]]$data[[2]]$x <- value
    expect_legacy_json(w)
  }
  w <- make_json_test_widget()
  w$x$spec$layers[[1]]$data[[1]]$x <- 99
  expect_legacy_json(w)
  expect_match(as.character(serialize_test_widget(w)), '"x":99', fixed=TRUE)
})

test_that("mixed logical and numeric edits preserve JSON scalar types", {
  for (number in list(1.5, 2L)) {
    for (values in list(list(number, TRUE, FALSE), list(TRUE, FALSE, number))) {
      w <- make_json_test_widget()
      for (i in seq_along(values)) {
        w$x$spec$layers[[1]]$data[[i]]$x <- values[[i]]
      }
      w$x$spec$layers[[1]]$data[[4]]$x <- number
      before <- w$x$spec$layers[[1]]$data
      expect_legacy_json(w)
      prepared <- w$preRenderHook(w)
      expect_identical(prepared$x$spec$layers[[1]]$data, before)
      expect_identical(w$x$spec$layers[[1]]$data, before)
      expect_identical(serialize_test_widget(prepared), serialize_test_widget(w))
      expect_legacy_json(prepared)
      for (widget in list(w, prepared)) {
        decoded <- jsonlite::fromJSON(as.character(serialize_test_widget(widget)),
          simplifyVector = FALSE)$x$spec$layers[[1]]$data
        for (i in seq_along(values)) {
          expect_identical(decoded[[i]]$x, values[[i]])
        }
      }
      # An export must not cache the old cells or make the R-facing rows opaque.
      w$x$spec$layers[[1]]$data[[2]]$x <- !values[[2]]
      prepared_again <- w$preRenderHook(w)
      expect_legacy_json(prepared_again)
      decoded <- jsonlite::fromJSON(as.character(serialize_test_widget(prepared_again)),
        simplifyVector = FALSE)$x$spec$layers[[1]]$data
      expect_identical(decoded[[2]]$x, !values[[2]])
      expect_identical(prepared$x$spec$layers[[1]]$data, before)
    }
  }
})

test_that("plain rows are compacted before htmlwidgets scans JS evaluations", {
  w <- make_json_test_widget()
  expect_true(is.function(w$preRenderHook))
  prepared <- w$preRenderHook(w)
  expect_s3_class(prepared$x$spec$layers[[1]]$data, "json")
  expect_true(is.list(w$x$spec$layers[[1]]$data))
  expect_identical(serialize_test_widget(prepared), serialize_test_widget(w))
  for (options in list(list(digits = 4), list(auto_unbox = FALSE),
                       list(rownames = TRUE), list(na = "string"))) {
    attr(w$x, "TOJSON_ARGS") <- options
    expect_identical(serialize_test_widget(w$preRenderHook(w)), serialize_test_widget(w))
  }
  w <- make_json_test_widget()
  w$x$spec$layers[[1]]$data[[2]]$x <- htmlwidgets::JS("function() {return 1;}")
  expect_identical(serialize_test_widget(w$preRenderHook(w)), serialize_test_widget(w))
  attr(w$x, "TOJSON_FUNC") <- function(x, ...) jsonlite::toJSON(x, ...)
  expect_identical(w$preRenderHook(w), w)
})

test_that("hydration avoids data-frame method dispatch for every scalar", {
  check_hydration <- function() {
    counter <- new.env(parent = emptyenv()); counter$n <- 0L
    old <- options(rDeckgl.test.subsets = counter)
    on.exit(options(old), add = TRUE)
    trace("[[.data.frame", where = baseenv(), print = FALSE, tracer = quote({
      if (identical(names(x), c("x", "y")) && nrow(x) == 50L) {
        counter <- getOption("rDeckgl.test.subsets")
        counter$n <- counter$n + 1L
      }
    }))
    on.exit(untrace("[[.data.frame", where = baseenv()), add = TRUE)
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    spec <- list(data = list(type = "duckdb", query =
      "SELECT i::DOUBLE AS x, (i*2)::DOUBLE AS y FROM range(50) t(i)"))
    result <- hydrate_deckgl_spec(spec, con)
    expect_equal(result$data[[50]], list(x = 49, y = 98))
    expect_lte(counter$n, 2L)
  }
  check_hydration()
})

test_that("serializer checks supported scalar types once per prototype field", {
  rows <- lapply(seq_len(50), function(i) list(x = i, flag = TRUE))
  attr(rows, "rdeckgl_json_rows") <- TRUE
  counter <- new.env(parent = emptyenv()); counter$n <- 0L
  old <- options(rDeckgl.test.scalar.types = counter)
  on.exit(options(old), add = TRUE)
  trace("%in%", where = baseenv(), print = FALSE, tracer = quote({
    if (identical(table, c("double", "integer", "logical", "character"))) {
      counter <- getOption("rDeckgl.test.scalar.types")
      counter$n <- counter$n + 1L
    }
  }))
  on.exit(untrace("%in%", where = baseenv()), add = TRUE)
  result <- .deckgl_to_json(rows, .prepare_only = TRUE)
  expect_s3_class(result, "json")
  expect_lte(counter$n, 2L)
  expect_identical(jsonlite::fromJSON(result)$x, seq_len(50))
})

test_that("serializer scalar shortcut keeps attributed and structural fallbacks", {
  for (value in list(factor("a"), as.POSIXct("2020-01-01", tz = "UTC"),
      as.POSIXlt("2020-01-01", tz = "UTC"), matrix(1:4, 2), I(1),
      structure(1, unit = "custom"), list(TRUE, 1.5))) {
    w <- make_json_test_widget()
    w$x$spec$layers[[1]]$data[[2]]$x <- value
    expect_legacy_json(w)
    expect_identical(serialize_test_widget(w$preRenderHook(w)), serialize_test_widget(w))
  }
  for (rows in list(list(), list(list(), list()),
      list(setNames(list(1, TRUE), c("x", "x"))),
      list(structure(list(x = 1), custom = "row")))) {
    attr(rows, "rdeckgl_json_rows") <- TRUE
    w <- make_json_test_widget(); w$x$spec$layers[[1]]$data <- rows
    expect_legacy_json(w)
    expect_identical(serialize_test_widget(w$preRenderHook(w)), serialize_test_widget(w))
  }
})
