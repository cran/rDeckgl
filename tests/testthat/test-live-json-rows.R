legacy_live_rows <- function(df) {
  lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE]))
}

test_that("live JSON conversion exposes reusable exact row conversion", {
  expect_true(exists(".deckgl_data_frame_rows", mode = "function"))
  if (exists(".deckgl_data_frame_rows", mode = "function")) {
    df <- data.frame(x = c(1, NA, Inf), flag = c(TRUE, FALSE, NA),
      text = c("quote\"", "<script>", ""), row.names = c("a", "b", "c"))
    expect_identical(.deckgl_data_frame_rows(df), legacy_live_rows(df))
  }
})

test_that("live Shiny JSON responses avoid per-row data-frame subsets", {
  session <- shiny::MockShinySession$new()
  on.exit(session$close(), add = TRUE)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(if (DBI::dbIsValid(con)) DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  messages <- list()
  session$sendCustomMessage <- function(type, message) {
    messages[[length(messages) + 1L]] <<- list(type = type, message = message)
  }
  w <- shiny::withReactiveDomain(session, deckgl(list(layers = list()), con = con))
  session$flushReact()
  counter <- new.env(parent = emptyenv()); counter$n <- 0L
  old <- options(rDeckgl.test.live.subsets = counter)
  on.exit(options(old), add = TRUE)
  trace("[.data.frame", where = baseenv(), print = FALSE, tracer = quote({
    if (identical(names(x), c("x", "flag")) && nrow(x) == 50L) {
      counter <- getOption("rDeckgl.test.live.subsets")
      counter$n <- counter$n + 1L
    }
  }))
  on.exit(untrace("[.data.frame", where = baseenv()), add = TRUE)
  query <- "SELECT i::DOUBLE AS x, (i%2=0) AS flag FROM range(50) t(i)"
  do.call(session$setInputs, setNames(list(list(type = "json", sql = query,
    request = "live-test")), paste0(w$x$widgetId, "_deckgl_query")))
  expect_length(messages, 1L)
  expect_identical(messages[[1]]$message$request, "live-test")
  expect_identical(messages[[1]]$message$dataFormat, "json")
  expect_lte(counter$n, 2L)
  # Compare the full unmarked payload, including types and its wire JSON.
  expected <- legacy_live_rows(DBI::dbGetQuery(con, query))
  expect_identical(messages[[1]]$message$data, expected)
  encode <- getFromNamespace("toJSON", "shiny")
  expect_identical(encode(messages[[1]]$message$data), encode(expected))
})

test_that("row conversion preserves base subsetting across unusual columns", {
  plain <- data.frame(number = c(1, NA, -Inf), integer = 1:3,
    flag = c(TRUE, FALSE, NA), text = c("a", "quote\"", NA),
    row.names = c("one", "two", "three"))
  cases <- list(plain, plain[FALSE, ], plain[, FALSE, drop = FALSE])
  additions <- list(factor(c("a", NA, "b")), as.Date(c("2020-01-01", NA, "2022-01-01")),
    as.POSIXct(c("2020-01-01", NA, "2022-01-01"), tz = "America/New_York"),
    as.POSIXlt(as.POSIXct(c("2020-01-01", NA, "2022-01-01"), tz = "UTC")),
    list(1, list(a = 2), NULL), matrix(1:6, 3, dimnames = list(c("x", "y", "z"), c("a", "b"))),
    I(1:3), I(matrix(1:6, 3)), structure(1:3, unit = "custom"),
    structure(1:3, names = c("a", "b", "c")), list(TRUE, 1L, 1.5))
  for (column in additions) {
    df <- plain; df$extra <- column
    cases[[length(cases) + 1L]] <- df
  }
  duplicate <- plain; names(duplicate) <- c("same", "same", "flag", "text")
  cases <- c(cases, list(duplicate, structure(plain, custom = "frame attribute")))
  for (df in cases) {
    before <- df
    expected <- legacy_live_rows(df)
    actual <- .deckgl_data_frame_rows(df)
    expect_identical(actual, expected)
    expect_identical(df, before)
    encode <- getFromNamespace("toJSON", "shiny")
    expect_identical(encode(actual), encode(expected))
  }
  set.seed(192)
  for (iteration in seq_len(30L)) {
    n <- sample(0:20, 1)
    df <- data.frame(a = sample(c(NA_real_, Inf, -Inf, -1, 0, 2.5), n, TRUE),
      b = sample(c(TRUE, FALSE, NA), n, TRUE),
      c = sample(c(NA_character_, "", "é", "\\\""), n, TRUE))
    if (n) rownames(df) <- paste0("row", seq_len(n))
    expect_identical(.deckgl_data_frame_rows(df), legacy_live_rows(df))
  }
})
