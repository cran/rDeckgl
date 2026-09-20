legacy_hydration_rows <- function(df) {
  columns <- unclass(df)
  lapply(seq_len(nrow(df)), function(i) {
    row <- list()
    for (col_name in names(columns)) {
      val <- columns[[col_name]][i]
      if (is.list(val) && length(val) == 1) row[[col_name]] <- val[[1]] else row[[col_name]] <- val
    }
    row
  })
}

test_that("hydration exposes exact bulk row construction", {
  expect_true(exists(".deckgl_hydrate_rows", mode = "function"))
  if (!exists(".deckgl_hydrate_rows", mode = "function")) return(invisible(NULL))
  frames <- list(data.frame(x = c(1, NA, Inf), flag = c(TRUE, FALSE, NA),
                            label = c("quote\"", NA, "")),
                 data.frame(x = numeric()), data.frame(row.names = 1:2),
                 data.frame(x = 1:2, x = 3:4, check.names = FALSE),
                 data.frame(x = 1:2, nested = I(list(NULL, c(2, 3)))),
                 data.frame(x = as.Date(c("2020-01-01", NA))),
                 data.frame(x = factor(c("a", "b"))))
  for (df in frames) {
    before <- df
    expect_identical(.deckgl_hydrate_rows(df), legacy_hydration_rows(df))
    expect_identical(df, before)
  }
})

test_that("SQL hydration uses bulk row construction once per result", {
  local <- new.env(parent = environment(hydrate_deckgl_spec))
  calls <- 0L
  local$.deckgl_hydrate_rows <- function(...) {
    calls <<- calls + 1L
    .deckgl_hydrate_rows(...)
  }
  hydrate <- hydrate_deckgl_spec
  environment(hydrate) <- local
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  result <- hydrate(list(data = list(type = "duckdb", query =
    "SELECT i::DOUBLE AS x, (i*2)::DOUBLE AS y FROM range(50) t(i)")), con)
  expect_identical(result$data[[50]], list(x = 49, y = 98))
  expect_true(attr(result$data, "rdeckgl_json_rows"))
  expect_identical(calls, 1L)
})
