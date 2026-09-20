arrow_test_connection <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  DBI::dbExecute(con, paste(
    "CREATE TABLE points AS",
    "SELECT i AS id, (i % 100)::DOUBLE AS x, (i // 100)::DOUBLE AS y,",
    "(i % 256)::INTEGER AS r, ((i * 7) % 256)::INTEGER AS g, 3::INTEGER AS b,",
    "(1 + i % 5)::DOUBLE AS radius FROM range(4500) t(i)"))
  con
}

arrow_test_spec <- function(format = "arrow") {
  list(layers = list(list(
    `@@type` = "ScatterplotLayer", id = "points",
    data = list(type = "duckdb", format = format,
                query = "SELECT id, x, y, r, g, b, radius FROM points ORDER BY id"),
    getPosition = "@@=[x, y]", getFillColor = "@@=[r, g, b]",
    getRadius = "@@=radius")))
}

export_methods <- c("copy_arrows", "record_batch_stream", "copy_parquet")

new_test_dir <- function() {
  path <- tempfile("rdeckgl_transport_")
  dir.create(path)
  path
}

payload_rows <- function(node) {
  if (!is.null(node$`__arrow`)) {
    bytes <- base64enc::base64decode(node$`__arrow`)
    return(arrow::read_ipc_stream(bytes, as_data_frame = FALSE)$num_rows)
  }
  bytes <- base64enc::base64decode(node$`__parquet`)
  arrow::read_parquet(arrow::BufferReader$create(bytes), as_data_frame = FALSE)$num_rows
}

test_that("file transport writes one DuckDB-exported data file into data_dir", {
  con <- arrow_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  data_dir <- new_test_dir()
  w <- deckgl(arrow_test_spec(), con = con, data_transport = "file", data_dir = data_dir)
  node <- w$x$spec$layers[[1]]$data
  files <- list.files(data_dir)
  expect_length(files, 1L)
  expect_true(node$`__export_method` %in% export_methods)
  url <- if (!is.null(node$`__arrow_url`)) node$`__arrow_url` else node$`__parquet_url`
  expect_identical(url, files)
  expect_match(url, "^deckgl_arrow_[0-9a-f]{8}\\.(arrows|parquet)$")
  path <- file.path(data_dir, url)
  rows <- if (!is.null(node$`__arrow_url`)) {
    expect_identical(node$`__arrow_format`, "stream")
    arrow::read_ipc_stream(path, as_data_frame = FALSE)$num_rows
  } else {
    arrow::read_parquet(path, as_data_frame = FALSE)$num_rows
  }
  expect_identical(rows, 4500L)
  expect_null(node$`__arrow`)
  expect_null(node$`__parquet`)
})

test_that("arrow exports never materialise rows in R", {
  con <- arrow_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  testthat::local_mocked_bindings(
    dbGetQuery = function(...) stop("materialised"),
    dbGetQueryArrow = function(...) stop("materialised"),
    .package = "DBI")
  expect_error(DBI::dbGetQuery(con, "SELECT 1"), "materialised")
  data_dir <- new_test_dir()
  for (transport in c("file", "inline")) {
    node <- hydrate_deckgl_spec(arrow_test_spec(), con,
      data_transport = transport, data_dir = data_dir)$layers[[1]]$data
    expect_true(node$`__export_method` %in% export_methods, label = transport)
  }
  expect_length(list.files(data_dir), 1L)
})

test_that("inline arrow transport embeds a payload with every row", {
  con <- arrow_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  w <- deckgl(arrow_test_spec(), con = con)
  node <- w$x$spec$layers[[1]]$data
  expect_true(node$`__export_method` %in% export_methods)
  expect_true(xor(is.character(node$`__arrow`), is.character(node$`__parquet`)))
  if (!is.null(node$`__arrow`)) expect_identical(node$`__arrow_format`, "stream")
  expect_identical(payload_rows(node), 4500L)
  expect_null(node$`__arrow_url`)
  expect_null(node$`__parquet_url`)
  expect_length(list.files(tempdir(), pattern = "^deckgl_arrow_"), 0L)
})

test_that("the export ladder descends to record batches and then Parquet", {
  con <- arrow_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  db_execute <- DBI::dbExecute
  refuse <- function(pattern) {
    function(conn, statement, ...) {
      if (grepl(pattern, statement)) stop("refused: ", pattern)
      db_execute(conn, statement, ...)
    }
  }
  testthat::local_mocked_bindings(dbExecute = refuse("FORMAT ARROWS"), .package = "DBI")
  exported <- .deckgl_export_query_file(con, "SELECT x, y FROM points",
    file.path(new_test_dir(), "ladder"))
  expect_identical(exported$method, "record_batch_stream")
  expect_identical(exported$format, "arrows")
  expect_identical(arrow::read_ipc_stream(exported$path, as_data_frame = FALSE)$num_rows, 4500L)
  node <- hydrate_deckgl_spec(arrow_test_spec(), con, data_transport = "inline")$layers[[1]]$data
  expect_identical(node$`__export_method`, "record_batch_stream")
  expect_identical(payload_rows(node), 4500L)

  testthat::local_mocked_bindings(
    duckdb_fetch_record_batch = function(...) stop("no record batches"),
    .package = "duckdb")
  exported <- .deckgl_export_query_file(con, "SELECT x, y FROM points",
    file.path(new_test_dir(), "ladder"))
  expect_identical(exported$method, "copy_parquet")
  expect_identical(exported$format, "parquet")
  expect_match(exported$path, "\\.parquet$")
  expect_identical(arrow::read_parquet(exported$path, as_data_frame = FALSE)$num_rows, 4500L)
  data_dir <- new_test_dir()
  node <- hydrate_deckgl_spec(arrow_test_spec(), con,
    data_transport = "file", data_dir = data_dir)$layers[[1]]$data
  expect_identical(node$`__export_method`, "copy_parquet")
  expect_identical(node$`__parquet_url`, list.files(data_dir))
  expect_null(node$`__arrow_url`)
  node <- hydrate_deckgl_spec(arrow_test_spec(), con, data_transport = "inline")$layers[[1]]$data
  expect_identical(node$`__export_method`, "copy_parquet")
  expect_identical(payload_rows(node), 4500L)

  testthat::local_mocked_bindings(dbExecute = refuse("^COPY"), .package = "DBI")
  stem <- file.path(new_test_dir(), "ladder")
  expect_error(.deckgl_export_query_file(con, "SELECT x, y FROM points", stem),
    "Failed to export the DuckDB query result.*copy_parquet: refused")
  expect_length(list.files(dirname(stem)), 0L)
})

test_that("invalid queries surface the DuckDB error from the final rung", {
  con <- arrow_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  expect_error(
    hydrate_deckgl_spec(list(data = list(type = "duckdb", format = "arrow",
      query = "SELECT nope FROM missing")), con, data_transport = "inline"),
    "Failed to export.*copy_parquet:.*missing")
})

test_that("JSON data nodes keep the row path beside arrow nodes", {
  con <- arrow_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  spec <- arrow_test_spec()
  spec$layers[[2]] <- list(`@@type` = "ScatterplotLayer", id = "rows",
    data = list(type = "duckdb", query = "SELECT x, y FROM points ORDER BY id LIMIT 3"),
    getPosition = "@@=[x, y]")
  hydrated <- hydrate_deckgl_spec(spec, con, data_transport = "inline")
  expect_true(hydrated$layers[[1]]$data$`__export_method` %in% export_methods)
  rows <- hydrated$layers[[2]]$data
  expect_true(attr(rows, "rdeckgl_json_rows"))
  expect_identical(rows[[3]], list(x = 2, y = 0))
})

test_that("file transport ships the data file as a widget dependency attachment", {
  con <- arrow_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  data_dir <- new_test_dir()
  w <- deckgl(arrow_test_spec(), con = con, data_transport = "file", data_dir = data_dir)
  node <- w$x$spec$layers[[1]]$data
  url <- if (!is.null(node$`__arrow_url`)) node$`__arrow_url` else node$`__parquet_url`

  deps <- Filter(function(d) startsWith(d$name, "deckgl-data-"), w$dependencies)
  expect_length(deps, 1L)
  expect_identical(unname(deps[[1]]$attachment), url)
  expect_identical(normalizePath(deps[[1]]$src$file), normalizePath(data_dir))

  # saveWidget into a directory that is not data_dir must carry the file along.
  out <- new_test_dir()
  html <- file.path(out, "widget.html")
  htmlwidgets::saveWidget(w, html, selfcontained = FALSE)
  copied <- list.files(out, pattern = url, recursive = TRUE)
  expect_length(copied, 1L)
  expect_match(copied, "widget_files/deckgl-data-")
  markup <- paste(readLines(html, warn = FALSE), collapse = "\n")
  expect_match(markup, 'rel="attachment"')
  expect_true(grepl(url, markup, fixed = TRUE))

  # The RStudio Viewer path (htmltools::save_html) places it under libdir.
  viewer <- new_test_dir()
  htmltools::save_html(htmltools::as.tags(w), file = file.path(viewer, "index.html"), libdir = "lib")
  expect_length(list.files(file.path(viewer, "lib"), pattern = url, recursive = TRUE), 1L)
})

test_that("a multi-statement query is refused before any rung runs", {
  con <- arrow_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  spec <- arrow_test_spec()
  spec$layers[[1]]$data$query <- "SELECT x, y FROM points; DROP TABLE points"
  expect_error(
    deckgl(spec, con = con, data_transport = "file", data_dir = new_test_dir()),
    "single SQL statement"
  )
  expect_true(DBI::dbExistsTable(con, "points"))
  expect_identical(.deckgl_single_statement("SELECT 1 ;  "), "SELECT 1")
})

test_that("a NULL in a bound column falls back to row objects instead of buffer bytes", {
  con <- arrow_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  spec <- arrow_test_spec()
  spec$layers[[1]]$data$query <-
    "SELECT id, CASE WHEN i % 500 = 0 THEN NULL ELSE x END AS x, y, r, g, b, radius FROM (SELECT *, id AS i FROM points) ORDER BY id"
  data_dir <- new_test_dir()
  w <- deckgl(spec, con = con, data_transport = "file", data_dir = data_dir)
  node <- w$x$spec$layers[[1]]$data
  url <- if (!is.null(node$`__arrow_url`)) node$`__arrow_url` else node$`__parquet_url`
  path <- file.path(data_dir, url)
  tbl <- if (!is.null(node$`__arrow_url`)) {
    arrow::read_ipc_stream(path, as_data_frame = TRUE)
  } else {
    as.data.frame(arrow::read_parquet(path))
  }
  # The export itself keeps the NULLs; the browser-side guard is covered by
  # tests/js/scatter-binary.cjs.
  expect_true(any(is.na(tbl$x)))
})

test_that("a caller-supplied connection survives the Shiny session ending", {
  con <- arrow_test_connection()
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)
  ended <- list()
  session <- structure(list(
    userData = new.env(parent = emptyenv()),
    input = list(),
    onSessionEnded = function(callback) ended[[length(ended) + 1L]] <<- callback,
    sendCustomMessage = function(...) invisible(NULL)
  ), class = c("ShinySession", "list"))
  testthat::local_mocked_bindings(getDefaultReactiveDomain = function() session, .package = "shiny")
  testthat::local_mocked_bindings(observeEvent = function(...) invisible(NULL), .package = "shiny")
  w <- deckgl(arrow_test_spec(), con = con, data_transport = "file", data_dir = new_test_dir())
  expect_s3_class(w, "htmlwidget")
  # Whatever the session registers must not close a connection it does not own.
  for (callback in ended) try(callback(), silent = TRUE)
  expect_true(DBI::dbIsValid(con))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM points")$n, 4500)
})
