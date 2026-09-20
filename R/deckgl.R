# R/deckgl.R

#' @importFrom stats setNames
NULL

#' Render a Deck.gl visualization
#'
#' Creates an interactive deck.gl visualization from a JSON or YAML specification.
#' Supports server-side data hydration via DuckDB for efficient data handling.
#'
#' @param spec      Deck.gl specification as an R list, JSON text, JSON file path,
#'                  YAML text, or YAML file path.
#' @param specType  One of "auto" (default), "json", or "yaml". Auto-detection
#'                  attempts to infer the format from the input.
#' @param data      Named list of data.frames to register in DuckDB. These tables
#'                  can be referenced in the spec using `type = "duckdb"` data nodes.
#' @param con       Optional DuckDB connection to use for queries. If provided,
#'                  this connection is used instead of creating a new one, which
#'                  is useful for GeoArrow workflows where the spatial extension
#'                  and geometry tables are already set up. `rDeckgl` never
#'                  disconnects a supplied connection, in a Shiny session or
#'                  otherwise; only connections it opens itself are closed, when
#'                  the call returns or when the Shiny session that serves their
#'                  queries ends.
#' @param data_transport How hydrated Arrow/Parquet query results are delivered
#'                  to the browser. `"auto"` uses `"file"` when `data_dir` is
#'                  supplied and otherwise falls back to `"inline"` for portable
#'                  widgets; `"inline"` embeds base64 payloads in the widget;
#'                  `"file"` writes binary files to `data_dir` and uses relative
#'                  URLs. Data nodes with `format = "arrow"` are exported by
#'                  DuckDB itself under either transport; see Details.
#' @param data_dir  Directory for `"file"` transport; when omitted a session
#'                  temporary directory is used. The data files travel with the
#'                  widget as an html dependency attachment, so
#'                  `htmlwidgets::saveWidget(selfcontained = FALSE)`, the
#'                  RStudio Viewer and Shiny all resolve them
#'                  (`selfcontained = TRUE` is not supported for file
#'                  transport). Serve or save the widget
#'                  from the same directory so relative URLs resolve.
#' @param width     CSS or pixel width (e.g. "100\%", "600px", or numeric).
#' @param height    CSS or pixel height (e.g. "100\%", "600px", or numeric).
#'
#' @details
#' A `type = "duckdb"` data node with `format = "arrow"` never materialises
#' query rows in R. DuckDB writes the result straight to a binary file using
#' the first method that succeeds: `COPY ... (FORMAT ARROWS)` when the
#' community `nanoarrow` DuckDB extension can be loaded, otherwise Arrow
#' record-batch streaming through `arrow::write_ipc_stream()`, otherwise
#' `COPY ... (FORMAT PARQUET)`. With `data_transport = "file"` that file is
#' written into `data_dir` and the node carries a relative `__arrow_url` (or
#' `__parquet_url`); with `"inline"` its bytes are base64-encoded into the
#' widget as `__arrow` (or `__parquet`). Either way the node records the
#' method in `__export_method` (`"copy_arrows"`, `"record_batch_stream"` or
#' `"copy_parquet"`). Extensions are never installed on a user-supplied
#' `con`; only `LOAD nanoarrow` is attempted.
#'
#' In the browser, standard layers such as `ScatterplotLayer` bind such a
#' table as binary attributes rather than row objects when `getPosition`,
#' `getFillColor` and `getRadius` are plain column references, for example
#' `"@@@@=[x, y]"`, `"@@@@=[x, y, 0]"`, `"@@@@=[r, g, b]"` and `"@@@@=radius"`
#' (or `list(fields = c("x", "y"))`). Constant colours and radii stay plain
#' props. Any other accessor that references row fields makes the layer fall
#' back to row objects, with a console warning naming the accessor.
#'
#' @return An htmlwidget that renders the Deck.gl visualization.
#'
#' @examples
#' if (interactive()) {
#'   # Simple scatterplot with inline data
#'   spec <- list(
#'     `@@type` = "DeckGL",
#'     initialViewState = list(
#'       longitude = -122.4,
#'       latitude = 37.76,
#'       zoom = 12,
#'       pitch = 0,
#'       bearing = 0
#'     ),
#'     layers = list(
#'       list(
#'         `@@type` = "ScatterplotLayer",
#'         id = "scatterplot",
#'         data = list(
#'           type = "duckdb",
#'           query = "SELECT lon, lat, radius FROM points"
#'         ),
#'         getPosition = "@@=[lon, lat]",
#'         getRadius = "@@=radius",
#'         getFillColor = c(255, 0, 0)
#'       )
#'     )
#'   )
#'
#'   data <- list(
#'     points = data.frame(
#'       lon = c(-122.4, -122.45, -122.35),
#'       lat = c(37.76, 37.78, 37.74),
#'       radius = c(100, 150, 200)
#'     )
#'   )
#'
#'   deckgl(spec = spec, data = data)
#' }
#'
#' @export
deckgl <- function(
    spec,
    specType = c("auto", "json", "yaml"),
    data = NULL,
    con = NULL,
    data_transport = c("auto", "file", "inline"),
    data_dir = NULL,
    width = NULL,
    height = NULL) {
  specType <- match.arg(specType)
  data_transport <- match.arg(data_transport)
  if (identical(data_transport, "auto")) {
    data_transport <- if (!is.null(data_dir)) "file" else "inline"
  }
  if (identical(data_transport, "file")) {
    if (is.null(data_dir)) data_dir <- .deckgl_session_data_dir("rdeckgl-data-")
    if (!is.character(data_dir) || length(data_dir) != 1L || !nzchar(data_dir)) {
      stop("'data_dir' must be a single directory path.", call. = FALSE)
    }
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  }
  .deckgl_reset_data_files()
  on.exit(.deckgl_reset_data_files(), add = TRUE)

  # 1) Determine format
  fmt <- specType
  if (fmt == "auto") {
    if (is.list(spec)) {
      fmt <- "json"
    } else if (is.character(spec) && length(spec) == 1 && file.exists(spec)) {
      ext <- tolower(tools::file_ext(spec))
      fmt <- if (ext %in% c("yaml", "yml")) "yaml" else "json"
    } else if (is.character(spec) && grepl("^\\s*-", spec)) {
      fmt <- "yaml"
    } else if (is.character(spec) && grepl("^\\s*\\{", spec)) {
      fmt <- "json"
    } else {
      fmt <- "json"
    }
  }

  spec_list <- NULL

  # 2) Parse JSON / YAML
  if (fmt == "json") {
    if (is.list(spec)) {
      spec_list <- spec
    } else {
      txt <- if (file.exists(spec)) readLines(spec) else spec
      spec_list <- jsonlite::fromJSON(
        paste(txt, collapse = "\n"),
        simplifyVector = FALSE
      )
    }
  } else if (fmt == "yaml") {
    if (is.list(spec)) {
      spec_list <- spec
    } else {
      txt <- if (file.exists(spec)) readLines(spec) else spec
      spec_list <- yaml::read_yaml(text = paste(txt, collapse = "\n"))
    }
  }

  # 3) Embed width/height into spec_list
  if (!is.null(spec_list)) {
    strip_px <- function(x) {
      if (is.numeric(x)) {
        return(as.integer(x))
      }
      if (is.character(x) && grepl("^[0-9]+px$", x)) {
        return(as.integer(sub("px$", "", x)))
      }
      NULL
    }
    if (is.null(spec_list$width) && !is.null(w <- strip_px(width))) {
      spec_list$width <- w
    }
    if (is.null(spec_list$height) && !is.null(h <- strip_px(height))) {
      spec_list$height <- h
    }
  }

  # 4) Setup DuckDB connection with spatial support
  # Use provided connection or create a new one
  own_con <- is.null(con)
  # A connection this function opened is closed when the call returns, unless a
  # Shiny session takes it over to serve the widget's queries. A connection the
  # caller supplied is never closed here: the caller owns its lifetime, and
  # closing it would break every later session in the same R process.
  keep_own_con <- FALSE
  if (own_con) {
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
    on.exit(if (!keep_own_con) try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
    
    # Load spatial extension for GeoArrow support
    try(DBI::dbExecute(con, "INSTALL spatial"), silent = TRUE)
    try(DBI::dbExecute(con, "LOAD spatial"), silent = TRUE)
    
    # Load nanoarrow extension for FORMAT ARROWS export
    try(DBI::dbExecute(con, "INSTALL nanoarrow FROM community"), silent = TRUE)
    try(DBI::dbExecute(con, "LOAD nanoarrow"), silent = TRUE)
    
    # Register GeoArrow extensions for proper Arrow export metadata
    try(DBI::dbExecute(con, "CALL register_geoarrow_extensions()"), silent = TRUE)
  }
  # Note: If user provides con, they are responsible for loading spatial extension,
  # nanoarrow extension, and calling register_geoarrow_extensions() if needed

  # Create metadata storage for list columns (use environment, not S4 slot)
  list_col_metadata <- new.env(parent = emptyenv())

  # 5) Handle data registration
  # Convert R data.frames to DuckDB tables for efficient server-side queries
  if (!is.null(data)) {
    if (!is.list(data)) {
      stop("'data' must be a named list of data.frames")
    }
    if (is.null(names(data)) || any(names(data) == "")) {
      stop("All elements in 'data' list must be named")
    }

    for (nm in names(data)) {
      df <- data[[nm]]
      if (!inherits(df, "data.frame")) {
        stop(sprintf(
          "Element '%s' in data list must be a data.frame, got: %s",
          nm,
          class(df)[1]
        ))
      }

      # Convert factors to character for safe JSON serialization
      # Also handle list columns by converting to JSON strings
      list_cols <- c()
      df[] <- lapply(names(df), function(col_name) {
        col <- df[[col_name]]
        if (is.factor(col)) {
          return(as.character(col))
        }
        # Check if this is a list column (nested structure like polygon coordinates)
        if (is.list(col) && !is.data.frame(col)) {
          list_cols <<- c(list_cols, col_name)
          # Convert list to JSON string for DuckDB storage
          return(vapply(col, jsonlite::toJSON, character(1), auto_unbox = TRUE))
        }
        col
      })
      names(df) <- names(data[[nm]])

      # Register data.frame as DuckDB table
      tryCatch(
        {
          DBI::dbWriteTable(con, nm, df, overwrite = TRUE)

          # Store metadata about which columns are JSON-encoded lists
          if (length(list_cols) > 0) {
            list_col_metadata[[nm]] <- list_cols
          }
        },
        error = function(e) {
          stop(sprintf(
            "Failed to register table '%s' in DuckDB: %s",
            nm,
            e$message
          ))
        }
      )
    }
  }

  # 6) Hydrate spec with DuckDB data
  if (!is.null(spec_list)) {
    spec_list <- hydrate_deckgl_spec(
      spec_list,
      con,
      list_col_metadata,
      data_transport = data_transport,
      data_dir = data_dir
    )
  }

  # 7) Setup Shiny query handler if in Shiny context
  uid <- paste0("deckgl_", sprintf("%08x", sample.int(.Machine$integer.max, 1)))
  session <- shiny::getDefaultReactiveDomain()

  if (!is.null(session) && !is.null(con)) {
    # The session serves queries after this call returns, so a connection this
    # function opened must outlive the call and is handed to the session to
    # close. Caller-supplied connections are deliberately not registered.
    keep_own_con <- TRUE
    if (own_con) {
      session$userData$deckglConnections <-
        c(session$userData$deckglConnections, setNames(list(con), uid))
    }

    # Register query handler
    shiny::observeEvent(
      session$input[[paste0(uid, "_deckgl_query")]],
      {
        req <- session$input[[paste0(uid, "_deckgl_query")]]
        if (is.null(req)) {
          return()
        }

        if (is.null(con)) {
          warning("Connection for widget ", uid, " is not available.")
          return()
        }

        tryCatch(
          {
            use_arrow <- identical(req$type, "arrow") || 
                         identical(req$type, "geoarrow")
            is_geoarrow <- identical(req$type, "geoarrow")
            
            if (use_arrow && requireNamespace("base64enc", quietly = TRUE)) {
              raw_bytes <- NULL
              
              # For geoarrow, prefer ADBC to preserve extension metadata
              if (is_geoarrow) {
                raw_bytes <- .adbc_query_to_ipc_bytes(con, req$sql)
              }
              
              # Fallback to duckdb arrow fetch
              if (is.null(raw_bytes) && requireNamespace("arrow", quietly = TRUE)) {
                res <- DBI::dbSendQuery(con, req$sql)
                arrow_table <- duckdb::duckdb_fetch_arrow(res, stream = TRUE)
                DBI::dbClearResult(res)
                raw_bytes <- arrow::write_to_raw(arrow_table, format = "stream")
              }
              
              if (!is.null(raw_bytes)) {
                session$sendCustomMessage(
                  paste0(uid, "_deckgl_response"),
                  .deckgl_arrow_response_message(
                    session = session,
                    uid = uid,
                    request = req$request,
                    raw_bytes = raw_bytes,
                    data_format = if (is_geoarrow) "geoarrow" else "arrow"
                  )
                )
              }
            } else {
              # Legacy JSON format
              dfres <- DBI::dbGetQuery(con, req$sql)
              payload <- .deckgl_data_frame_rows(dfres)
              session$sendCustomMessage(
                paste0(uid, "_deckgl_response"),
                list(
                  request = req$request,
                  data = payload,
                  dataFormat = "json"
                )
              )
            }
          },
          error = function(e) {
            session$sendCustomMessage(
              paste0(uid, "_deckgl_response"),
              list(request = req$request, error = as.character(e))
            )
          }
        )
      },
      ignoreNULL = TRUE
    )

    # Close only the connections this package opened; a supplied connection is
    # left exactly as the caller handed it over.
    if (is.null(session$userData$.deckglCleanup)) {
      session$onSessionEnded(function() {
        lapply(session$userData$deckglConnections, function(cnn) {
          try(DBI::dbDisconnect(cnn), silent = TRUE)
        })
        session$userData$deckglConnections <- NULL
      })
      session$userData$.deckglCleanup <- TRUE
    }
  }

  # 8) Create widget
  widget_data <- list(
    spec = spec_list,
    widgetId = uid
  )
  attr(widget_data, "TOJSON_FUNC") <- .deckgl_to_json

  data_dependency <- if (identical(data_transport, "file")) {
    .deckgl_data_dependency(.deckgl_recorded_data_files(), data_dir, uid)
  } else NULL

  htmlwidgets::createWidget(
    name = "deckgl",
    x = widget_data,
    width = width,
    height = height,
    package = "rDeckgl",
    dependencies = if (is.null(data_dependency)) NULL else list(data_dependency),
    preRenderHook = .deckgl_pre_render,
    sizingPolicy = htmlwidgets::sizingPolicy(browser.fill = TRUE)
  )
}


# Files written for `data_transport = "file"` during one deckgl() call. The
# widget must ship them as an html dependency attachment, otherwise a relative
# URL only resolves when the page happens to be served from `data_dir`.
# An auto-created data directory is scoped to the Shiny session that renders the
# widget: one directory per session instead of one per render, removed when the
# session ends. Outside Shiny it is a session temporary directory as before.
# Re-rendering a widget still writes a new payload into that directory; the
# previous one is removed with the directory when the session ends.
.deckgl_session_data_dir <- function(prefix) {
  session <- if (requireNamespace("shiny", quietly = TRUE)) shiny::getDefaultReactiveDomain() else NULL
  if (is.null(session)) return(tempfile(prefix))
  existing <- session$userData$.deckgl_data_dir
  if (!is.null(existing) && dir.exists(existing)) return(existing)
  path <- tempfile(prefix)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  session$userData$.deckgl_data_dir <- path
  session$onSessionEnded(function() unlink(path, recursive = TRUE))
  path
}

.deckgl_data_file_registry <- new.env(parent = emptyenv())
.deckgl_data_file_registry$files <- character()

.deckgl_reset_data_files <- function() {
  .deckgl_data_file_registry$files <- character()
  invisible(NULL)
}

.deckgl_record_data_file <- function(path) {
  .deckgl_data_file_registry$files <- c(.deckgl_data_file_registry$files, path)
  invisible(path)
}

.deckgl_recorded_data_files <- function() unique(.deckgl_data_file_registry$files)

# One dependency per widget, carrying its data files as attachments. htmltools
# copies attachments next to the page on save and print (RStudio Viewer), and
# Shiny serves the directory, so the relative URLs in the spec resolve in all
# three contexts.
.deckgl_data_dependency <- function(files, data_dir, uid) {
  files <- files[file.exists(files)]
  if (!length(files)) return(NULL)
  htmltools::htmlDependency(
    name = paste0("deckgl-data-", uid),
    version = as.character(utils::packageVersion("rDeckgl")),
    src = c(file = normalizePath(data_dir, winslash = "/", mustWork = TRUE)),
    attachment = stats::setNames(basename(files), paste0("data", seq_along(files))),
    all_files = FALSE
  )
}


#' Export a DuckDB query to Arrow IPC bytes via ADBC
#'
#' Uses the ADBC driver (adbcdrivermanager) to execute a query and capture
#' the result as raw Arrow IPC stream bytes. This preserves GeoArrow extension
#' metadata automatically in DuckDB >= 1.5.
#'
#' @param con A DBI connection to DuckDB (used to resolve the database path).
#' @param query SQL query string.
#' @return Raw vector of Arrow IPC stream bytes, or NULL on failure.
#' @keywords internal
.adbc_query_to_ipc_bytes <- function(con, query) {
  if (!requireNamespace("adbcdrivermanager", quietly = TRUE) ||
      !requireNamespace("nanoarrow", quietly = TRUE)) {
    return(NULL)
  }

  db_path <- tryCatch({
    info <- DBI::dbGetInfo(con)
    dbname <- info$dbname
    if (is.null(dbname)) dbname <- info$dbdir
    if (is.null(dbname)) dbname <- ":memory:"
    dbname
  }, error = function(e) ":memory:")

  tryCatch({
    adbc_drv <- duckdb::duckdb_adbc()
    adbc_db <- adbcdrivermanager::adbc_database_init(adbc_drv, path = db_path)
    adbc_con <- adbcdrivermanager::adbc_connection_init(adbc_db)
    on.exit({
      try(adbcdrivermanager::adbc_connection_release(adbc_con), silent = TRUE)
      try(adbcdrivermanager::adbc_database_release(adbc_db), silent = TRUE)
    }, add = TRUE)

    # Ensure spatial is loaded on the ADBC connection
    load_stmt <- adbcdrivermanager::adbc_statement_init(adbc_con)
    adbcdrivermanager::adbc_statement_set_sql_query(load_stmt, "INSTALL spatial")
    tryCatch(
      adbcdrivermanager::adbc_statement_execute_query(load_stmt),
      error = function(e) invisible(NULL)
    )
    adbcdrivermanager::adbc_statement_release(load_stmt)
    load_stmt2 <- adbcdrivermanager::adbc_statement_init(adbc_con)
    adbcdrivermanager::adbc_statement_set_sql_query(load_stmt2, "LOAD spatial")
    adbcdrivermanager::adbc_statement_execute_query(load_stmt2)
    adbcdrivermanager::adbc_statement_release(load_stmt2)

    stmt <- adbcdrivermanager::adbc_statement_init(adbc_con)
    adbcdrivermanager::adbc_statement_set_sql_query(stmt, query)
    stream <- nanoarrow::nanoarrow_allocate_array_stream()
    adbcdrivermanager::adbc_statement_execute_query(stmt, stream)
    tf <- tempfile(fileext = ".arrows")
    nanoarrow::write_nanoarrow(stream, tf, format = "stream")
    adbcdrivermanager::adbc_statement_release(stmt)
    raw_bytes <- readBin(tf, "raw", file.info(tf)$size)
    unlink(tf)
    raw_bytes
  }, error = function(e) {
    NULL
  })
}


#' Hydrate Deck.gl DuckDB data references
#'
#' Recursively walks a Deck.gl specification and replaces `type = "duckdb"`
#' data nodes with concrete result sets queried via the provided connection.
#'
#' @param spec Deck.gl specification as an R list.
#' @param con  A live DBI connection to DuckDB.
#' @param list_col_metadata Environment containing metadata about JSON-encoded list columns.
#' @param data_transport `"auto"` to use `"file"` when `data_dir` is supplied
#'   and `"inline"` otherwise, `"inline"` for base64 payloads, or `"file"` for
#'   relative Arrow/Parquet URLs.
#' @param data_dir Directory used by `"file"` transport.
#' @return A hydrated list that is safe to JSON-encode for Deck.gl.
#' @keywords internal
hydrate_deckgl_spec <- function(
    spec,
    con,
    list_col_metadata = NULL,
    data_transport = c("auto", "file", "inline"),
    data_dir = NULL) {
  data_transport <- match.arg(data_transport)
  if (identical(data_transport, "auto")) {
    data_transport <- if (!is.null(data_dir)) "file" else "inline"
  }
  transform_node <- function(node, inside_data = FALSE) {
    if (is.list(node)) {
      if (
        !is.null(node$type) && identical(node$type, "duckdb") && inside_data
      ) {
        if (is.null(con)) {
          stop(
            "Deck.gl spec includes DuckDB data but no active connection is available."
          )
        }
        query <- node$query
        if (!is.character(query) || length(query) < 1 || !nzchar(query[[1]])) {
          stop("Deck.gl DuckDB data nodes require a non-empty 'query' field.")
        }
        query <- query[[1]]
        fmt <- if (is.null(node$format)) "json" else tolower(node$format[[1]])

        if (fmt == "geoarrow") {
          # Strategy priority:
          # 1. COPY FORMAT ARROWS — uses the current DBI connection so it
          #    sees temporary tables (e.g. color map joins from GiottoDB).
          #    Requires the nanoarrow DuckDB extension.
          # 2. ADBC — opens a separate connection; fastest binary export but
          #    cannot see temporary tables from the DBI connection.
          # 3. DBI fetch + R arrow — universal fallback, may lose GeoArrow
          #    extension metadata.
          
          # Ensure GeoArrow-capable extensions are loaded on the DBI connection
          try(DBI::dbExecute(con, "INSTALL nanoarrow FROM community"), silent = TRUE)
          try(DBI::dbExecute(con, "LOAD nanoarrow"), silent = TRUE)
          try(DBI::dbExecute(con, "CALL register_geoarrow_extensions()"), silent = TRUE)
          
          # 1) Try COPY FORMAT ARROWS (same connection, sees temp tables)
          temp_arrow <- tempfile(fileext = ".arrows")
          on.exit(unlink(temp_arrow), add = TRUE)
          
          copy_result <- tryCatch({
            DBI::dbExecute(con, sprintf(
              "COPY (%s) TO '%s' (FORMAT ARROWS)",
              query, temp_arrow
            ))
            raw_bytes <- readBin(temp_arrow, "raw", file.info(temp_arrow)$size)
            .deckgl_arrow_data_node(raw_bytes, data_transport, data_dir, "geoarrow", TRUE)
          }, error = function(e) NULL)
          
          if (!is.null(copy_result)) return(copy_result)
          
          # 2) Try ADBC (separate connection — only works for persistent tables)
          adbc_bytes <- .adbc_query_to_ipc_bytes(con, query)
          if (!is.null(adbc_bytes) && length(adbc_bytes) > 0) {
            return(.deckgl_arrow_data_node(adbc_bytes, data_transport, data_dir, "geoarrow", TRUE))
          }
          
          # 3) Fallback: DBI fetch + R arrow package
          result <- tryCatch({
            df <- DBI::dbGetQuery(con, query)
            if (!is.data.frame(df) || nrow(df) == 0) {
              return(list(`__arrow` = "", `__arrow_format` = "stream"))
            }
            df[] <- lapply(df, function(col) {
              if (is.factor(col)) as.character(col) else col
            })
            arrow_table <- arrow::as_arrow_table(df)
            raw_bytes <- arrow::write_to_raw(arrow_table, format = "stream")
            .deckgl_arrow_data_node(raw_bytes, data_transport, data_dir, "geoarrow")
          }, error = function(e) {
            warning("All GeoArrow export methods failed: ", e$message)
            list(`__arrow` = "", `__arrow_format` = "stream")
          })
          return(result)
        } else if (fmt %in% c("geoparquet", "parquet")) {
          # Export to Parquet, then re-read via parquet_scan and export Arrow stream
          temp_parquet <- tempfile(fileext = ".parquet")
          temp_arrow <- tempfile(fileext = ".arrows")
          on.exit(unlink(c(temp_parquet, temp_arrow)), add = TRUE)
          
          result <- tryCatch({
            DBI::dbExecute(con, sprintf(
              "COPY (%s) TO '%s' (FORMAT PARQUET)",
              query, temp_parquet
            ))
            if (identical(data_transport, "file")) {
              target <- file.path(
                data_dir,
                sprintf("deckgl_parquet_%08x.parquet", sample.int(.Machine$integer.max, 1L))
              )
              dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
              if (!file.copy(temp_parquet, target, overwrite = TRUE)) {
                stop("Failed to write Parquet data file: ", target, call. = FALSE)
              }
              .deckgl_record_data_file(target)
              return(list(
                `__parquet_url` = basename(target),
                `__geoarrow` = TRUE
              ))
            }
            DBI::dbExecute(con, sprintf(
              "COPY (SELECT * FROM parquet_scan('%s')) TO '%s' (FORMAT ARROWS)",
              temp_parquet, temp_arrow
            ))
            raw_bytes <- readBin(temp_arrow, "raw", file.info(temp_arrow)$size)
            .deckgl_arrow_data_node(raw_bytes, data_transport, data_dir, "geoarrow", TRUE)
          }, error = function(e) {
            warning("GeoParquet export failed: ", e$message)
            list(`__arrow` = "", `__arrow_format` = "stream")
          })
          return(result)
        } else if (fmt == "arrow") {
          # Regular Arrow format without GeoArrow metadata. DuckDB writes the
          # result itself, so rows never enter R.
          return(.deckgl_export_arrow_data_node(con, query, data_transport, data_dir))
        } else {
          # Default JSON format
          df <- DBI::dbGetQuery(con, query)
          if (!is.data.frame(df) || nrow(df) == 0) {
            return(list())
          }

          # Parse JSON-encoded list columns back to nested lists
          # Check if metadata environment has info about JSON-encoded columns
          if (!is.null(list_col_metadata) && length(list_col_metadata) > 0) {
            # Extract table names from query (simple heuristic: look for FROM clause)
            query_upper <- toupper(query)
            for (table_name in ls(list_col_metadata)) {
              if (grepl(toupper(table_name), query_upper, fixed = TRUE)) {
                json_cols <- list_col_metadata[[table_name]]
                for (col_name in json_cols) {
                  if (col_name %in% names(df)) {
                    df[[col_name]] <- lapply(df[[col_name]], function(json_str) {
                      if (is.na(json_str) || json_str == "") return(NULL)
                      jsonlite::fromJSON(json_str, simplifyVector = FALSE)
                    })
                  }
                }
              }
            }
          }

          # Convert factors to characters for safe JSON serialization
          df[] <- lapply(df, function(col) {
            if (is.factor(col)) as.character(col) else col
          })

          # Convert to row-oriented format, preserving list columns
          rows <- .deckgl_hydrate_rows(df)
          attr(rows, "rdeckgl_json_rows") <- TRUE
          return(rows)
        }
      }

      if (is.null(names(node))) {
        return(lapply(node, transform_node, inside_data = inside_data))
      }

      result <- node
      for (nm in names(result)) {
        result[[nm]] <- transform_node(
          result[[nm]],
          inside_data = inside_data || identical(nm, "data")
        )
      }
      return(result)
    }

    node
  }

  transform_node(spec, inside_data = FALSE)
}

# One statement only. The check is deliberately conservative: a ';' anywhere
# other than the end (including inside a string literal) is refused rather than
# parsed, because the export ladder hands the text to several engines.
.deckgl_single_statement <- function(query) {
  if (!is.character(query) || length(query) != 1L) {
    stop("A DuckDB data node needs a single SQL string.", call. = FALSE)
  }
  trimmed <- sub("[[:space:];]+$", "", query)
  if (grepl(";", trimmed, fixed = TRUE)) {
    stop("A DuckDB data node must contain a single SQL statement; ",
         "remove the ';' inside the query.", call. = FALSE)
  }
  trimmed
}

.deckgl_data_file_stem <- function(data_dir, prefix) {
  if (!is.character(data_dir) || length(data_dir) != 1L || !nzchar(data_dir)) {
    stop("'data_dir' is required for file transport.", call. = FALSE)
  }
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  file.path(
    data_dir,
    sprintf("deckgl_%s_%08x", prefix, sample.int(.Machine$integer.max, 1L))
  )
}

.deckgl_write_data_file <- function(raw_bytes, data_dir, prefix, ext) {
  path <- paste0(.deckgl_data_file_stem(data_dir, prefix), ext)
  writeBin(raw_bytes, path)
  .deckgl_record_data_file(path)
  list(`__arrow_url` = basename(path))
}

# Export a query result straight from DuckDB into a binary file. Rows never
# pass through R: each rung streams from the database engine, and the ladder
# only descends when a rung is unavailable on this DuckDB build.
#   1. COPY (FORMAT ARROWS): needs the community nanoarrow extension.
#   2. Arrow record-batch streaming through arrow::write_ipc_stream().
#   3. COPY (FORMAT PARQUET): always available.
.deckgl_export_query_file <- function(con, query, path_stem) {
  # COPY wraps the query in parentheses, so a trailing semicolon would break the
  # first rung for a cosmetic reason. Anything beyond one statement is refused:
  # the COPY rung would fail to parse and the record-batch rung would execute
  # every statement on the caller's connection and export the last result.
  query <- .deckgl_single_statement(query)
  failures <- character()
  attempt <- function(method, format, export) {
    path <- paste0(path_stem, ".", format)
    ok <- tryCatch({
      export(path)
      TRUE
    }, error = function(e) {
      failures[[method]] <<- conditionMessage(e)
      FALSE
    })
    if (!ok) {
      # A failed rung may leave a partial file behind.
      unlink(path)
      return(NULL)
    }
    list(path = path, format = format, method = method)
  }

  # Loading is a probe, not an install: a user-supplied connection keeps its
  # extension set, and a missing extension simply skips the first rung.
  nanoarrow_loaded <- tryCatch({
    DBI::dbExecute(con, "LOAD nanoarrow")
    TRUE
  }, error = function(e) {
    failures[["copy_arrows"]] <<- paste("LOAD nanoarrow:", conditionMessage(e))
    FALSE
  })
  if (nanoarrow_loaded) {
    result <- attempt("copy_arrows", "arrows", function(path) {
      DBI::dbExecute(con, sprintf(
        "COPY (%s) TO %s (FORMAT ARROWS)", query, DBI::dbQuoteString(con, path)
      ))
    })
    if (!is.null(result)) return(result)
  }

  result <- attempt("record_batch_stream", "arrows", function(path) {
    res <- DBI::dbSendQuery(con, query, arrow = TRUE)
    on.exit(DBI::dbClearResult(res), add = TRUE)
    arrow::write_ipc_stream(duckdb::duckdb_fetch_record_batch(res), path)
  })
  if (!is.null(result)) return(result)

  result <- attempt("copy_parquet", "parquet", function(path) {
    DBI::dbExecute(con, sprintf(
      "COPY (%s) TO %s (FORMAT PARQUET)", query, DBI::dbQuoteString(con, path)
    ))
  })
  if (!is.null(result)) return(result)

  stop(
    "Failed to export the DuckDB query result as Arrow or Parquet. ",
    paste(sprintf("%s: %s", names(failures), failures), collapse = "; "),
    call. = FALSE
  )
}

# Build the data node for format = "arrow". File transport exports directly
# into data_dir; inline transport exports to a temporary file, then embeds
# its bytes. Neither path fetches rows into R.
.deckgl_export_arrow_data_node <- function(con, query, data_transport, data_dir) {
  if (identical(data_transport, "file")) {
    exported <- .deckgl_export_query_file(
      con, query, .deckgl_data_file_stem(data_dir, "arrow")
    )
    .deckgl_record_data_file(exported$path)
    node <- if (identical(exported$format, "arrows")) {
      list(`__arrow_url` = basename(exported$path), `__arrow_format` = "stream")
    } else {
      list(`__parquet_url` = basename(exported$path))
    }
  } else {
    exported <- .deckgl_export_query_file(con, query, tempfile("deckgl_arrow_"))
    on.exit(unlink(exported$path), add = TRUE)
    raw_bytes <- readBin(exported$path, "raw", file.info(exported$path)$size)
    node <- if (identical(exported$format, "arrows")) {
      .deckgl_arrow_data_node(raw_bytes, "inline", NULL, "arrow")
    } else {
      list(`__parquet` = base64enc::base64encode(raw_bytes))
    }
  }
  node$`__export_method` <- exported$method
  node
}

.deckgl_arrow_data_node <- function(raw_bytes, data_transport, data_dir, prefix, geoarrow = FALSE) {
  out <- if (identical(data_transport, "file")) {
    c(.deckgl_write_data_file(raw_bytes, data_dir, prefix, ".arrows"), list(`__arrow_format` = "stream"))
  } else {
    list(`__arrow` = base64enc::base64encode(raw_bytes), `__arrow_format` = "stream")
  }
  if (geoarrow) out$`__geoarrow` <- TRUE
  out
}
.deckgl_arrow_response_message <- function(session, uid, request, raw_bytes, data_format) {
  if (is.function(session$registerDataObj)) {
    data_url <- session$registerDataObj(
      paste0(uid, "_", request),
      raw_bytes,
      function(data, req) list(
        status = 200L,
        headers = list(
          "Content-Type" = "application/vnd.apache.arrow.stream",
          "Content-Length" = as.character(length(data))
        ),
        body = data
      )
    )
    return(list(request = request, dataUrl = data_url, dataFormat = data_format))
  }
  list(
    request = request,
    data = base64enc::base64encode(raw_bytes),
    dataFormat = data_format
  )
}
