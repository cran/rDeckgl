# R/ggsql.R
#
# Public entry point for rendering a ggsql query as a Deck.gl widget.
# Compiles the IR produced by [.parse_ggsql()] into a Deck.gl JSON spec
# (with DuckDB-backed data nodes) and hands it to [deckgl()].

#' Render a ggsql query as a Deck.gl visualization
#'
#' `ggsql()` lets you describe a Deck.gl visualization using the
#' ggsql dialect (VISUALIZE / DRAW / PLACE / SCALE / LABEL / SETTING)
#' instead of the native Deck.gl spec. The parser is shared with
#' rMosaic; this entry point compiles for the Deck.gl rendering path,
#' which is the right choice for spatial layers (polygons, hex grids,
#' big point clouds, GeoArrow data).
#'
#' @param sql A character scalar containing ggsql.
#' @param data Optional named list of data.frames to register in the
#'   widget's DuckDB before rendering. Use this when the FROM source
#'   in the SQL is not already a table the widget's DuckDB can see.
#' @param con Optional DuckDB connection. If supplied it is reused
#'   instead of a fresh one (mirrors the `deckgl()` argument). Useful
#'   for GiottoDB / dbProject workflows.
#' @param width,height Optional widget dimensions.
#' @param ... Reserved for forward compatibility.
#' @return An htmlwidget produced by [deckgl()].
#' @seealso [deckgl()].
#' @export
ggsql <- function(sql, data = NULL, con = NULL,
                  width = NULL, height = NULL, ...) {
  ir <- .parse_ggsql(sql)
  if (is.null(ir)) {
    stop("Input contains no VISUALIZE clause; nothing to render.")
  }
  spec <- .ggsql_compile_deckgl(ir, width = width, height = height)
  deckgl(
    spec = spec,
    specType = "json",
    data = data,
    con = con,
    width = width %||% spec$width,
    height = height %||% spec$height
  )
}

# Compile IR -> Deck.gl spec list ---------------------------------------------

.ggsql_compile_deckgl <- function(ir, width = NULL, height = NULL) {
  coord <- tolower(ir$settings[["coord"]] %||% "geo")
  if (!coord %in% c("geo", "cartesian", "orthographic")) {
    stop(sprintf(
      "SETTING coord => '%s' is not recognised; use 'geo' or 'cartesian'.",
      coord
    ))
  }
  is_cartesian <- coord %in% c("cartesian", "orthographic")

  view <- .ggsql_deckgl_view(ir, is_cartesian)

  layers <- list()
  for (i in seq_along(ir$layers)) {
    layer <- ir$layers[[i]]
    layer_id <- sprintf("layer_%d", i)
    if (identical(layer$kind, "place")) {
      compiled <- .ggsql_deckgl_place(layer, layer_id, ir, is_cartesian)
    } else {
      compiled <- .ggsql_deckgl_draw(layer, layer_id, ir, is_cartesian)
    }
    if (!is.null(compiled)) layers[[length(layers) + 1L]] <- compiled
  }
  if (!length(layers)) {
    stop("ggsql query produced no Deck.gl layers.")
  }

  spec <- list(
    `@@type` = "DeckGL",
    initialViewState = .ggsql_deckgl_view_state(ir, is_cartesian),
    views = list(view),
    layers = layers
  )

  if (!is.null(ir$settings[["tooltip"]])) {
    spec$tooltip <- list(html = ir$settings[["tooltip"]])
  }
  if (!is.null(ir$settings[["pickable"]])) {
    # surface as a default applied to each layer
    for (i in seq_along(spec$layers)) {
      spec$layers[[i]]$pickable <-
        spec$layers[[i]]$pickable %||% ir$settings[["pickable"]]
    }
  }

  if (!is.null(width)) spec$width <- width
  if (!is.null(height)) spec$height <- height
  if (!is.null(ir$settings[["width"]])) spec$width <- ir$settings[["width"]]
  if (!is.null(ir$settings[["height"]])) spec$height <- ir$settings[["height"]]
  if (is.null(spec$width)) spec$width <- 800
  if (is.null(spec$height)) spec$height <- 600
  spec
}

.ggsql_deckgl_view <- function(ir, is_cartesian) {
  if (is_cartesian) {
    list(
      `@@type` = "OrthographicView",
      controller = TRUE,
      flipY = ir$settings[["flipy"]] %||% TRUE
    )
  } else {
    out <- list(
      `@@type` = "MapView",
      controller = TRUE
    )
    if (!is.null(ir$settings[["basemap"]])) {
      out$mapStyle <- .ggsql_deckgl_basemap(ir$settings[["basemap"]])
    }
    out
  }
}

.ggsql_deckgl_basemap <- function(name) {
  if (grepl("^https?://", name)) return(name)
  switch(
    tolower(name),
    "dark" = "https://basemaps.cartocdn.com/gl/dark-matter-gl-style/style.json",
    "dark-nolabels" =
      "https://basemaps.cartocdn.com/gl/dark-matter-nolabels-gl-style/style.json",
    "light" = "https://basemaps.cartocdn.com/gl/positron-gl-style/style.json",
    "voyager" = "https://basemaps.cartocdn.com/gl/voyager-gl-style/style.json",
    name
  )
}

.ggsql_deckgl_view_state <- function(ir, is_cartesian) {
  if (is_cartesian) {
    list(
      target = ir$settings[["target"]] %||% list(0, 0, 0),
      zoom = ir$settings[["zoom"]] %||% 0,
      minZoom = ir$settings[["minzoom"]] %||% -10,
      maxZoom = ir$settings[["maxzoom"]] %||% 10
    )
  } else {
    list(
      longitude = ir$settings[["longitude"]] %||% 0,
      latitude = ir$settings[["latitude"]] %||% 0,
      zoom = ir$settings[["zoom"]] %||% 1,
      pitch = ir$settings[["pitch"]] %||% 0,
      bearing = ir$settings[["bearing"]] %||% 0
    )
  }
}

.ggsql_deckgl_select_columns <- function(ir, extra = character()) {
  cols <- unique(c(unlist(ir$aesthetics), extra))
  cols <- cols[nzchar(cols)]
  if (!length(cols)) "*" else paste(cols, collapse = ", ")
}

.ggsql_deckgl_source_query <- function(ir, columns = NULL) {
  col_expr <- columns %||% .ggsql_deckgl_select_columns(ir)
  if (!is.null(ir$base_sql)) {
    sprintf("WITH vis_source AS (%s) SELECT %s FROM vis_source",
            ir$base_sql, col_expr)
  } else if (!is.null(ir$from)) {
    sprintf("SELECT %s FROM %s", col_expr, ir$from)
  } else {
    stop(
      "VISUALIZE requires a data source: either a SELECT preceding ",
      "VISUALIZE, or `FROM <table>` after the mappings."
    )
  }
}

.ggsql_deckgl_position_accessor <- function(ir) {
  x <- ir$aesthetics[["x"]]
  y <- ir$aesthetics[["y"]]
  if (is.null(x) || is.null(y)) {
    stop("DRAW point requires x and y aesthetics in VISUALIZE.")
  }
  if (!is.null(ir$aesthetics[["z"]])) {
    sprintf("@@=[%s, %s, %s]", x, y, ir$aesthetics[["z"]])
  } else {
    sprintf("@@=[%s, %s]", x, y)
  }
}

.ggsql_deckgl_color_accessor <- function(ir, layer, default_rgb = c(80, 144, 224)) {
  settings <- layer$settings
  if (!is.null(settings[["fill_color"]])) return(settings[["fill_color"]])
  if (!is.null(settings[["getfillcolor"]])) return(settings[["getfillcolor"]])
  if (!is.null(settings[["fill"]])) {
    return(.ggsql_deckgl_hex_to_rgb(settings[["fill"]]))
  }
  rgb_cols <- .ggsql_deckgl_rgb_columns(ir$aesthetics[["color"]] %||% ir$aesthetics[["fill"]])
  if (!is.null(rgb_cols)) {
    return(sprintf("@@=[%s, %s, %s, %d]", rgb_cols[1], rgb_cols[2],
                   rgb_cols[3], as.integer(settings[["alpha"]] %||% 200)))
  }
  c(default_rgb, as.integer(settings[["alpha"]] %||% 200))
}

.ggsql_deckgl_rgb_columns <- function(col) {
  if (is.null(col)) return(NULL)
  if (grepl(",", col)) {
    parts <- trimws(strsplit(col, ",", fixed = TRUE)[[1]])
    if (length(parts) == 3) return(parts)
  }
  NULL
}

.ggsql_deckgl_hex_to_rgb <- function(x) {
  if (is.numeric(x) && length(x) %in% c(3, 4)) return(as.integer(x))
  if (is.character(x) && grepl("^#?[0-9a-fA-F]{6}$", x)) {
    h <- sub("^#", "", x)
    return(c(
      strtoi(substr(h, 1, 2), 16L),
      strtoi(substr(h, 3, 4), 16L),
      strtoi(substr(h, 5, 6), 16L),
      200L
    ))
  }
  x
}

.ggsql_deckgl_draw <- function(layer, id, ir, is_cartesian) {
  type <- layer$type
  settings <- layer$settings
  if (type %in% c("point", "dot", "scatterplot")) {
    out <- list(
      `@@type` = "ScatterplotLayer",
      id = id,
      data = list(
        type = "duckdb",
        query = .ggsql_deckgl_source_query(ir)
      ),
      getPosition = .ggsql_deckgl_position_accessor(ir),
      pickable = settings[["pickable"]] %||% TRUE
    )
    fill <- .ggsql_deckgl_color_accessor(ir, layer)
    out$getFillColor <- fill
    if (!is.null(ir$aesthetics[["size"]])) {
      out$getRadius <- sprintf("@@=%s", ir$aesthetics[["size"]])
    } else if (!is.null(settings[["radius"]])) {
      out$getRadius <- settings[["radius"]]
    }
    if (!is.null(settings[["radiusunits"]])) out$radiusUnits <- settings[["radiusunits"]]
    if (!is.null(settings[["radiusminpixels"]])) out$radiusMinPixels <- settings[["radiusminpixels"]]
    if (!is.null(settings[["radiusmaxpixels"]])) out$radiusMaxPixels <- settings[["radiusmaxpixels"]]
    if (is_cartesian) {
      out$coordinateSystem <- "@@#COORDINATE_SYSTEM.CARTESIAN"
      out$coordinateOrigin <- settings[["coordinateorigin"]] %||% c(0, 0, 0)
      out$positionFormat <- settings[["positionformat"]] %||% "XY"
    }
    out
  } else if (type %in% c("polygon", "polygons")) {
    poly_col <- ir$aesthetics[["polygon"]] %||% settings[["polygon"]] %||% "polygon"
    format_hint <- tolower(settings[["format"]] %||% "auto")
    use_geoarrow <- format_hint %in% c("geoarrow", "arrow", "geoparquet")
    out <- list(
      id = id,
      data = list(
        type = "duckdb",
        query = .ggsql_deckgl_source_query(
          ir,
          .ggsql_deckgl_select_columns(ir, extra = poly_col)
        )
      ),
      pickable = settings[["pickable"]] %||% TRUE,
      filled = settings[["filled"]] %||% TRUE,
      stroked = settings[["stroked"]] %||% TRUE
    )
    if (use_geoarrow) {
      out$`@@type` <- settings[["layer"]] %||% "GeoArrowSolidPolygonLayer"
      out$data$format <- "geoarrow"
    } else {
      out$`@@type` <- "PolygonLayer"
      out$getPolygon <- sprintf("@@=%s", poly_col)
      out$positionFormat <- settings[["positionformat"]] %||% "XY"
    }
    fill <- .ggsql_deckgl_color_accessor(ir, layer, default_rgb = c(66, 135, 245))
    out$getFillColor <- fill
    out$getLineColor <- settings[["line_color"]] %||% c(25, 64, 155, 200)
    if (!is.null(settings[["linewidth"]])) {
      out$lineWidthMinPixels <- settings[["linewidth"]]
    } else {
      out$lineWidthMinPixels <- 1
    }
    if (is_cartesian) {
      out$coordinateSystem <- "@@#COORDINATE_SYSTEM.CARTESIAN"
      out$coordinateOrigin <- settings[["coordinateorigin"]] %||% c(0, 0, 0)
    }
    if (!is.null(ir$aesthetics[["elevation"]])) {
      out$getElevation <- sprintf("@@=%s", ir$aesthetics[["elevation"]])
      out$extruded <- settings[["extruded"]] %||% TRUE
    }
    out
  } else if (type %in% c("hexgrid", "hexagon", "hex")) {
    out <- list(
      `@@type` = "HexagonLayer",
      id = id,
      data = list(
        type = "duckdb",
        query = .ggsql_deckgl_source_query(ir)
      ),
      getPosition = .ggsql_deckgl_position_accessor(ir),
      radius = settings[["radius"]] %||% 1000,
      coverage = settings[["coverage"]] %||% 1,
      pickable = settings[["pickable"]] %||% TRUE,
      extruded = settings[["extruded"]] %||% TRUE
    )
    if (!is.null(ir$aesthetics[["elevation"]]) ||
        !is.null(ir$aesthetics[["weight"]])) {
      wcol <- ir$aesthetics[["elevation"]] %||% ir$aesthetics[["weight"]]
      out$getElevationWeight <- sprintf("@@=%s", wcol)
      out$getColorWeight <- sprintf("@@=%s", wcol)
    }
    if (!is.null(settings[["elevationscale"]])) out$elevationScale <- settings[["elevationscale"]]
    if (!is.null(settings[["elevationrange"]])) out$elevationRange <- settings[["elevationrange"]]
    if (!is.null(settings[["colorrange"]])) out$colorRange <- settings[["colorrange"]]
    if (is_cartesian) {
      out$coordinateSystem <- "@@#COORDINATE_SYSTEM.CARTESIAN"
      out$coordinateOrigin <- settings[["coordinateorigin"]] %||% c(0, 0, 0)
    }
    out
  } else if (type %in% c("heatmap", "density")) {
    out <- list(
      `@@type` = "HeatmapLayer",
      id = id,
      data = list(
        type = "duckdb",
        query = .ggsql_deckgl_source_query(ir)
      ),
      getPosition = .ggsql_deckgl_position_accessor(ir),
      pickable = settings[["pickable"]] %||% FALSE,
      radiusPixels = settings[["radiuspixels"]] %||% 30
    )
    if (!is.null(ir$aesthetics[["weight"]])) {
      out$getWeight <- sprintf("@@=%s", ir$aesthetics[["weight"]])
    }
    if (!is.null(settings[["colorrange"]])) out$colorRange <- settings[["colorrange"]]
    if (is_cartesian) {
      out$coordinateSystem <- "@@#COORDINATE_SYSTEM.CARTESIAN"
      out$coordinateOrigin <- settings[["coordinateorigin"]] %||% c(0, 0, 0)
    }
    out
  } else if (type %in% c("path", "line")) {
    path_col <- ir$aesthetics[["path"]] %||% settings[["path"]] %||% "path"
    out <- list(
      `@@type` = "PathLayer",
      id = id,
      data = list(
        type = "duckdb",
        query = .ggsql_deckgl_source_query(
          ir,
          .ggsql_deckgl_select_columns(ir, extra = path_col)
        )
      ),
      getPath = sprintf("@@=%s", path_col),
      pickable = settings[["pickable"]] %||% TRUE,
      widthMinPixels = settings[["linewidth"]] %||% 1
    )
    fill <- .ggsql_deckgl_color_accessor(ir, layer)
    out$getColor <- fill
    if (is_cartesian) {
      out$coordinateSystem <- "@@#COORDINATE_SYSTEM.CARTESIAN"
      out$coordinateOrigin <- settings[["coordinateorigin"]] %||% c(0, 0, 0)
      out$positionFormat <- settings[["positionformat"]] %||% "XY"
    }
    out
  } else if (type == "arc") {
    x1 <- ir$aesthetics[["x"]]; y1 <- ir$aesthetics[["y"]]
    x2 <- ir$aesthetics[["xend"]] %||% ir$aesthetics[["x2"]]
    y2 <- ir$aesthetics[["yend"]] %||% ir$aesthetics[["y2"]]
    if (is.null(x2) || is.null(y2)) {
      stop("DRAW arc requires xend/yend aesthetics in VISUALIZE.")
    }
    out <- list(
      `@@type` = "ArcLayer",
      id = id,
      data = list(
        type = "duckdb",
        query = .ggsql_deckgl_source_query(ir)
      ),
      getSourcePosition = sprintf("@@=[%s, %s]", x1, y1),
      getTargetPosition = sprintf("@@=[%s, %s]", x2, y2),
      pickable = settings[["pickable"]] %||% TRUE,
      getWidth = settings[["linewidth"]] %||% 2
    )
    out$getSourceColor <- .ggsql_deckgl_color_accessor(ir, layer)
    out$getTargetColor <- settings[["target_color"]] %||% out$getSourceColor
    if (is_cartesian) {
      out$coordinateSystem <- "@@#COORDINATE_SYSTEM.CARTESIAN"
      out$coordinateOrigin <- settings[["coordinateorigin"]] %||% c(0, 0, 0)
    }
    out
  } else if (type == "text") {
    text_expr <- ir$aesthetics[["label"]] %||% settings[["text"]]
    if (is.null(text_expr)) {
      stop("DRAW text requires a `label` aesthetic or SETTING text => '<col>'.")
    }
    out <- list(
      `@@type` = "TextLayer",
      id = id,
      data = list(
        type = "duckdb",
        query = .ggsql_deckgl_source_query(ir, .ggsql_deckgl_select_columns(ir, extra = text_expr))
      ),
      getPosition = .ggsql_deckgl_position_accessor(ir),
      getText = sprintf("@@=String(%s)", text_expr),
      pickable = settings[["pickable"]] %||% FALSE,
      sizeUnits = settings[["sizeunits"]] %||% "pixels",
      getSize = settings[["size"]] %||% 12
    )
    out$getColor <- .ggsql_deckgl_color_accessor(ir, layer, default_rgb = c(0, 0, 0))
    if (is_cartesian) {
      out$coordinateSystem <- "@@#COORDINATE_SYSTEM.CARTESIAN"
      out$coordinateOrigin <- settings[["coordinateorigin"]] %||% c(0, 0, 0)
    }
    out
  } else {
    stop(sprintf(
      "DRAW %s is not supported by the Deck.gl backend (try rMosaic::ggsql for non-spatial geoms).",
      layer$type
    ))
  }
}

.ggsql_deckgl_place <- function(layer, id, ir, is_cartesian) {
  type <- layer$type
  settings <- layer$settings
  if (type %in% c("text", "label")) {
    xs <- settings[["x"]] %||% 0
    ys <- settings[["y"]] %||% 0
    txt <- settings[["text"]] %||% settings[["label"]] %||% ""
    rows <- if (length(xs) == length(ys) && length(xs) > 1) {
      sprintf("(%s, %s, '%s')",
              vapply(xs, format, character(1)),
              vapply(ys, format, character(1)),
              gsub("'", "''", as.character(txt)))
    } else {
      sprintf("(%s, %s, '%s')",
              format(xs[[1]]), format(ys[[1]]),
              gsub("'", "''", as.character(txt)))
    }
    sql <- sprintf(
      "SELECT * FROM (VALUES %s) AS t(x, y, label)",
      paste(rows, collapse = ", ")
    )
    out <- list(
      `@@type` = "TextLayer",
      id = id,
      data = list(type = "duckdb", query = sql),
      getPosition = "@@=[x, y]",
      getText = "@@=label",
      getColor = settings[["color"]] %||% c(0, 0, 0, 255),
      getSize = settings[["size"]] %||% 12,
      sizeUnits = "pixels"
    )
    if (is_cartesian) {
      out$coordinateSystem <- "@@#COORDINATE_SYSTEM.CARTESIAN"
      out$coordinateOrigin <- settings[["coordinateorigin"]] %||% c(0, 0, 0)
    }
    out
  } else {
    warning(sprintf(
      "PLACE %s is not supported by the Deck.gl backend; skipping.",
      type
    ))
    NULL
  }
}

`%||%` <- function(x, y) if (is.null(x)) y else x
