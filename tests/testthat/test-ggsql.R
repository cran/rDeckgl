test_that("parser returns NULL when VISUALIZE is absent", {
  expect_null(rDeckgl:::.parse_ggsql("SELECT * FROM points"))
})

test_that("geo scatter compiles to ScatterplotLayer with MapView", {
  ir <- rDeckgl:::.parse_ggsql(
    "VISUALIZE lon AS x, lat AS y FROM points DRAW point"
  )
  spec <- rDeckgl:::.ggsql_compile_deckgl(ir)
  expect_equal(spec$views[[1]]$`@@type`, "MapView")
  expect_equal(spec$layers[[1]]$`@@type`, "ScatterplotLayer")
  expect_equal(spec$layers[[1]]$getPosition, "@@=[lon, lat]")
  expect_match(spec$layers[[1]]$data$query, "SELECT lon, lat FROM points")
})

test_that("cartesian SETTING switches to OrthographicView + CARTESIAN", {
  ir <- rDeckgl:::.parse_ggsql(
    "VISUALIZE x AS x, y AS y FROM cells SETTING coord => 'cartesian' DRAW point"
  )
  spec <- rDeckgl:::.ggsql_compile_deckgl(ir)
  expect_equal(spec$views[[1]]$`@@type`, "OrthographicView")
  expect_equal(spec$layers[[1]]$coordinateSystem,
               "@@#COORDINATE_SYSTEM.CARTESIAN")
})

test_that("polygon layer wires getPolygon accessor + line color", {
  ir <- rDeckgl:::.parse_ggsql(
    "VISUALIZE id AS id, polygon AS polygon FROM cells SETTING coord => 'cartesian' DRAW polygon"
  )
  spec <- rDeckgl:::.ggsql_compile_deckgl(ir)
  expect_equal(spec$layers[[1]]$`@@type`, "PolygonLayer")
  expect_equal(spec$layers[[1]]$getPolygon, "@@=polygon")
  expect_true(spec$layers[[1]]$filled)
})

test_that("polygon with format => 'geoarrow' picks GeoArrowSolidPolygonLayer", {
  ir <- rDeckgl:::.parse_ggsql(
    "VISUALIZE polygon AS polygon FROM cells DRAW polygon SETTING format => 'geoarrow'"
  )
  spec <- rDeckgl:::.ggsql_compile_deckgl(ir)
  expect_equal(spec$layers[[1]]$`@@type`, "GeoArrowSolidPolygonLayer")
  expect_equal(spec$layers[[1]]$data$format, "geoarrow")
})

test_that("hexgrid wires getElevationWeight from weight aesthetic", {
  ir <- rDeckgl:::.parse_ggsql(
    "VISUALIZE lng AS x, lat AS y, count AS weight FROM events DRAW hexgrid SETTING radius => 500"
  )
  spec <- rDeckgl:::.ggsql_compile_deckgl(ir)
  expect_equal(spec$layers[[1]]$`@@type`, "HexagonLayer")
  expect_equal(spec$layers[[1]]$getElevationWeight, "@@=count")
  expect_equal(spec$layers[[1]]$radius, 500)
})

test_that("base SELECT becomes a CTE in the duckdb query", {
  ir <- rDeckgl:::.parse_ggsql(
    "SELECT x, y FROM raw_points WHERE keep VISUALIZE x AS x, y AS y DRAW point"
  )
  spec <- rDeckgl:::.ggsql_compile_deckgl(ir)
  expect_match(spec$layers[[1]]$data$query,
               "WITH vis_source AS \\(SELECT x, y FROM raw_points WHERE keep\\)")
})

test_that("non-spatial DRAW types raise a helpful error", {
  ir <- rDeckgl:::.parse_ggsql(
    "VISUALIZE x AS x FROM t DRAW histogram"
  )
  expect_error(
    rDeckgl:::.ggsql_compile_deckgl(ir),
    "not supported by the Deck.gl backend"
  )
})

test_that("basemap alias resolves to known carto style", {
  ir <- rDeckgl:::.parse_ggsql(
    "VISUALIZE lon AS x, lat AS y FROM t SETTING basemap => 'dark' DRAW point"
  )
  spec <- rDeckgl:::.ggsql_compile_deckgl(ir)
  expect_match(spec$views[[1]]$mapStyle, "dark-matter")
})

test_that("hex fill literal becomes RGB tuple", {
  ir <- rDeckgl:::.parse_ggsql(
    "VISUALIZE x AS x, y AS y FROM t DRAW point SETTING fill => '#ff0000'"
  )
  spec <- rDeckgl:::.ggsql_compile_deckgl(ir)
  expect_equal(spec$layers[[1]]$getFillColor[1:3], c(255L, 0L, 0L))
})
