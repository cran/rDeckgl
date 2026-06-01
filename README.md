# rDeckgl

R bindings for [deck.gl](https://deck.gl) 9.2.2. The package wraps the deck.gl
JSON interface inside an [htmlwidget](https://www.htmlwidgets.org/), hydrates
data through DuckDB, and ships Shiny bindings so you can drop interactive
WebGL maps and large-data visualizations into any R workflow.

## Installation

Once on CRAN:

```r
install.packages("rDeckgl")
```

Development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("TiRizvanov/rDeckgl")
```

The deck.gl 9.2.2 JavaScript bundle, JSON converter, default loaders, CSS, and
widget glue are pre-built and shipped under `inst/htmlwidgets/`, so no Node or
JavaScript tooling is required at runtime.

## Quick start

```r
library(rDeckgl)

spec <- list(
  `@@type` = "DeckGL",
  initialViewState = list(longitude = -122.4, latitude = 37.76, zoom = 12),
  layers = list(
    list(
      `@@type` = "ScatterplotLayer",
      id   = "points",
      data = list(type = "duckdb", query = "SELECT lon, lat, radius FROM points"),
      getPosition  = "@@=[lon, lat]",
      getRadius    = "@@=radius",
      getFillColor = c(255, 0, 0)
    )
  )
)

data <- list(
  points = data.frame(
    lon    = c(-122.40, -122.45, -122.35),
    lat    = c( 37.76,   37.78,   37.74),
    radius = c(100,      150,     200)
  )
)

deckgl(spec, data = data, width = "100%", height = "500px")
```

`deckgl()` accepts deck.gl JSON specs as R lists, JSON strings, YAML strings,
or file paths; the format is auto-detected. Data passed via the `data`
argument is registered as DuckDB tables that your `type = "duckdb"` data
nodes can query.

## Shiny integration

```r
library(shiny)
library(rDeckgl)

ui <- fluidPage(
  deckglOutput("map", width = "100%", height = "600px")
)

server <- function(input, output, session) {
  output$map <- renderDeckgl({
    deckgl(spec, data = data)
  })
}

shinyApp(ui, server)
```

## ggsql

`ggsql()` exposes the same renderer through the ggsql dialect — describe a
deck.gl visualization with `VISUALIZE` / `DRAW` / `PLACE` / `SCALE` clauses
instead of hand-writing JSON. See `?ggsql` for the full reference.

## Learning more

- Vignette: `vignette("getting-started", package = "rDeckgl")`
- Function reference: `?deckgl`, `?deckglOutput`, `?renderDeckgl`, `?ggsql`

## Contributing

Issues and pull requests are welcome on
[GitHub](https://github.com/TiRizvanov/rDeckgl/issues). Please report bugs to
the package maintainer listed in `DESCRIPTION`.

## License

MIT — see [LICENSE](LICENSE) for the full text, including the third-party
deck.gl license bundled with the JavaScript assets.

## Acknowledgements

Developed in the [Dries Lab](https://www.drieslab.com/) at Boston University.
Funding support was provided by the Dries Lab and the Boston University
Undergraduate Research Opportunities Program (UROP).
