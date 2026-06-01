# R/shiny.R

#' Shiny output for Deck.gl widget
#'
#' Use this in the UI to create a placeholder for the Deck.gl visualization.
#'
#' @param outputId output variable name
#' @param width,height CSS dimensions (e.g. '100\%', '400px') for the container.
#'
#' @return A Shiny output binding for use in the UI.
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(rDeckgl)
#'
#'   ui <- fluidPage(
#'     deckglOutput("myDeckgl", height = "600px")
#'   )
#'
#'   server <- function(input, output, session) {
#'     output$myDeckgl <- renderDeckgl({
#'       deckgl(spec = my_spec, data = my_data)
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' @export
deckglOutput <- function(outputId, width = "100%", height = "400px") {
  htmlwidgets::shinyWidgetOutput(
    outputId,
    "deckgl",
    width,
    height,
    package = "rDeckgl"
  )
}

#' Shiny render function for Deck.gl
#'
#' Use this in the server to render the Deck.gl visualization to the output.
#'
#' @param expr An expression that generates a call to \code{\link{deckgl}()}.
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @return A Shiny render function for use in the server.
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(rDeckgl)
#'
#'   ui <- fluidPage(
#'     deckglOutput("myDeckgl")
#'   )
#'
#'   server <- function(input, output, session) {
#'     output$myDeckgl <- renderDeckgl({
#'       spec <- list(
#'         `@@type` = "DeckGL",
#'         initialViewState = list(longitude = -122.4, latitude = 37.76, zoom = 12),
#'         layers = list(
#'           list(
#'             `@@type` = "ScatterplotLayer",
#'             id = "points",
#'             data = list(type = "duckdb", query = "SELECT * FROM points"),
#'             getPosition = "@@=[lon, lat]",
#'             getRadius = 100,
#'             getFillColor = c(255, 0, 0)
#'           )
#'         )
#'       )
#'       deckgl(spec = spec, data = list(points = data.frame(lon = -122.4, lat = 37.76)))
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' @export
renderDeckgl <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  }
  htmlwidgets::shinyRenderWidget(expr, deckglOutput, env, quoted = TRUE)
}
