#' @import shiny
#' @export run_curver
run_curver <- function(legacy = FALSE) {
  if (legacy) return(shinyApp(ui = shiny_UI, server = shiny_server))
  shiny::addResourcePath("curver", system.file("myapp/www", package="CuRveR"))
  shinyApp(ui = modern_ui(), server = modern_server)
}
