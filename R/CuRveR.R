#' @import shiny
#' @export run_curver
run_curver <- function(legacy = FALSE) {
  if (legacy) return(shinyApp(ui = shiny_UI, server = shiny_server))
  shinyApp(ui = modern_ui(), server = modern_server)
}
