#' shinytrains
#'
#' An interactive shiny app which allows the user to engage a slider to change the values for which we'd like to know the probability between (area under) the plotted density curve. Further the app features the ability to change the values by "brushing".
#'
#'
#' @return a plot of the density with shaded area and displayed probability result
#' @export
#'
#' @examples \dontrun{shinytrains()}
shinytrains <- function() {
  shiny::runApp(system.file("shinytrains",
                            package = "MATH4753ROSAlab9"),
                launch.browser = TRUE)
}
