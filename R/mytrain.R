#' mytrain
#'
#' This is the precursor to the shiny app included in this package. It produces a plot with shaded region representing the probability between inputs `a` and `b`
#'
#' @param a lower endpoint
#' @param b upper endpoint such that `P(a <= Y <= b) = P(Y = b) - P(Y = a)`
#'
#' @return a plot with shaded probability area and the probability
#' @export
#'
#' @examples \dontrun{mytrain(a = -2, b = 3)}
mytrain <- function(a, b){

  x <- NULL

  graphics::curve(dtrain(x),
        xlim = c(-10,10),
        col = "black",
        lwd = 3,
        main = "Time a Train Is Late",
        xlab = "Y",
        ylab = "Density")

  xcurve <- seq(a, b, length = 500)
  ycurve <- dtrain(xcurve)

  graphics::polygon(x = c(a, xcurve, b), y = c(0, ycurve, 0), col = "darkgreen")
  graphics::axis(side = 1, at = c(a, b))

  prob <- ptrain(b) - ptrain(a)
  dec <- round(prob, 4)

  graphics::text(5, 0.1, paste0("Area ="," ", dec))
}
