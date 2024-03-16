#' dtrain
#'
#' This is the density function f for the Train Problem (`d-stem`)
#'
#' @param y a number or vector
#'
#' @return the height of the density f(x)
#' @export
#'
#' @examples \dontrun{ dtrain(y = 1.96)}
dtrain <- function(y){

  ifelse(abs(y) <= 5, (3*(25-y^2))/500, 0)
}
