#' ptrain
#'
#' This is the cumulative function F for the Train problem of f (`p-stem`)
#'
#' @param y a number or vector
#'
#' @return the lower-tail probability P(X <= x)
#' @export
#'
#' @examples \dontrun{ ptrain(y = 3)}
ptrain <- function(y) {

  ifelse(abs(y) <= 5, (250 + 75*y - y^3)/500, 0)
}
