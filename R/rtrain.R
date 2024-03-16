#' rtrain
#'
#' Produces a random sample from the distribution, a histogram of the simulated values, and a plot of the objective function from `w-F` theory (`r-stem`)
#' Note also that using a specific value for w yields the "p-train" function (`p-stem`)
#'
#' @param n number of samples
#' @param w the value of w where W ~ Unif(0, 1)
#'
#' @return a random sample from the distribution, a histogram of the simulated values, and a plot of the objective function
#' @export
#'
#' @examples \dontrun{rtrain(n = 10000, w = 0.3)}
rtrain <- function(n, w = 0.3){

  x <- NULL
  obj <- function(n){

    w <- stats::runif(1)
    f <- function(x) {
      ptrain(x)-w
    }

    stats::uniroot(f, interval = c(-5,5))$root
  }
  v <- replicate(n, obj())

  graphics::curve(ptrain(x)-w, c(-5,5),
        xlab = "Y",
        ylab = "Objective function f",
        col = "hotpink",
        xlim = c(-5,5),
        lwd = 2
  )
  graphics::abline(h = 0,
         lwd = 2,
         col = "blue")

  h <- graphics::hist(v, plot = FALSE)
  ratio <- h$density/max(h$density)
  coll <- grDevices::rgb(0,0,ratio)
  graphics::hist(v,
       freq = FALSE,
       xlab = "Y",
       ylab = "Density",
       col = coll,
       main = "Simulation of Time a Train is Late"
  )
  graphics::curve(dtrain(x),
        add = TRUE,
        lwd = 3,
        col = "hotpink"
  )

  head_v <- as.character(utils::head(v, 10))
  list(head_v = head_v)
}
