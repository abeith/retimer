#' findPeak
#'
#' Call Praat via system2()
#'
#' @param x a numeric vector
#' @param ... further arguements to be passed to `density`
#'
#' @return Returns the value of `x` that corresponds to the peak of the density curve.
#' @export
#'
#' @seealso density


findPeak <- function(x, ...){
  durDensity <- stats::density(x, ...)
  peakDensity <- durDensity$x[which.max(durDensity$y)]
  peakDensity
}
