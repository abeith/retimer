#' extractPitchTier
#'
#' Extracts Praat PitchTier from wav object.
#'
#' @param x name of wav file
#' @param res resolution (Praat)
#' @param fmin minimum frequency (Praat)
#' @param fmax maximum frequency (Praat)
#' @param stylize stylize (Praat)
#' @param wd working directory for Praat to use
#'
#' @return Runs script in Praat and prints stdout to console.
#' @export
#'

extractPitchTier <- function(x = "A01", res = 0.1, fmin = 50, fmax =  250, stylize = 1, wd = getwd()) {

  if(grepl(".wav$", x)) x <- gsub(".wav$", "", x)

  praatScript(args = paste(x, res, fmin, fmax, stylize), script = "extractPitchTier.praat", wd = wd)
}
