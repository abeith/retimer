#' flatF0
#'
#' Flatten pitch
#'
#' @param filename name of wav file to flatten
#' @param .f function to use to determine pitch
#' @param wd working directory for praat to use
#'
#' @return Creates wav file with flat F0 contour.
#'
#' @seealso extractPitchTier

flatF0 <- function(filename, .f = findPeak, wd = getwd()){

  wd <- normalizePath(wd)

  # Extract Pitch Tier
  extractPitchTier(x = filename, wd = wd)

  # Read Pitch Tier
  pitchTier <- rPraat::pt.read(paste0(wd, "/", filename, ".PitchTier"))

  # Create new pitch tier
  newPitchTier <- pitchTier
  newPitchTier$t <- c(min(pitchTier$t), max(max(pitchTier$t)))
  newPitchTier$f <- c(rep(.f(pitchTier$f), 2))

  funName <- as.character(substitute(.f))

  # Write PT
  rPraat::pt.write(newPitchTier, paste0(wd, "/", filename, "_", funName, ".PitchTier"))

  # Resynthesise
  praatScript(args = paste(filename, funName), script = "reSynthPitch.praat", wd = "./data")

}
