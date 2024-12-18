#' flatF0
#'
#' Flatten pitch
#'
#' @param wav path to a wav file or a tuneR WAVE object
#' @param .f function to use to determine pitch. Default is findPeak which finds the mode of the existing pitch contour.
#' @param ... Additional arguments passed to extractPitchTier
#'
#' @return Returns a tuneR WAVE object of the input with a flat F0 contour
#'
#' @seealso extractPitchTier
#' @export

flatF0 <- function(wav, .f = findPeak, ...){

  ## Extract PitchTier
  pt_file <- extractPitchTier(wav, ..., output = "file")
  pt <- rPraat::pt.read(pt_file)

  ## Create flat PitchTier
  pt_flat <- pt
  pt_flat$t <- range(pt$t)
  pt_flat$f <- rep(.f(pt$f), 2)

  ## Write new PitchTier
  base_file <- gsub(".PitchTier$", "", basename(pt_file))
  wd <- dirname(pt_file)
  pt_flat_base <- paste0(base_file, "_flat.PitchTier")
  pt_flat_file <- file.path(wd, pt_flat_base)
  rPraat::pt.write(pt_flat, pt_flat_file)

  ## Build the command
  script <- system.file("extdata/praat", "reSynthPitch.praat", package = "retimer")
  args <- paste("--run", script, base_file, pt_flat_base, wd)

  ## Run script
  praatSys(args = args)

  ## Read wav file
  wav_out <- tuneR::readWave(file.path(wd, paste0(base_file, "_resynth.wav")))

  return(wav_out)
}
