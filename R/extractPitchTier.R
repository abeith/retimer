#' extractPitchTier
#'
#' Extracts "Praat" PitchTier from wav object.
#'
#' @param wav path to a wav file or a tuneR WAVE object
#' @param res resolution of PitchTier
#' @param fmin minimum frequency of PitchTier
#' @param fmax maximum frequency of PitchTier
#' @param output can be "PitchTier" or "file"
#'
#' @return Returns a PitchTier object or the temporary path to the generated PitchTier file
#' @export
#'

extractPitchTier <- function(wav, res = 0.1, fmin = 50, fmax = 250, output = "PitchTier") {

    wav_file <- tempfile(fileext = ".wav")

    if(methods::is(wav, "character")){
        wav <- tuneR::readWave(wav)
    }
    
    tuneR::writeWave(wav, wav_file)

    ## Build the command
    script <- system.file("extdata/praat", "extractPitchTier.praat", package = "retimer")
    wd <- dirname(wav_file)
    base_file <- gsub(".wav$", "", basename(wav_file))
    args <- paste("--run", script, base_file, res, fmin, fmax, wd)

    ## Run script
    praatSys(args = args)

    ## Read PitchTier
    pt_file <- file.path(wd, paste0(base_file, ".PitchTier"))
    if(output == "PitchTier") {
        pt <- rPraat::pt.read(pt_file)
        return(pt)
    } else if(output == "file") {
        return(pt_file)
    }
}
