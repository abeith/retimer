#' praatRetime
#'
#' @param wav path to a wav file or a tuneR WAVE object
#' @param tg a 'Praat' TextGrid object with 2 tiers: First tier should be
#' intervals in the input audio file and second tier should be the same
#' intervals with the desired onsets (t1) and offsets (t2).
#'
#' @seealso [read_tg()] for reading an existing TextGrid and [write_tg()] for saving a tibble as a TextGrid.
#'
#' @return A wav file with the timing of the second tier of the TextGrid will be saved to the outfile location.
#' 
#' @examples
#' set.seed(42)
#' data(mm1)
#' dur <- length(mm1)/mm1@samp.rate
#'
#' x <- runif(10)
#' t2_out <- dur*cumsum(x)/sum(x)
#' t1_out <- c(0, t2_out[-length(t2_out)])
#' t2_in <- dur*seq_len(10)/10
#' t1_in <- c(0, t2_in[-length(t2_in)])
#'
#' tg <- dplyr::tibble(
#'                name = rep(c("old", "new"), each = 10),
#'                type = "interval",
#'                t1 = c(t1_in, t1_out),
#'                t2 = c(t2_in, t2_out),
#'                label = rep(letters[1:10], times = 2)
#'              ) |>
#'   tidyr::nest(data = c(t1, t2, label))
#' if (Sys.which("praat") != "") {
#'  wav_retimed <- praatRetime(mm1, tg)
#' } else {
#'  message("Skipping example because Praat is not installed.")
#' }
#' @export

praatRetime <- function(wav, tg) {

    if(methods::is(wav, "character")){
        wav <- tuneR::readWave(wav)
    }
    
    ## Generate temporary filenames
    wav_file <- tempfile(fileext = ".wav")
    tg_file <- gsub(".wav$", ".TextGrid", wav_file)
    wav_out <- gsub(".wav$", "_out.wav", wav_file)
    wd <- dirname(wav_file)

    ## Write files to temporary dir
    tuneR::writeWave(wav, wav_file)
    write_tg(tg, tg_file)

    ## Build command
    script <- system.file("extdata/praat", "retime.praat", package = "retimer")
    args <- paste("--run", script, basename(wav_file), basename(tg_file), basename(wav_out), wd)

    ## Run script
    praatSys(args = args)

    return(tuneR::readWave(wav_out))
    
}
