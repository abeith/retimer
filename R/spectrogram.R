#' spectrogram
#'
#' Universal spectrogram function.
#'
#' @param x a signal, `tuneR` WAVE object, or the path to an .wav or .mp3 file.
#' @param fs sample rate if supplying the signal as a vector
#' @param method spectrogram implementation to use
#' @param output format of output
#'
#' @return Returns a spectrogram in the desired format
#'
#' @export
#'

spectrogram <- function(x, fs = NULL, method, output = 'tibble', wintime = 25, steptime = 10) {
    if(is.character(x)){
        wav <- read_wav_or_mp3(x)
        sig <- wav@left
        fs <- wav@samp.rate
    } else if(is.vector(x)) {
        sig <- x
        if (is.null(fs)) stop("Sampling frequency (fs) cannot be NULL if x is a vector")
    } else if(class(x) == "Wave"){
        sig <- x@left
        fs <- x@samp.rate
    } else {
        stop("Unexpected argument for x")
    }

    if (is.null(method)) {
        stop("Method cannot be NULL. Available methods are seewave and phonTools.")
    } else if (method == 'seewave') {
        spec <- seewave_spectro(sig, fs, wintime = wintime, steptime = steptime)
    } else if (method == 'phonTools') {
        spec <- phontools_spectro(sig, fs, wintime = wintime, steptime = steptime)
    } else if (method == 'tuneR') {
        spec <- tuner_spectro(sig, fs, wintime = wintime, steptime = steptime)
    }

    return(spec)
}

seewave_spectro <- function(sig, fs, wintime, steptime) {

    wl <- round(fs*wintime/1e3)

    step <- fs*steptime/1e3

    ovlp <- 100 * (1 - (step/wl))
    
    spec <- seewave::spectro(sig, fs, plot = F, wl = wl, ovlp = ovlp)
    spec$amp <- t(spec$amp)
    spec$freq <- spec$freq * 1e3
    return(spec)
}

phontools_spectro <- function(sig, fs, wintime, steptime) {
    phontools_spec <- phonTools::spectrogram(sig, fs, show = F, windowlength = wintime, timestep = steptime)
    spec_attr <- with(phontools_spec,
                      list(
                          method = "phontools",
                          fs = fs,
                          windowlength = windowlength,
                          timestep = timestep,
                          dynamicrange = dynamicrange,
                          colors = colors,
                          maxfreq = maxfreq
                      ))

    spec <- list()
    attributes(spec) <- spec_attr
    spec$time <- as.numeric(attr(phontools_spec$spectrogram, "dimnames")[[1]])/1e3
    spec$freq <- as.numeric(attr(phontools_spec$spectrogram, "dimnames")[[2]])
    spec$amp <- phontools_spec$spectrogram

    attributes(spec$amp)$dimnames <- NULL
  

    return(spec)
}

tuner_spectro <- function(sig, fs, wintime, steptime) {
    
    tuner_spec <- tuneR::powspec(sig, fs, wintime/1e3, steptime/1e3)

    ## seewave default for converting to dB
    tuner_spec <- 20 * log10(tuner_spec)
    
    k <- fs / (wintime*1e3) / 2
    
    spec <- list(
        time = steptime * seq_len(ncol(tuner_spec)) / 1e3,
        freq = (fs/2)*seq_len(length.out = nrow(tuner_spec))/nrow(tuner_spec),
        amp = t(tuner_spec),
        call = spec_call
    )
    return(spec)
}

read_wav_or_mp3 <- function(x) {
    if(grepl("\\.wav$", tolower(x))) {
        wav <- tuneR::readWave(x)
    } else if(grepl("\\.mp3$", tolower(x))) {
        wav <- tuneR::readMP3(x)
    } else {
        stop("x must be the path to a wav/mp3 file, a WAVE object or a vector")
    }
    return(wav)
}
