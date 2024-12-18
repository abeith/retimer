#' fft_spectro
#'
#' Calculates low frequency power spectrogram of vocalic interval of speech signal. Following method of Tilsen & Johnson (2008)
#'
#' @param x a `tuneR` "Wave" object or the path to a .wav file.
#' @param f_out the sample frequency for the output
#' @param window_size number of samples to calculate each spectrum over
#' @param padding length to zero pad signal to. If signal is longer than padding, this will be increased.
#' @param plot if true a spectrogram will be plotted
#'
#' @return Returns a tibble with frequency (Hz), time (s) and power
#'
#' @references Tilsen, S., & Johnson, K. (2008). Low-frequency Fourier analysis of speech rhythm. The Journal of the Acoustical Society of America, 124(2), EL34–EL39. doi:10.1121/1.2947626
#' @seealso fft_spectrum
#' @export

fft_spectro <- function(x, f_out = 80, window_size = 256, padding = 2048, plot = TRUE){

  freq <- pwr <- NULL


  if(is.character(x)){
    if(file.exists(x)){
      # Load wav
      wav <- tuneR::readWave(x)
    }else{
      stop(sprintf("%s is not a valid file path", x))
    }
  }else if(methods::is(x, "Wave")){
    wav <- x
  }else{
    stop("x must be a filepath to a wav or a tuneR wav object")
  }

  # Get vocalic envelope
  env <- voc_env(wav@left, wav@samp.rate, f_out)

  # define cutoffs
  low <- 1:(length(env) - window_size)
  high <- (window_size + 1):length(env)
  spec <- purrr::map2(low, high, \(x, y) fft_spec_pwr(env[x:y], f_out, padding) |> tibble::as_tibble())

  nested_spec <- tibble::tibble(t = (low + high)/(f_out * 2), #denominator is 2 (to average) * 80 (sample rate)
                               spec = spec)

  df_out <- nested_spec |>
    tidyr::unnest() |>
    dplyr::filter(freq <= 8) |>
    dplyr::select(freq, t, pwr)

  if(plot){
    p <- df_out |>
      ggplot2::ggplot(ggplot2::aes(t, freq, fill = pwr)) +
      ggplot2::geom_raster()

    print(p)
  }

  return(df_out)
}



