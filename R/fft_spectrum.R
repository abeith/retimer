#' fft_spectrum
#'
#' Calculates low frequency power spectrum of vocalic interval of speech signal. Following method of Tilsen & Johnson (2008)
#'
#' @param signal a speech signal
#' @param f sampling frequency
#' @param padding length to zero pad signal to. If signal is longer than padding, this will be increased.
#'
#' @return Returns a matrix with columns `freq` (frequency in Hz) and `pwr` (spectral power).
#'
#' @references Tilsen, S., & Johnson, K. (2008). Low-frequency Fourier analysis of speech rhythm. The Journal of the Acoustical Society of America, 124(2), EL34–EL39. doi:10.1121/1.2947626
#' @seealso fft_spectro
#' @export

fft_spectrum <- function(signal, f, padding = 2048){

  signal_80 <- voc_env(signal, f)

  result <- fft_spec_pwr(signal_80, 80, padding)

  return(result)
}

voc_env <- function(signal, f){
  # Butterworth filter to extract vocalic signal
  Wn <- c(700, 1300) / (f / 2)
  bw_filt <- signal::butter(1, Wn, 'pass')
  voc_signal <- signal::filter(bw_filt, signal)

  # Butterworth filter to lowpass at 10Hz
  bw_low <- signal::butter(4, 10 / (f / 2), 'low')
  lowpass_signal = signal::filtfilt(bw_low, abs(voc_signal))

  #adj_samples = round(f * 0.045) + 1
  #lowpass_mm1 = lowpass_mm1[adj_samples:length(lowpass_mm1)]

  signal_mag <- abs(voc_signal)
  #plot(signal_mag/max(signal_mag))
  #lines(lowpass_signal/max(lowpass_signal), col = "red")

  signal_80 <- signal::resample(lowpass_signal, 80, f)
  #plot(signal_80, type = 'l')

  # mean centre
  signal_80 = signal_80 - mean(signal_80);
  #mean(signal_80)

  # scale
  signal_80 <- signal_80 / max(signal_80)
  #mean(signal_80)
  #plot(signal_80, type = 'l')

  return(signal_80)
}

fft_spec_pwr <- function(signal, f, padding){

  # Increase size of padding if signal is longer than manual setting
  if(length(signal) > padding){
    binary_size <- intToBits(length(signal) - 1) %>%
      as.integer()

    padding <- 2**max(which(binary_size == 1))
  }

  # zero pad the signal
  zero_len <- (padding - length(signal))/2
  padded_signal <- c(rep(0, floor(zero_len)), signal, rep(0, ceiling(zero_len)))

  # calculate fft
  fft_signal <- stats::fft(padded_signal)
  padded_len <- length(padded_signal)

  # get power from spectrum
  pwr <- abs(fft_signal/padded_len)
  pwr <- pwr[1:((padded_len/2)+1)]
  pwr[2:(length(pwr) - 1)] <- 2 * pwr[2:(length(pwr) - 1)]
  freq = f * (0:(padded_len/2)) / padded_len

  result <- cbind(freq, pwr)

  return(result)
}
