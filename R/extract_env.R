#' extract_env
#'
#' Extract amplitude envelope of filtered speech signal. Adapted from Tilson & Johnson (2008). Procedure:
#'
#' 1. Signal is bypass filtered to extract desired frequency range
#' 2. Absolute signal is then lowpass filtered
#' 3. Signal is downsampled and mean centred if desired
#'
#' @param x a speech signal
#' @param fs sampling frequency of signal
#' @param low_pass frequency of lowpass filter used for smoothing
#' @param fs_out output sampling frequency
#' @param win lower and upper frequencies for initial bypass filter. Default is 700Hz-1300Hz as in Tilson & Johnson (2008)
#' @param mean_centre if TRUE signal will be scaled between 0 and 1 and then mean centred. Default is FALSE
#'
#' @return A matrix with time and amplitude
#'
#' @references Tilsen, S., & Johnson, K. (2008). Low-frequency Fourier analysis of speech rhythm. The Journal of the Acoustical Society of America, 124(2), EL34–EL39. doi:10.1121/1.2947626
#' @seealso fft_spectro
#' @export

extract_env <- function(x, fs, low_pass = 80, fs_out = 80, win = c(700, 1300), mean_centre = FALSE){
  ## Bypass
  wn <- win / (fs / 2)
  bw_pass <- signal::butter(1, wn, 'pass')
  voc_signal <- signal::filter(bw_pass, x)

  ## Lowpass
  bw_low <- signal::butter(4, low_pass / fs, 'low')
  lowpass_signal <- signal::filtfilt(bw_low, abs(voc_signal))

  ## Downsample
  signal_out <- signal::resample(lowpass_signal, fs_out, fs)

  ## mean centre
  if(mean_centre){
    signal_out <- signal_out - min(signal_out)
    signal_out <- signal_out / max(signal_out)
    signal_out <- signal_out - mean(signal_out)
  }

  ## Add time and convert to matrix
  t <- seq_along(signal_out)/fs_out
  res <- cbind(time = t, amp = signal_out)
  
  return(res)
}
