#' tempogram
#'
#' Produce a tempogram of a time series. No longer exported due to the rray requirement
#'
#' @param x signal as vector
#' @param sr sample rate
#' @param window length of acf window
#' @param hop distance between windows
#'
#' @return a list including:
#'    acf: tempogram in long format tibble
#'    tempo: peak tempos for each time bin (f0 of tempogram)
#'    acf.m: tempogram in 2D matrix
#'

tempogram <- function(x, sr = 250, window = 200, hop = 100){

  # Get number of bins for tempogram
  n <- floor((length(x) - window)/hop)

  # Make 2D array
  a <- rray::rray(1:window, c(window,1))
  b <- rray::rray((0:n) * hop, c(1, n + 1))
  c <- a + b

  times <- as.vector(c[window,])

  c <- rray::rray(x[c], c(window, n + 1))

  get_acf <- function(x){
    x <- as.vector(x)
    stats::acf(x, lag.max = length(x), plot = FALSE)[["acf"]]
  }

  acf_result <- apply(c, 2, get_acf)
  lags <- 1:dim(acf_result)[1]
  lags <- lags - 1

  #Selects highest peak: Alternative would be first peak
  find_tempo <- function(x){
    peaks <- pracma::findpeaks(x, minpeakheight = 0)
    tempo <- peaks[1,2]
    return(tempo)
  }

  tempo <- apply(acf_result, 2, function(x) find_tempo(x))
  tempo[which(sapply(tempo, is.null))] <- NaN
  tempo <- unlist(tempo)
  tempo <- cbind(t = as.numeric(times/sr), tempo = tempo/sr)

  colnames(acf_result) <- times

  acf.tbl <- cbind(lags, acf_result) %>%
    tibble::as_tibble() %>%
    tidyr::gather(t, acf, -lags) %>%
    dplyr::mutate(t = as.numeric(t)/sr,
                  lags = lags/sr,
                  f = 1/lags)

  return(list(acf = acf.tbl, tempo = tibble::as_tibble(tempo), acf.m = acf_result))
}
