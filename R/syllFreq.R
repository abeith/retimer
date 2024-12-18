# Get peak frequency of syllable interval
syllFreq <- function(x, silence = "_", freq = TRUE){

  name <- data <- label <- t1 <- interval <- NULL

  int_dur <- read_tg(x) |>
    dplyr::filter(name == "Syllable") |>
    tidyr::unnest(cols = c(data)) |>
    dplyr::filter(label != silence) |>
    dplyr::mutate(interval = t1 - dplyr::lag(t1)) |>
    dplyr::summarise(interval = findPeak(interval, na.rm = TRUE)) |>
    dplyr::pull(interval)

  if(freq){
    return (1/int_dur)
  }else{
    return (int_dur)
  }
}
