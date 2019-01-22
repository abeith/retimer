# Extract peak density value for a duration vector
findPeak <- function(x){
  durDensity <- density(x)
  peakDensity <- durDensity$x[which.max(durDensity$y)]
  peakDensity
}

# Get peak frequency of syllable interval
syllFreq <- function(grid){
  require(dplyr)
  require(tidyr)
  require(purrr)
  intDur <- loadGrid(grid, addExt = F) %>%
    pluck("Syllable") %>%
    filter(label != "_") %>%
    mutate(interval = t1 - lag(t1)) %>%
    drop_na(interval) %>%
    pull(interval) %>%
    findPeak
  return(1/intDur)
}
