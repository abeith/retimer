# *nix compatible wrapper for Praat retiming
reTimeNix <- function(name, newName= "retimed", script = "retimeSpeech.praat", praatPath = "praat", args = "--run"){

  scriptPath <- system.file("praat", script, package = "retimer") %>%
    paste0("\"", getwd(), scriptPath, "\"")

  system(paste(praatPath, args, scriptPath, name, newName))
}

# Get Praat path for Windows
getPraatPath <- function(appDir = "C:/Program Files") {
  list.files(appDir, "praat", full.names = T) %>%
    list.files("Praat.exe", full.names = TRUE) %>%
    str_replace_all("/", "\\\\") %>%
    paste0("\"", ., "\"")
}

#
reTimeWin <- function(name, newName= "retimed", praatPath = NULL, args = "--run"){
  if(is.null(praatPath)) praatPath <- getPraatPath()
  script <- system.file("praat", "reTimeWin.praat", package = "retimer") %>%
    str_replace_all("/", "\\\\") %>%
    paste0("\"", ., "\"")

  system(paste(praatPath, args, script, name, newName))
}

# Extract TextGrid as list of tibbles
loadGrid <- function(file, drop = NULL, addExt = TRUE){
  require(rPraat)
  require(dplyr)
  require(purrr)

  if(addExt){file <- paste0(file, ".TextGrid")}

  # Load existing text grid
  oldGrid <- tg.read(file)

  # Transform TextGrid into list of tibbles
  dat <- oldGrid %>%
    map(as_tibble) %>%
    map(select, -name, -type)

  if(!is.null(drop)){dat <- discard(dat, names(dat) %in% drop)}

  return(dat)
}

# Extract Text Grid as one tibble
loadGridLong <- function(file, drop = NULL, group = NULL){
  require(dplyr)
  require(tidyr)
  require(purrr)

  # Load grid list
  grid <- loadGrid(file, drop)

  # Get breaks for intonational phrase grouping
  if(!is.null(group)){
    #Get intonational group onsets
    cuts <- grid %>%
      pluck(group) %>%
      gather(label, times, t1:t2) %>%
      distinct(times) %>%
      pull(times)
  }

  # Rename labels, calculate duration, tidy
  dat <- grid %>%
    map2(.x = ., .y = names(.), function(x, y){rename(x, !!y := label)}) %>%
    map(~mutate(., duration = t2 - t1)) %>%
    reduce(full_join, by = c("t1", "t2", "duration")) %>%
    select(t1:t2, duration, everything()) %>%
    gather(level, label, -t1, -t2, -duration) %>%
    drop_na(label)

  if(!is.null(group)){dat <- mutate(dat, group = cut(t1, cuts, right = FALSE))}

  return(dat)
}
