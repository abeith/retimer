# *nix compatible wrapper for Praat retiming
reTimeNix <- function(name, newName= "retimed", script = "retimeSpeech.praat", praatPath = "praat", args = "--run"){

  scriptPath <- system.file("praat", script, package = "retimer") %>%
    paste0("\"", getwd(), scriptPath, "\"")

  system(paste(praatPath, args, scriptPath, name, newName))
}

# Check Praat path: Should work on any apt-get/homebrew *nix install or Windows
# if Praat is installed to C:/Program Files...
getPraatPath <- function(...){
  if(.Platform$OS.type == "windows"){
    return(getPraatPathWin(...))}

  # No need in Unix based system
  if(.Platform$OS.type == "unix"){
    return("praat")
  }
}

# Get Praat path for Windows
getPraatPathWin <- function(appDir = "C:/Program Files") {
  list.files(appDir, "praat", full.names = T) %>%
    list.files("Praat.exe", full.names = TRUE) %>%
    str_replace_all("/", "\\\\") %>%
    paste0("\"", ., "\"")
}

#
reTimeWin <- function(name = filename, newName= "retimed", praatPath = getPraatPath(), wd = getwd()){
  script <- system.file("praat", "reTimeWin.praat", package = "retimer") %>%
    str_replace_all("/", "\\\\") %>%
    paste0("\"", ., "\"")

  wd <- wd %>%
    str_replace_all("/", "\\\\") %>%
    paste0("\\")

  args <- paste("--run", script, name, newName, wd)
  cat(paste("Running:", praatPath, args, "\n"))
  system(paste(praatPath, args))
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

writeGrid <- function(grid = newGrid, path = "./newGrids/%s_tmp.TextGrid", name = filename){

  newGrid <- grid

  #Create empty TextGrid with 2 tiers (order matters)
  newTextGrid <- tg.createNewTextGrid(0, endPoint) %>%
    tg.insertNewIntervalTier(1, "syllOld", 0, endPoint) %>%
    tg.insertNewIntervalTier(2, "syllNew", 0, endPoint)

  #Fill the TextGrids
  newTextGrid$syllOld$t1 <- newGrid$t1
  newTextGrid$syllOld$t2 <- newGrid$t2
  newTextGrid$syllOld$label <- newGrid$label
  newTextGrid$syllNew$t1 <- newGrid$newT1
  newTextGrid$syllNew$t2 <- newGrid$newT2
  newTextGrid$syllNew$label <- newGrid$label

  #Write the grid to a file
  tg.write(newTextGrid, sprintf(path, filename))
}
