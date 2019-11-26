# Some functions for working with Praat TextGrids as tidy data
# Not exported

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
