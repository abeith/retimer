# Wrapper for Praat script to squeeze F0 contour by a specified factor
# Assumes a folder called "audio" in your wd with the inFile and a folder
# called outputs in you wd where you want to save
squeezeContour <- function(inFile = filename,
                           newName= "squeezed",
                           factor = 0.5,
                           wd = getwd(),
                           praatPath = getPraatPath()){
  require(tidyverse)

  script <- system.file("praat", "squeezeContour.praat", package = "retimer")

  if(.Platform$OS.type == "windows"){
    script <- script %>%
      str_replace_all("/", "\\\\")

    wd <- wd %>%
      str_replace_all("/", "\\\\")

    slash <- "\\"}else slash <- "/"

  script <- script %>%
    paste0("\"", ., "\"")

  args <- paste("--run", script, inFile, newName, factor, slash, wd)
  cat(paste("Running:", praatPath, args, "\n"))
  system(paste(praatPath, args))
}

praatSys <- function(args = filename,
                     script = "extractPitchTier.praat",
                     wd = getwd(),
                     praatPath = getPraatPath()){
  require(tidyverse)
  script <- system.file("extdata/praat", script, package = "retimer")

  if(.Platform$OS.type == "windows"){
    script <- script %>%
      str_replace_all("/", "\\\\")

    wd <- wd %>%
      str_replace_all("/", "\\\\")

    slash <- "\\"}else slash <- "/"

  script <- script %>%
    paste0("\"", ., "\"")

  command <- paste(praatPath, "--run", script, args, wd)
  cat(paste("Running:", command, "\n"))
  system(command)
}
