# Wrapper for Praat script to squeeze F0 contour by a specified factor
# Assumes a folder called "audio" in your wd with the inFile and a folder
# called outputs in you wd where you want to save
squeezeContour <- function(x,
                           newName= "squeezed",
                           factor = 0.5,
                           wd = getwd()){

  wd <- normalizePath(wd)

  command <- paste(x, newName, factor)
  praatScript(args = command, script = "squeezeContour.praat", wd = wd)

}
