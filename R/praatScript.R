#' praatScript
#'
#' Executes a Praat script using the R system function.
#'
#' @param args arguements to pass to Praat script ("--run" not required)
#' @param script name of script if using a script from this package, or path to script for other scripts
#' @param wd working directory for Praat to use
#' @param praat path to Praat. If null will search for Praat in C:/Program Files (for Windows) or attempt to use "praat" for Unix based systems.
#'
#' @return Runs script in Praat and prints stdout to console.
#' @export
#'

praatScript <- function(args,
                        script = "reTimeWin.praat",
                        wd = getwd(),
                        praat = NULL){

  # Check if user is calling a retimer praat script
  if(file.exists(system.file("extdata/praat", script, package = "retimer"))){
    script <- system.file("extdata/praat", script, package = "retimer")
  }

  script <- paste0("\"", normalizePath(script), "\"")
  wd <- paste0("\"", normalizePath(wd), "\"")

  command <- paste("--run", script, args, wd)
  cat(paste("Running: praat", command, "\n"))

  praatSys(args = command, praat = praat)
}
