#' runRetime
#'
#' @param audio relative path to input audio file
#' @param grid relative path to input TextGrid
#' @param outfile relative path to output audio file
#' @param wd path to working directory
#'
#' @return A wav file with the timing of the second tier of the TextGrid will be saved to the outfile location.
#' @export

runRetime <- function(audio, grid, outfile, wd = getwd()){

  # Check if input files exist
  if(!file.exists(paste0(wd, audio))){
    stop(paste0(wd, audio, " does not exist"))
  }
  if(!file.exists(paste0(wd, grid))){
    stop(paste0(wd, grid, " does not exist"))
  }

  # Get path to retime script
  script_path <- system.file("extdata/praat", "retime.praat", package = "retimer")

  # Get command
  command <- paste("praat --run",
                   paste0("\"", script_path, "\""),
                   audio, grid, outfile,
                   paste0("\"", wd, "\""))

  # Print command in console
  cat(paste("Running:", command))

  # Run
  if(.Platform$OS.type == "windows"){
    tmpfile <- tempfile(fileext = ".txt")
    shell(paste(command, ">", tmpfile))
    cat(readLines(tmpfile, skipNul = TRUE))
  }else{
    system(command)
  }
}
