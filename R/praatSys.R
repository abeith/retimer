#' praatSys
#'
#' Call Praat via system2()
#'
#' @param args arguements to pass to Praat
#' @param praat path to Praat. If null will search for Praat in C:/Program Files (for Windows) or attempt to use "praat" for Unix based systems.
#' @param ... arguements to pass to internal get_praat_path() function. This can be used to change the folder to look for R in for Windows (default is appDir = "C:/Program Files")
#'
#' @return Prints stdout to console
#' @export
#'
#' @seealso system2
#'


praatSys <- function(args = "--version", praat = NULL, ...){

  # Make sure most recent version of Praat is being used for Windows
  if(.Platform$OS.type == "windows"){
    if(is.null(praat)){
      praat <- Sys.getenv("PRAAT_PATH")
    }

    if(praat == ""){
      Sys.setenv(PRAAT_PATH = get_praat_path(...))
      praat <- Sys.getenv("PRAAT_PATH")
    }
  }else{
    praat <- "praat"
  }

  stdout <- tempfile(pattern = "stdout", fileext = ".txt")
  stderr <- tempfile(pattern = "stderr", fileext = ".txt")

  system2(praat, args, stdout = stdout, stderr = stderr)

  stdout_chr <- readLines(stdout, skipNul = T)
  stderr_chr <- readLines(stderr, skipNul = T)

  stdout_chr <- stdout_chr[stdout_chr != ""]
  stderr_chr <- stderr_chr[stderr_chr != ""]

  message(paste(stdout_chr, stderr_chr, collapse = "\n"))
}

get_praat_path <- function(appDir = "C:/Program Files"){

  v1 <- v2 <- v3 <- NULL

  # find all available praat versions
  find_praat_windows <- function(path){

    # get full path to executable
    exe <- list.files(path, ".exe", full.names = T)

    # make tmp file for stdout (see https://github.com/praat/praat/issues/1033)
    stdout <- tempfile(pattern = "stdout", fileext = ".txt")

    # get version
    system2(exe, "--version", stdout = stdout, stderr = NULL)

    praat_version <- readLines(stdout, skipNul = T) %>%
      paste(collapse = " ") %>%
      trimws()

    # send version info to console
    message(paste(exe, "--version\n", praat_version))

    # make df with numerical version info
    tibble::tibble(path = exe,
           version = stringr::str_extract(praat_version, "(?<=Praat )[\\d\\.]+")) %>%
      tidyr::separate(version, c("v1", "v2", "v3"), convert = TRUE)
  }

  # arrange praat versions in tibble
  praat_versions <- list.files(appDir, "praat", full.names = T, ignore.case = T) %>%
    purrr::map(find_praat_windows) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(v1, v2, v3)

  # get most recent
  praat_versions$path[length(praat_versions$path)]
}
