#' extractWord
#'
#' Extract from a wav file with reference to a TextGrid. 
#'
#' @param x path to a TextGrid
#' @param word word to search for
#' @param tier name of word tier in TextGrid
#' @param ignore_case default is `TRUE`
#' @param instance instance of word in TextGrid to extract. Default extracts a random instance. Can also be numeric (row number)
#' @param wd working directory for Praat to use. Accepts relative paths.
#'
#' @return Extracts section of wav file corresponding to word and saves in format name_wordi.wav where name is the original name, word is the word and x is the numeric instance.
#'
#' @seealso density

extractWord <- function(x, word, tier = "Word", ignore_case = TRUE, instance = "random", wd = getwd()){
  wd <- normalizePath(wd)

  name <- stringr::str_extract(x, "[^/]*(?=.TextGrid)")

  word_dat <- findWord(x, word = word, tier = tier, ignore_case = ignore_case) |>
    dplyr::mutate(row = dplyr::row_number())

  if(instance == "random"){
    word_dat <- dplyr::sample_n(word_dat, 1)
  }else if(is.numeric(instance)){
    word_dat <- dplyr::slice(word_dat, as.integer(instance))
  }else{
    warning("instance must be 'random' or an integer")
  }

  args <- sprintf("%s %s%i %0.3f %0.3f", name, word_dat$label, word_dat$row, word_dat$t1, word_dat$t2)

  praatScript(args = args, script = "extractWord.praat", wd = wd)
}
