#' findWord
#'
#' Find a word in a TextGrid
#'
#' @param x path to a TextGrid
#' @param word word to search for
#' @param tier name of word tier in TextGrid
#' @param ignore_case default is `TRUE`
#'
#' @return Returns a tibble with onset (t1) and offset (t2) of each occurance of the word in the TextGrid
#' @export
#'
#' @seealso extractWord

# Function for finding a word in TextGrids
findWord <- function(x, word = "speech", tier = "Word", ignore_case = TRUE){

  name <- data <- label <- NULL

  query <- stringr::regex(paste0("^", word, "$"), ignore_case = ignore_case)

  read_tg(x) |>
    dplyr::filter(name == tier) |>
    tidyr::unnest(cols = c(data)) |>
    dplyr::filter(stringr::str_detect(label, query))
}
