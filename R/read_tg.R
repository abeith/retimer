#' read_tg
#'
#' Reads a Praat TextGrid as a nested tibble
#'
#' @param file path to TextGrid file
#' @param encoding Passed to rPraat::tg.read: 'auto' (default) will detect encoding, or can be set to 'UTF-8' (rPraat default)
#'
#' @return Returns a nested tibble with `name`, `type` and `data`. `data` has the variables `t1`, `t2` and `label`
#'
#' @export

read_tg <- function(file, encoding = 'auto'){

  name <- type <- NULL

  if(!grepl(".TextGrid$", file)) file <- paste0(file, ".TextGrid")

  # Read TextGrid
  tg <- rPraat::tg.read(file, encoding = 'auto')

  # Tidy
  tidy_grid <- tg %>%
    purrr::map(tibble::as_tibble) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(name, type) %>%
    tidyr::nest() %>%
    dplyr::ungroup()

  return(tidy_grid)
}
