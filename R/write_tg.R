#' write_tg
#'
#' Writes a nested tibble to a Praat TextGrid file
#'
#' @param x Nested tibble. Must contain the columns `name`, `type` and `data`. `data` must have the columns `t1`, `t2` and `label`
#' @param name File name to save TextGrid as
#' @param path Path to save folder. Default is working directory.
#'
#' @export

write_tg <- function(x, name, path = "."){

  data <- t1 <- t2 <- NULL

  # Get time limits for attributes
  tmin <- x %>%
    dplyr::select(data) %>%
    tidyr::unnest() %>%
    dplyr::summarise(tmin = min(t1)) %>%
    dplyr::pull(tmin)

  tmax <- x %>%
    dplyr::select(data) %>%
    tidyr::unnest() %>%
    dplyr::summarise(tmax = max(t2)) %>%
    dplyr::pull(tmax)

  # Convert name column to factor to preserve order
  x$name <- factor(x$name, levels = unique(x$name))

  # Extract name and type
  tg_a <- split(x, x$name) %>%
    purrr::map(~.x %>%
                 select(name, type) %>%
                 as.list())

  # Extract data
  tg_b <- split(x, x$name) %>%
    purrr::map(~.x %>%
                 dplyr::select(data) %>%
                 tidyr::unnest() %>%
                 as.list())

  # Concatenate lists
  tg <- purrr::map2(tg_a, tg_b, c)

  # Set attributes
  attr(tg, "class") <- c("list", tmin = tmin, tmax = tmax, type = "TextGrid", name = paste0(name, ".TextGrid"))

  # Write TextGrid

  if(!grepl(".TextGrid", name)) name <- paste0(name, ".TextGrid")

  rPraat::tg.write(tg, paste0(path, "/", name))
}
