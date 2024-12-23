#' write_tg
#'
#' Writes a nested tibble to a 'Praat' TextGrid file
#'
#' @param x Nested tibble. Must contain the columns `name`, `type` and `data`. `data` must have the columns `t1`, `t2` and `label`
#' @param file File name to save TextGrid as
#'
#' @return Returns path of saved TextGrid file
#' @export

write_tg <- function(x, file){

    data <- t1 <- t2 <- type <- name <- NULL

    ## Get time limits for attributes
    tmin <- x |>
        dplyr::select(data) |>
        tidyr::unnest(data) |>
        dplyr::summarise(tmin = min(t1)) |>
        dplyr::pull(tmin)

    tmax <- x |>
        dplyr::select(data) |>
        tidyr::unnest(data) |>
        dplyr::summarise(tmax = max(t2)) |>
        dplyr::pull(tmax)

    ## Convert name column to factor to preserve order
    x$name <- factor(x$name, levels = unique(x$name))

    ## Extract name and type
    tg_a <- split(x, x$name) |>
        purrr::map(\(x) dplyr::select(x, name, type) |> as.list())

    ## Extract data
    tg_b <- split(x, x$name) |>
        purrr::map(\(x) dplyr::select(x, data) |> tidyr::unnest(data) |> as.list())

    ## Concatenate lists
    tg <- purrr::map2(tg_a, tg_b, c)

    if(!grepl(".TextGrid", file)) file <- paste0(file, ".TextGrid")
    
    ## Set attributes
    attr(tg, "class") <- c("list", tmin = tmin, tmax = tmax, type = "TextGrid", name = file)

    ## Write TextGrid
    rPraat::tg.write(tg, file)

    return(file)
}
