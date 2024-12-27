#' S4 generic for length
#'
#' S4 generic for length.
#'
#' @param x a 'tuneR' WAVE object
#'
#' @returns The length of the left channel of the WAVE object
#'
#' @seealso length

setMethod("length", signature(x = "Wave"), 
          function(x){
              validObject(x)
              length(x@left)
          })
