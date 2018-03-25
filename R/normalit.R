#' Normalizes a vector of values from 0 to 1
#'
#' @param m A vector
#' @param ... Unused extra arguments
#' @return A normalized vector with values rescaled from 0 to 1
#' @examples
#
#' normalit(m)
#' @export


normalit <- function(m)
{
    (m - min(m))/(max(m) - min(m))
}
