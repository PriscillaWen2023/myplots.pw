#' Create a ggplot histogram.
#'
#' This function creates a ggplot-style histogram of a given vector.
#'
#' @param x A vector of numeric values to be plotted.
#' @param bins Number of bins for the histogram.
#'
#' @return This function returns a ggplot histogram object.
#'
#' @examples
#' ## Create a histogram of a numeric vector.
#' data <- rnorm(100)
#' ghist(data)
#'
#' @import
#'   ggplot2
#'
#' @export

ghist <- function(x, bins = 10) {
  ggplot() +
    geom_histogram(data = data.frame(x), aes(x = x), bins = bins, fill = "skyblue", color = "black", alpha = 0.7) +
    labs(title = "Histogram",
         x = "Value",
         y = "Frequency")
}
