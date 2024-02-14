#' Create a ggplot box plot or side-by-side box plots.
#'
#' This function creates a ggplot box plot when a single numeric vector is provided, 
#' or side-by-side box plots when one numeric and one categorical variable are given.
#'
#' @param x A numeric vector for the x-axis if only one variable is provided, 
#' or the numeric vector for the y-axis if two variables are provided.
#' @param y A numeric vector for the y-axis. Only used if two variables are provided.
#' @param orientation The orientation of the box plots. 
#' It can be either "vertical" (default) or "horizontal".
#'
#' @return A ggplot object representing the box plot or side-by-side box plots.
#'
#' @examples
#' ## Example 1: Single numeric vector (vertical box plot)
#' set.seed(123)
#' x <- rnorm(100)
#' gbox(x)
#'
#' ## Example 2: Single numeric vector (horizontal box plot)
#' gbox(x, orientation = "horizontal")
#'
#' ## Example 3: Numeric and categorical variables (vertical box plot)
#' set.seed(123)
#' y <- factor(sample(c("A", "B", "C"), 100, replace = TRUE))
#' gbox(x, y)
#'
#' ## Example 4: Numeric and categorical variables (horizontal box plot)
#' gbox(x, y, orientation = "horizontal")
#'
#' @import ggplot2
#' @export

gbox <- function(x, y = NULL, orientation = "vertical") {
  if (is.null(y)) {
    # Single numeric vector provided, create a box plot
    if (orientation == "vertical") {
      ggplot(data.frame(y = x), aes(x = 1, y = y)) +
        geom_boxplot(fill = "skyblue") +
        labs(title = "Box Plot",
             x = "",
             y = deparse(substitute(x)))
    } else if (orientation == "horizontal") {
      ggplot(data.frame(x = x), aes(x = x, y = 1)) +
        geom_boxplot(fill = "skyblue") +
        labs(title = "Box Plot",
             x = deparse(substitute(x)),
             y = "")
    } else {
      stop("Invalid orientation. Please choose 'vertical' or 'horizontal'.")
    }
  } else {
    # Numeric and categorical variables provided, create side-by-side box plots
    if (orientation == "vertical") {
      ggplot(data.frame(x = x, y = y), aes(x = y, y = x)) +
        geom_boxplot(fill = "skyblue") +
        labs(title = "Side-by-Side Box Plots",
             x = deparse(substitute(y)),
             y = deparse(substitute(x)))
    } else if (orientation == "horizontal") {
      ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
        geom_boxplot(fill = "skyblue") +
        labs(title = "Side-by-Side Box Plots",
             x = deparse(substitute(x)),
             y = deparse(substitute(y)))
    } else {
      stop("Invalid orientation. Please choose 'vertical' or 'horizontal'.")
    }
  }
}

