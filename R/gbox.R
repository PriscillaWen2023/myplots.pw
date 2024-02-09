#' Create a ggplot box plot.
#'
#' This function creates a ggplot-style box plot of a given numeric vector,
#' or side-by-side box plots if both a numeric and a categorical variable are provided.
#'
#' @param data A data frame containing the variables to be plotted.
#' @param x_var The variable for the x-axis.
#' @param y_var (Optional) The variable for the y-axis.
#' @param orientation The orientation of the box plots: "vertical" or "horizontal".
#'
#' @return This function returns a ggplot box plot object.
#'
#' @examples
#' ## Create a box plot of a numeric vector.
#' data <- data.frame(values = rnorm(100))
#' gbox(data, "values", orientation = "vertical")
#'
#' ## Create side-by-side box plots of a numeric and a categorical variable.
#' data <- data.frame(values = rnorm(100), category = sample(c("A", "B"), 100, replace = TRUE))
#' gbox(data, "values", "category", orientation = "vertical")
#'
#' @import
#'   ggplot2
#'
#' @export

gbox <- function(data, x_var, y_var = NULL, orientation = "vertical") {
  if (is.null(y_var)) {
    # Single numeric vector provided, create a box plot
    if (orientation == "vertical") {
      ggplot(data, aes(x = 1, y = !!rlang::sym(x_var))) +
        geom_boxplot(fill = "skyblue") +
        labs(title = "Box Plot",
             x = "",
             y = x_var)
    } else if (orientation == "horizontal") {
      ggplot(data, aes(x = !!rlang::sym(x_var), y = 1)) +
        geom_boxplot(fill = "skyblue") +
        labs(title = "Box Plot",
             x = x_var,
             y = "")
    } else {
      stop("Invalid orientation. Please choose 'vertical' or 'horizontal'.")
    }
  } else {
    # Numeric and categorical variables provided, create side-by-side box plots
    if (orientation == "horizontal") {
      ggplot(data, aes_string(x = x_var, y = y_var)) +
        geom_boxplot(fill = "skyblue") +
        labs(title = "Side-by-Side Box Plots",
             x = x_var,
             y = y_var)
    } else if (orientation == "vertical") {
      ggplot(data, aes_string(x = y_var, y = x_var)) +
        geom_boxplot(fill = "skyblue") +
        labs(title = "Side-by-Side Box Plots",
             x = y_var,
             y = x_var)
    } else {
      stop("Invalid orientation. Please choose 'vertical' or 'horizontal'.")
    }
  }
}
