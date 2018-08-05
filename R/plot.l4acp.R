
#' plot the threshold grid curve produced by l4acp
#'
#' Plots the lasso objective function's value obtained from running regression by putting each value of threshold parameter's range in a model. It can also make plot not from lasso objective function but from ordinary MSE. Since \code{l4acp} can use multiple lambda values, it is required to specify a lambda value. Note that the plot has x-axis corresponding to threshold grid, and y-axis corresponding to the lasso objective function.
#'
#' @param x fitted "l4acp" object
#' @param s the specified lambda value for threshold grid curve. Note that when specific lambda values are putted, it does not come out with the result from that lambda values exactly. Rather it finds the nearest lambda value from the lambda sequence in the fitted "l4acp" object. By default it is the smallest one from the lambda sequence.
#' @param type If the type is "lasso", it makes the plot by the value of a lasso objective function. Else if the type is "mse", it makes the plot by the value of a MSE.
#' @param ... other arguments that can be passedd to l4acp
#'
#' @details  A plot is produced, and nothing is returned
#'
#'
#' @examples
#' # using 'lr' as an threshold variable
#' data("grdata")
#' lrdata <- grdata[(!is.na(grdata[,"lr"])),]
#' x <- lrdata[,c(4:dim(grdata)[2])]
#' q <- lrdata[,"lr"]
#' y <- lrdata[,"gr"]
#' fit <- l4acp(x=x, y=y, q=q, s=0.00094)
#' plot(fit, type="lasso")
#' @export

plot.l4acp <- function(x, s = min(x$lambda), type = "lasso", ...) {

  num <- which.min(abs(x$lambda - s))
  dat <- x$grid.loop[[1]]

  if (type == "lasso") {
    y <- x$grid.loop[[2]][, num]
    graphics::par(new = FALSE)
    graphics::plot(dat, y, type = "o", pch = 19, xlab = "threshold grid", ylab = "Lasso objective funct")
  }

  if (type == "mse") {
    y <- x$grid.loop[[3]][, num]
    graphics::par(new = FALSE)
    graphics::plot(dat, y, type = "o", pch = 19, xlab = "threshold grid", ylab = "MSE")
  }

  if (min(abs(x$lambda - s)) >= 1e-07) {
    cat("NOTE: input for lambda should be the one from the model's lambda sequence")
    cat("\n")
    cat("the result is from when lambda =", x$lambda[num])
  }

}



coef.l4acp <- function(x, ...) {
  print(x$coefficients)
}

