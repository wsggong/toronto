#' plot the threshold grid curve produced by l4acp
#'
#' Plots the lasso objective function's value obtained from running regression by putting each value of threshold parameter's range in a model. It can also make plot not from lasso objective function but from ordinary MSE. Since \code{l4acp} can use multiple lambda values, it is required to specify a lambda value. Note that the plot has x-axis corresponding to threshold grid, and y-axis corresponding to the lasso objective function.
#'
#' @param x fitted "l4acp" object
#' @param v inex for a lambda value. Remember that you can only make a plot with one lambda value. By default it is 1, which is the largest among the lambda sequence used for \code{l4acp}. It is consistent to the column names of a matrix that comes out when you call the fitted model in console.
#' @param type If the type is "lasso", it makes the plot by the value of a lasso objective function. Else if the type is "mse", it makes the plot by the value of a MSE.
#' @param ... other arguments that can be applied to l4acp
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

plot.l4acp <- function(x, v=1, type = "lasso", ...) {

  dat <- x$grid.loop[[1]]

  if (type == "lasso") {
    y <- x$grid.loop[[2]][, v]
    graphics::par(new = FALSE)
    graphics::plot(dat, y, type = "o", pch = 19, xlab = "threshold grid", ylab = "Lasso objective funct")
    graphics::title(paste("lambda=",x$lambda[v],sep=""))
  }

  if (type == "mse") {
    y <- x$grid.loop[[3]][, v]
    graphics::par(new = FALSE)
    graphics::plot(dat, y, type = "o", pch = 19, xlab = "threshold grid", ylab = "MSE")
    graphics::title(paste("lambda=",x$lambda[v],sep=""))
  }
}

coef.l4acp <- function(x, ...) {
  print(x$coefficients)
}

