
#' plot the cross-validation curve produced by cvl4acp
#'
#' Plots the cross-validation curve, as a function of the lambda values used.
#'
#' @param x fitted "cvl4acp" object
#' @param logscale If it is TRUE, the x-axis is shown by log scale. By default it is FALSE
#' @param ... Other argumnets that can be passed to cvl4acp
#'
#' @details A plot is produced, and nothing is returned. Red line indicates the selected lambda with minimal tested error. Blue line is for lambda with one standard deviation above, which is \code{lambda.1se} of the fitted model.
#'
#' @examples
#' x <- matrix(rnorm(1000), nrow = 50, ncol = 20)
#' q <- runif(50)
#' y <- x[,c(1,3)] %*% c(1,1) + x[,c(2,3)] %*% c(-1,1) * (q < 0.5) + 0.5 * rnorm(50)
#' cvfit <- cvl4acp(x,y,q, nfolds=5)
#' plot(cvfit, logscale=TRUE)
#'
#' @export

plot.cvl4acp <- function(x, logscale=FALSE, ...) {

  if(logscale==FALSE){
    graphics::par(new = FALSE)
    graphics::matplot(x$lambda.seq, x$cvm, type = "o", pch = 19, xlab = "lambda", ylab = "tested-MSE", ylim = c(0, max(x$cvm)))
    graphics::abline(v = x$lambda.min, col = "red")
    graphics::abline(v = x$lambda.1se, col = "blue")
  }
  if(logscale==TRUE){
    graphics::par(new = FALSE)
    graphics::matplot(log(x$lambda.seq), x$cvm, type = "o", pch = 19, xlab = "lambda", ylab = "tested-MSE", ylim = c(0, max(x$cvm)))
    graphics::abline(v = log(x$lambda.min), col = "red")
    graphics::abline(v = log(x$lambda.1se), col = "blue")
  }
}
