#'@export

cvl4acpEst <- function(x, y, q, s = NULL, trim = 0.1, nfolds = 10) {

  x <- as.matrix(x)
  if(!is.matrix(x)) stop("'x' should be a matrix")
  y <- as.vector(y)
  if(!is.vector(y)) stop("'y' should be a vector")
  q <- as.vector(q)
  if(!is.vector(q)) stop("'q' should be a vector")
  if(!is.null(s)){
    s <- as.vector(s)
    if(!is.vector(s)&!is.numeric(s)) stop("'s' should be a vector")
  }
  if(length(trim)!=1) stop("'trim' should not be a multiple value")
  if(trim<0|!is.numeric(trim)) stop("'trim' should be a number between 0 and 0.5")
  nfolds <- as.integer(nfolds)
  if(!is.integer(nfolds)) stop("'nfolds' should be an integer")
  if(nfolds<2|nfolds>length(y)) stop("'nfolds' should be above 1 and below the #obs")

  flds <- caret::createFolds(y, k = nfolds, list = TRUE, returnTrain = FALSE)

  if (is.null(colnames(x)) == 1) {
    reg.names <- c("const", (paste("x", c(1:ncol(x)), sep = "")))
  }
  if (is.null(colnames(x)) == 0) {
    reg.names <- c("const", colnames(x))
  }

  nobs <- length(y)  # nobs  = number of observations
  nx <- ncol(x)  # nx    = number of variables
  nreg <- dim(x)[2] * 2 + 2  # nreg  = number of regressors used (2*nx+2, including const.)
  if(trim>=0.5){
    stop("trim cannot be above an half")
  }
  sorted.tau <- sort(q)
  trim.factor <- round(length(sorted.tau) * trim, 0)
  grid.tau <- sorted.tau[(trim.factor + 1):(nobs - trim.factor)]
  ngrid <- length(grid.tau)

  if (is.null(s)) {
    cat("\n")
    cat("obtaining lambda_seq")
    q_l <- min(grid.tau)
    q_m <- stats::median(grid.tau)
    q_u <- max(grid.tau)

    q_seq <- c(q_l, q_m, q_u)
    s <- c()

    for (i in 1:3) {
      ind <- (q < q_seq[i])
      x.reg <- cbind(x, 1 * ind, x * ind)  # preparing regressor (x, x(tau))
      s <- append(s, glmnet::glmnet(x.reg, y)$lambda / 2 )
      s <- sort(s, decreasing = TRUE)
      s <- s[!duplicated(s)]
    }
    nlmbd <- length(s)
  } else {
    s <- sort(s, decreasing = TRUE)
    nlmbd <- length(s)
  }
  mse <- matrix(NA, nfolds, nlmbd)

  for (k in 1:nfolds) {
    cat("\n")
    cat(paste("# of folds=", k))

    cvfit <- l4acpEst(x = x[-flds[[k]], ], y = y[-flds[[k]]], q = q[-flds[[k]]], s = s, trim = trim)
    cat("\n")
    cat("  well  done")
    cvcoef <- cvfit$coefficients
    cvtau <- cvfit$tau.hat

    if (length(flds[[k]]) != 1) {
      x_t <- x[flds[[k]], ]
      y_t <- y[flds[[k]]]
      q_t <- q[flds[[k]]]
    } else {
      x_t <- t(x[flds[[k]], ])
      y_t <- y[flds[[k]]]
      q_t <- q[flds[[k]]]
    }

    for (i in 1:nlmbd) {
      ind <- (q[flds[[k]]] < cvtau[i])
      x.reg <- cbind(x_t, 1 * ind, x_t * ind)
      uhat <- y_t - cbind(1, x.reg) %*% (cvcoef[, i])
      mse[k, i] <- sum((uhat)^2)/length(y_t)
    }
  }

  cvm <- apply(mse, 2, mean)
  cvn <- which.min(cvm)
  cvsd <- apply(mse, 2, stats::sd)
  shat1 <- s[cvn]
  shat2 <- max(s[(cvm < cvm[cvn] + cvsd[cvn])])

  return(list(cvm = cvm, mse = mse, cvsd = cvsd, lambda.seq = s, lambda.min = shat1, lambda.1se = shat2))
}

#' Cross-validation for l4acp
#'
#' Does k-folds cross-validation for l4acp, produces a plot, and returns a value for lambda
#'
#' @usage cvl4acp(x, y, q, s=NULL, trim=0.1, nfolds=10)
#'
#' @param x Covariates as in \code{l4acp}.
#' @param y A dependent variable as in \code{l4acp}.
#' @param q A threshold covariate as in \code{l4acp}.
#' @param s Input for the sequence of lambdas to be tested. By default, when it is not specificed, it generates 3 lambda sequences of length 100, following the method used in \code{glmnet} 3 times from the top, middle, and bottom values of threshold parameter's range.
#' But it is possible that the three lambda sequences are identical. Then after removing duplication, 100 values remain.
#' It is recommended to run without lambda sequence at first time with small nfolds. Important! Remember to check whether it is credible by using plot. See \code{plot.cvl4acp}. You can get an idea which lambda sequence to put based on the default one's range.
#' @param trim The percentile for trimming to obtain the range of a threshold parameter as in \code{l4acp}.
#' @param nfolds the number of folds for cross-validation. By default it is 10
#'
#' @return
#' \item{cvm}{ The iterated mean of tested mean squared errors for each fold. Each value corresponds to different lambdas. Note that it is written as tested-MSE in the graph from \code{plot.cvl4acp}.}
#' \item{mse}{ The tested MSE for each fold. Its row corresponds to different folds, and column corresponds to different lambdas.}
#' \item{cvsd}{ The standard error for tested MSE (computed from k number of errors). Each value corresponds to different lambdas.}
#' \item{lambda.seq}{ The lambda sequence it runned cross validation.}
#' \item{lamda.min}{ The lambda value that minimizes the cvm.}
#' \item{lambda.1se}{ The maximal lambda among the values that are smaller than \code{lambda.min}+1*\code{cvsd}(it depends on lambda)}
#'
#' @examples
#' x <- matrix(rnorm(1000), nrow = 50, ncol = 20)
#' q <- runif(50)
#' y <- x[,c(1,3)] %*% c(1,1) + x[,c(2,3)] %*% c(-1,1) * (q < 0.5) + 0.5 * rnorm(50)
#' cvfit <- cvl4acp(x,y,q, nfolds=5)
#' print(cvfit)
#'
#'@references Sokbae Lee, Myung Hwan Seo, and Youngki Shin, (2016) \emph{The Lasso for High-Dimensional Regression with a Possible Change Point}, Journal of the Royal Statistical Society Series B, Vol 78(1), 193-210
#'
#'@seealso \code{plot.cvl4acp} and \code{l4acp}
#'@aliases cvl4acpEst
#'
#' @export

cvl4acp <- function(x, y, q, s = NULL, trim = 0.1, nfolds = 10) UseMethod("cvl4acp")

#' @export

cvl4acp.default <- function(x, y, q, s = NULL, ...) {

  x<-as.matrix(x)
  y<-as.numeric(y)
  q<-as.numeric(q)

  est <- cvl4acpEst(x, y, q, s, ...)
  est$call <- match.call()
  class(est) <- "cvl4acp"
  est
}

#' @export

print.cvl4acp <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\n selected lambda:\n")
  print(round(x$lambda.min, 5))
}
