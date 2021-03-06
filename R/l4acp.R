#'@export
l4acpEst <- function(x, y, q, s, trim = 0.1) {

  x <- as.matrix(x)
  if(!is.matrix(x)) stop("'x' should be a matrix")
  y <- as.vector(y)
  if(!is.vector(y)) stop("'y' should be a vector")
  q <- as.vector(q)
  if(!is.vector(q)) stop("'q' should be a vector")
  s <- as.vector(s)
  if(!is.vector(s)) stop("'s' should be a vector")
  if(length(trim)!=1) stop("'trim' should not be a multiple value")
  if(trim<0|!is.numeric(trim)) stop("'trim' should be a number between 0 and 0.5")

  if (is.null(colnames(x)) == 1) {
    reg.names <- c("const", (paste("x", c(1:ncol(x)), sep = "")))
  }
  if (is.null(colnames(x)) == 0) {
    reg.names <- c("const", colnames(x))
  }

  nobs <- length(y)
  nx <- ncol(x)
  nreg <- dim(x)[2] * 2 + 2
  if(trim>=0.5){
    stop("trim cannot be above an half")
  }
  sorted.tau <- sort(q)
  trim.factor <- round(length(sorted.tau) * trim, 0)
  grid.tau <- sorted.tau[(trim.factor + 1):(nobs - trim.factor)]
  ngrid <- length(grid.tau)

  nlmbd <- length(s)
  s <- sort(s, decreasing = TRUE)


  ## step 1: variable preparation ##

  norm.x <- matrix(rep(NA), nrow = 1, ncol = nreg - 1)  # ell_2 norm of x variables

  delta.hat.grid <- array(rep(NA), dim = c(ngrid, nreg, nlmbd))  # whole(from whole thres.grid) coefficients
  delta.hat <- array(rep(NA), dim = c(1, nreg, nlmbd))  # selected(from tau.hat) coefficients
  tau.hat <- rep(NA, nlmbd)  # estimated tau.hat

  obj.v <- matrix(rep(NA), ngrid, nlmbd)  # lasso objective function value
  mse <- matrix(rep(NA), ngrid, nlmbd)


  ## step 2 : start of threshold lasso regression ##

  for (i in 1:ngrid) {
    ind <- (q < grid.tau[i])  # the obs that are over threshold

    x.reg <- cbind(x, 1 * ind, x * ind)  # preparing regressor (x, x(tau))
    x.reg.bar <- apply(x.reg,2,mean)
    x.reg.bar <- matrix(rep(x.reg.bar, nobs), nrow=nobs, ncol=nreg-1, byrow=T)

    norm.x <- sqrt(apply((x.reg-x.reg.bar)^2/nobs, 2, sum))  # (ell_2 norm / sqrt(nobs) ) of centered & scaled data
    norm.x <- matrix(rep(norm.x), nlmbd, nreg - 1, byrow = T)  # make it a matrix(from vector, nlmbd repetition)

    new_m <- glmnet::glmnet(x.reg, y)
    delta.hat.grid[i, , ] <- as.matrix(glmnet::coef.glmnet(new_m, s = s/2 )[,])  # coef(but for intercept) estimation

    ###############################################################################
    #                     Note on obj.v of lasso
    #============================================================================
    # When it comes to lasso regression(which is default for gaussian family),
    # glmnet minimizes [ 1/(2n)*RSS + lambda*|beta|_1 ]
    # But before glmnet runs regression, it standardize and normalize variables
    # scaling on y should be negligible
    # for X, each column is standardized such that [ 1/n*\sum_{j}{X_ij} = 1, i=1,2,...]
    # i.e. divide each column by [ sqrt(1/n*\sum_{j}{X_ij}) = (ell_2 norm of x), i=1,2,...]
    # Therefore what glmnet really minimize is the following:
    # [ 1/(2n)*RSS + lambda *(ell_2 norm of x) * |beta|_1 ]
    # by substituting "lambda" by "lambda/2", we get
    # [ (1/n*RSS + lambda *(ell_2 norm of x) * |beta|_1) * (1/2) ]
    ###############################################################################

    yhat <- cbind(1, x.reg) %*% delta.hat.grid[i, , ]  #nlmbd coef at one time
    uhat <- matrix(rep(y), nobs, nlmbd) - yhat

    ssr <- diag(t(uhat) %*% uhat)/nobs
    p <- diag(norm.x %*% abs(delta.hat.grid[i, -1, ]))
    obj.v[i, ] <- ssr + s * p
    mse[i, ] <- ssr
  }


  ## step 3 : fundamental regression loop ended ##
  #  * select tau hat & return the result  ##

  for (j in 1:nlmbd) {
    opt <- which.min(obj.v[, j])
    delta.hat[, , j] <- delta.hat.grid[opt, , j]
    tau.hat[j] <- grid.tau[opt]
  }

  M.alpha <- rep(NA, nlmbd)
  R2 <- rep(NA, nlmbd)
  adj.R2 <- rep(NA, nlmbd)

  for (j in 1:nlmbd) {
    M.alpha[j] <- sum((delta.hat[, , j] != 0))
    tss <- sum((y - mean(y))^2)
    rss <- sum((y - cbind(1, x, 1 * (q < tau.hat[j]), x * (q < tau.hat[j])) %*% delta.hat[, , j])^2)
    R2[j] <- 1 - rss/tss
    adj.R2[j] <- 1 - (1 - R2[j]) * (nobs - 1)/(nobs - M.alpha[j])
  }

  rval <- cbind(matrix(s, nlmbd, 1), matrix(tau.hat, nlmbd, 1), t(delta.hat[1, , ]), matrix(M.alpha), matrix(R2), matrix(adj.R2))
  dimnames(rval) <- list(c(paste("V", 1:nlmbd, sep = "")), c("lambda", "tauhat", t(reg.names), t(paste("t.", reg.names, sep = "")), "M.alpha", "R2", "adj.R2"))
  coefficients <- as.matrix(delta.hat[1, , ])
  dimnames(coefficients) <- list(c( t(reg.names), t(paste("t.", reg.names, sep = ""))), c(paste("V", 1:nlmbd, sep = "")))
  grid.loop.info <- list(grid.tau, obj.v, mse)


  return(list(lambda = s, tau.hat = tau.hat, coefficients = Matrix::Matrix(coefficients, sparse=TRUE), adj.R2 = adj.R2, M.alpha = M.alpha, grid.loop = grid.loop.info,
              matrix = t(rval)))
}

#' l4acp
#'
#' LASSO regression of a model with a change point due to a covariate threshold. It obtains regression coefficients for covariates, and a threshold parameter.
#'
#' @usage l4acp(x, y, q, s, trim=0.1)
#'
#' @param x Covariates.
#' @param y A dependent variable.
#' @param q A threshold covariate.
#' @param s The values of lambda(tuning parameter) used for lasso regression. To know what the objective function that is minimized for lasso regression, see the "Details" below. This is not given by default. To get an idea which value to use, check \code{cvl4acp}.
#' @param trim The percentile for trimming the data of a threshold covariate (from above and below) to obtain the range for threshold parameter. By default, it is 0.1. And this results in finding a threshold parameter from 10-90 percentile range of a threshold variable.
#'
#' @return
#' \item{lambda}{ The values of lambda used for regression. Each lambda has corresponding \code{tau.hat}, \code{coefficients}, \code{M.alpha}, \code{R2}, and \code{adj.R2}. Note that it is automatically ordered from high to low values before it runs, and on its result.}
#' \item{tau.hat}{ The estimated threshold covariates.}
#' \item{coefficients}{ The regression coefficients for covariates. If the covariates has n different variables, it leads to (2n+2) coefficients.
#' The first (n+1) coefficients can be interpreted as the coefficients when a threshold covariate is below the threshold parameter. It includes an intercept at the head. That's why the number of coefficients is (n+1), not n.
#' And the next (n+1) variables are change of regression coefficients when a threshold covariate exceeds the threshold parameter, also including change of an intercept.}
#' \item{adj.R2}{ Adjusted R2 square.}
#' \item{M.alpha}{ The number of nonzero coefficients from regression, whose maximum is (2n+2).}
#' \item{grid.loop}{ Used for the threshold grid curve.}
#' \item{matrix}{ A matrix that includes \code{lambda}, \code{tau.hat}, \code{coefficients}, \code{M.alpha}, \code{R2}, and \code{adj.R2}. Each column represents a specific lambda value.}
#'
#' @details
#' The regression model by this function is \deqn{Y = \alpha + X*\beta + X*\delta*I{q<\tau} + u}.
#' The estimated \code{coefficients} by \code{l4acp} contain the estimators for alpha(intercept), beta(coefficients before a change point), and delta(coefficients change after a change point). \code{tau.hat} is the estimator for tau in the model above.
#' The objective function that is minimized is \deqn{(1/n)*RSS + lambda * penalty},
#' while the penalty is \deqn{\sum||X(j)||_2 * |\beta_{j}|_1}.
#' This is the exact objective function written in Sokbae Lee, Myung Hwan Seo, and Youngki Shin, (2016), which is a little different from that of \code{LARS} or \code{glmnet} packages.
#' Remember that beta does not contain the estimated intercept.
#'
#' @examples
#' # using 'gdp60' as an threshold variable
#' data("grdata")
#' x <- grdata[,c(5:dim(grdata)[2])]
#' q <- grdata[,"gdp60"]
#' y <- grdata[,"gr"]
#' fit <- l4acp(x,y,q, s=0.0007)
#' plot(fit)
#'
#'@references Sokbae Lee, Myung Hwan Seo, and Youngki Shin, (2016) \emph{The Lasso for High-Dimensional Regression with a Possible Change Point}, Journal of the Royal Statistical Society Series B, Vol 78(1), 193-210
#'
#'@seealso \code{plot.l4acp} and \code{cvl4acp}
#'@aliases l4acpEst
#'@export
l4acp <- function(x, y, q, s, trim = 0.1) UseMethod("l4acp")

#'@export
l4acp.default <- function(x, y, q, s, ...) {

  est <- l4acpEst(x, y, q, s, ...)
  est$call <- match.call()
  class(est) <- "l4acp"
  est
}

#'@export
print.l4acp <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\nBrief table:\n")
  table <- Matrix::Matrix(round(x$matrix,7), sparse=TRUE)
  print(table)
}
