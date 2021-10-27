#' @name MKLR
#'
#' @title Multi-class Kernel Logistic Regression
#'
#' @description This function fit a Multi-class Kernel Logistic Regression model to the data (\code{y}, \code{x}) using some pre-specified kernel. The return list contains the estimated kernel weights as well as the original data to perform predictions.There are
#' two types of kernel, they are 'RBF' and 'polynomial'
#'
#' @param y A \code{n x 1} column vector containing multiclass responses.
#' @param x A \code{n x p} matrix containing the covariates.
#' @param kernel The kernel to use. Either \code{RBF} (default) or \code{polynomial}.
#' @param lambda The regularization parameter.
#' @param sigma2 The scale in the \code{RBF} and \code{polynomial} kernel. See details.
#' @param d The degree in the \code{polynomial} kernel.
#' @param threshold The convergence threshold.
#' @param max_iter The maximum number of iterations.
#'
#' @details The \code{RBF} kernel has the following form:
#' \deqn{exp(-||x-y||^2/sigma2).}
#' The \code{polynomial} kernel has the following form:
#' \deqn{(1+x'y/sigma2)^d.}
#'
#' @return A list containing:
#' \describe{
#' \item{\code{x}}{The original \code{x}.}
#' \item{\code{alpha}}{The vector of fitted weights.}
#' \item{\code{kernel}}{The kernel.}
#' \item{\code{sigma2}}{The scale parameter.}
#' \item{\code{d}}{The polynomial degree.}
#' }
#' @export
#' @seealso \link{predict.MKLR}
#'

MKLR <- function(
  y,
  x,
  kernel=c("RBF", "polynomial")[1],
  lambda=0.01,
  sigma2=1.0,
  d=3,
  lr=1e-4,
  threshold=1.0e-4,
  max_iter=500
){
  # inputs check
  if(is.vector(y)) y = matrix(y, length(y), 1)
  if(lambda<1.0e-16) stop("lambda should be positive")
  if(sigma2<1.0e-16) stop("sigma2 should be positive")
  if(threshold<1.0e-16) stop("threshold should be positive")
  if(d < 1) stop("d should be larger than or equal to 1")

  # preparation
  n = nrow(x)
  p = ncol(x)
  if(is.null(colnames(x))) colnames(x) = paste("X",seq(p),sep="")
  vnames = colnames(x)

  # compute kernel
  KERNELS = c("RBF", "polynomial")
  if(kernel == "RBF"){
    D = as.matrix(stats::dist(x))
    K = exp( - D ^ 2 / sigma2 )
  }else if(kernel == "polynomial"){
    xs = scale(x, scale=F)
    D = xs %*% t(xs)
    K = ( 1 + D / sigma2 ) ^ d
  }else{
    stop(paste("only kernels", KERNELS, "are implemented"))
  }
  K = scale(t(scale(K, scale=F)), scale=F)


  # obj function
  objective = function(alpha){
    lin_pred = linear_predictors(alpha)
    penalty = 0.5 * lambda * sum(t(alpha) %*% K %*% alpha)
    y_linpred=0
    for (i in (1:length(y))){
      j<-y[i,]
      if(j< length(table(y))-1){
        y_linpred<-y_linpred+lin_pred[i,][j]
      }
    }
    loss = - y_linpred/length(y) + mean(apply(exp(lin_pred), 1, sum)+ matrix(1, length(y), 1))
    return(loss+ penalty)#((loss + penalty))#[1,1])
  }

  # linear predictor
  linear_predictors = function(alpha){
    lin_pred = K %*% alpha
    return(lin_pred)
  }

  # fitted probability
  probabilities = function(alpha){
    lin_pred = linear_predictors(alpha)
    proba = exp(lin_pred)%o%(1/(apply(exp(lin_pred), 1, sum)+ matrix(1, length(y), 1)))
    return(proba)
  }

  # gradient wrt alpha
  y_m<-matrix(0,nrow=length(y),ncol=length(table(y))-1)
  for (i in (1:length(y))) {
    j<-y[i,]
    if(j<10){
      y_m[i,][j]<-1
    }
  }
  gradients = function(alpha){
    proba = probabilities(alpha)
    g_alpha = - t(K) %*% ((y_m - proba[,,1,1]) / n - lambda * alpha)
    return(g_alpha)
  }

  # fit using gradient descent
  alpha = matrix(0, n, length(table(y))-1)
  obj_val_prev = objective(alpha)
  for(i in seq(max_iter)){
    print(paste('iteration ',i))
    # compute gradient
    grad = gradients(alpha)
    # update
    alpha = alpha - lr * grad
    # check convergence
    obj_val = objective(alpha)
    if(abs(obj_val - obj_val_prev)< threshold){
      break
    }
    obj_val_prev = obj_val
  }
  # return
  out = list(x=x, alpha=alpha, kernel=kernel, sigma2=sigma2, d=d, lambda=lambda, n_iter=i)
  class(out) = "MKLR"
  return(out)
}




#' @name predict.MKLR
#'
#' @title Predict values by MKLR model
#'
#' @description Prediction
#'
#' @param object MKLR model.
#' @param newx new data without labels
#' @param response probability or class.
#'
#' @return The predicted probabilities or classes.
#' @method predict MKLR
#' @export predict.MKLR
#' @export
#' @seealso \link{MKLR}
#' 
predict.MKLR <- function(object, newx,response=c('probability','class')[1]){
  # construct kernel
  m = nrow(newx)
  n = nrow(object$x)
  p = ncol(object$x)
  if(object$kernel == "RBF"){
      D = matrix(pdist::pdist(object$x, newx)@dist, n, m, T)
      K = exp( - D ^ 2 / object$sigma2 )
    }
  else if(object$kernel == "polynomial"){
    xs = scale(object$x, scale=F)
    newx = (newx - matrix(attr(xs, 'scaled:center'), m, p, T))
    D = xs %*% t(newx)
    K = ( 1 + D / object$sigma2) ^ object$d
  }

  # compute predictors
  f = t(K) %*% object$alpha
  p0 <- exp(f)%o%(1/(apply(exp(f), 1, sum)+ as.vector(matrix(1, length(newx[,1]), 1))))
  p0_<-p0[, , 1]
  p_<-apply(p0_, 1,function(x) 1-sum(x))
  p<-cbind(p0_,p_)
  if(response=='probability'){
    p <- p
    return(p)
  }else{
    p<-apply(p, 1,function(x) which.max(x))
    return(p)

  }

}

