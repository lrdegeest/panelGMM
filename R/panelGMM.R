#' Generalized Method of Moments (GMM) for panel data
#'
#' Fit one-step and two-step GMM models for panel data with lagged instrumental variables. Provides panel-robust standard errors allowing for heteroskedasticity and correlation over time.
#'
#' @author Lawrence R De Geest, \email{lrdegeest@@gmail.com}
#' @param formula specification of the regression relationship
#' @param instruments set of variables to instrument for the independent variables
#' @param time the name of the time series variable in the provided data
#' @param nlags the highest number of lags in the instruments
#' @param twostep estimate a two-step GMM model. Defaults to FALSE.
#' @param data data of class data.frame
#' @return model object of generic class "list" containing the following elements:
#' \item{\code{coefficients}}{the estimated coefficients}
#' \item{\code{fitted.values}}{predicted values of the dependent variable}
#' \item{\code{residuals}}{the residuals}
#' \item{\code{standard.errors}}{the panel-robust standard errors for the estimated coefficients}
#' \item{\code{RMSE}}{the root mean squared error}
#' \item{\code{pvalues}}{the p-values for the estimated coefficients}
#' \item{\code{y}}{a vector of the model's dependent variable}
#' \item{\code{x}}{the model's independent variables and matrices, each in their own matrix}
#' \item{\code{results}}{a \code{data.frame} of the results: coefficients, standard errors and p-values}
#' \item{\code{call}}{the model call}
#' \item{\code{formula}}{the model formula}
#' \item{\code{terms}}{a list with elements "regressors" and "instruments" containing the terms objects for the respective components}
#' \item{\code{levels}}{levels of the categorical regressors}
#' \item{\code{summary}}{a list object summarizing the model: the model call, the results (\code{results}), the RMSE, and if \code{twostep = TRUE}, the OIR test statistic and p-value.}
#' @references Cameron, A. Colin, and Pravin K. Trivedi. Microeconometrics: methods and applications. Cambridge university press, 2005.
#' @export
#' @examples
#' # Load the "hours and wages" data provided by the panelGMM package:
#' data("hours_wages", package = "panelGMM")
#' # Generate differenced and lagged variables:
#' hours_wages_gmm <- renameVars(hours_wages, "lnhr", c("lnwg", "kids", "age", "agesq", "disab"))
#' hours_wages_gmm <- makeDIFFS(hours_wages_gmm, id, 1)
#' hours_wages_gmm <- makeLAGS(hours_wages_gmm, id, 1, y = FALSE)
#' hours_wages_gmm <- makeLAGS(hours_wages_gmm, id, 2, y = FALSE)
#' # Estimate
#' model <- d1_y_lnhr ~ 0 + d1_x_lnwg + d1_x_kids + d1_x_age + d1_x_agesq + d1_x_disab | 0 + l1_x_kids + l1_x_age + l1_x_agesq + l1_x_disab + l2_x_kids + l2_x_age + l2_x_agesq + l2_x_disab + l2_x_lnwg
#' estimation <- panelGMM(model, time = year, nlags = 2, twostep = T, data = hours_wages_gmm)
#' # View results:
#' model$summary


panelGMM <- function (formula, instruments, time, nlags, twostep = FALSE, data) {
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "time", "data"), names(mf), 0)
  mf <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  if (!missing(instruments)) {
    formula <- Formula::as.Formula(formula, instruments)
    cl$instruments <- NULL
    cl$formula <- formula(formula)
  }
  else {
    formula <- Formula::as.Formula(formula)
  }
  mf$formula <- formula
  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  y <- model.response(mf, "numeric")
  mt <- terms(formula, data = data)
  mtX <- terms(formula, data = data, rhs = 1)
  X <- model.matrix(mtX, mf)
  mtZ <- delete.response(terms(formula, data = data, rhs = 2))
  Z <- model.matrix(mtZ, mf)
  t <- length(unique(mf$`(time)`)) + nlags
  W <- solve(crossprod(Z,Z))
  beta <- crossprod(solve(crossprod(X,Z) %*% W %*% crossprod(Z,X)),(crossprod(X,Z) %*% W %*% crossprod(Z,y)))
  cs <- seq(1, (nrow(data)/t), 1)
  cst = rep(cs, each = (nrow(na.omit(data))/length(cs)))
  r <- ncol(Z)
  K <- ncol(X)
  prediction <- X %*% beta
  e <- y - prediction
  getS <- function(instruments, residuals) {
    ZuuZ <- lapply(cs, function(i) t(Z[cst == i, ]) %*% e[cst == i] %*% t(e[cst == i]) %*% Z[cst == i, ])
    S <- matrix(apply(matrix(unlist(ZuuZ), ncol = r * r, byrow = T),
                      MAR = 2, FUN = sum), ncol = r, byrow = T)
    return(S)
  }
  S <- getS(Z,e)
  XZWZX <- crossprod(X,Z) %*% W %*% crossprod(Z,X)
  XZWSWZX <- crossprod(X,Z) %*% W %*% S %*% t(W) %*% crossprod(Z,X)
  var <- solve(XZWZX) %*% XZWSWZX %*% solve(XZWZX)
  se <- sqrt(diag(var))
  if(twostep){
    beta <- crossprod(solve(crossprod(X,Z) %*% solve(S) %*% crossprod(Z,X)),(crossprod(X,Z) %*% solve(S) %*% crossprod(Z,y)))
    prediction <- X %*% beta
    e <- y - prediction
    S2 <- getS(Z,e)
    var <- solve(crossprod(X,Z) %*% (solve(S2)) %*% crossprod(Z,X))
    se <- sqrt(diag(var))
    Zu <- lapply(cs, function(i) t(Z[cst == i, ]) %*% e[cst == i])
    uZ <- lapply(cs, function(i) t(e[cst == i]) %*% Z[cst == i, ])
    Zu_mat <- matrix(apply(matrix(unlist(Zu), ncol = 1 * r, byrow = T), MAR = 2, FUN = sum), ncol = r, byrow = T)
    uZ_mat <- matrix(apply(matrix(unlist(Zu), ncol = r * 1, byrow = T), MAR = 2, FUN = sum), ncol = 1, byrow = T)
    OIR <- Zu_mat %*% solve(S2) %*% uZ_mat
    OIR.pvalue <- pchisq(OIR, df = r - K, lower.tail = FALSE)
  }
  output <- list()
  output$coefficients <- beta
  output$fitted.values <- prediction
  output$residuals <- as.matrix(e)
  output$standard.errors <- as.matrix(se)
  output$RMSE <- round(sqrt(mean(output$residuals^2)), 3)
  output$pvalues <- pnorm(-abs(output$coefficients) / output$standard.errors)*2
  output$y <- y
  output$x <- list(regressors = X, instruments = Z, projected = output$x)
  output$results <- with(output, data.frame("Estimated coefficient" = coefficients, "Standard error" = standard.errors, "p-value" = pvalues))
  output$results <- data.frame(apply(output$results, MARGIN = 2, function(x) round(x,4)))
  output$call <- cl
  output$formula <- formula(formula)
  output$terms <- list(regressors = mtX, instruments = mtZ, full = mt)
  output$levels <- .getXlevels(mt, mf)
  if(twostep){
    output$OIR <- paste0(round(OIR,3), " ", paste0("(p = ", round(OIR.pvalue, 3), ")"))
    output$summary <- with(output, list("Call" = call, "Model" = formula, "Results" = results, "RMSE" = RMSE, OIR = OIR))
  } else {
    output$summary <- with(output, list("Call" = call, "Model" = formula, "Results" = results, "RMSE" = RMSE))
  }
  return(output)
}
