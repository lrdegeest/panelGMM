#' Generalized Method of Moments (GMM) for panel data
#'
#' Fit one-step and two-step GMM models for panel data with lagged instrumental variables. Calculates panel-robust standard errors allowing for heteroskedasticity and correlation over time.
#' Includes \code{summary.panelGMM} and \code{print.summary.panelGMM} methods.
#'
#' @author Lawrence R. De Geest, \email{lrdegeest@@gmail.com}
#' @param formula the model in standard R form with instruments (e.g., \code{y ~ x1 + x2 | z1 + z2 ...}).
#' @param panel the name of the panel variable in the data.
#' @param time the name of the time variable in the data.
#' @param twostep estimate a two-step GMM model. Defaults to TRUE.
#' @param intercept optional intercept. Defaults to FALSE.
#' @param data data of class \code{data.frame}/\code{tibble}/\code{data.table}.
#' @return a \code{panelGMM} object.
#' @note Currently works only for balanced panels.
#' @import Rcpp
#' @export
#' @examples
#' # Using the hours and wages data provided by the panelGMM package:
#' data("hours_wages", package = "panelGMM")
#' # create lagged and differenced instruments
#' hours_wages_gmm = hours_wages %>%
#'    shiftCols(data = ., panel=id, type = "difference", shifts = 1, columns=c("lnhr", "lnwg", "kids", "age", "agesq", "disab")) %>%
#'    shiftCols(data = ., panel=id, type = "lag", shifts = 1:4, columns=c("lnhr", "lnwg", "kids", "age", "agesq", "disab"))
#' # define the model
#' model = lnhr_diff_1 ~ lnwg_diff_1 + kids_diff_1 + age_diff_1 + agesq_diff_1 + disab_diff_1 | kids_lag_1 + kids_lag_2 + age_lag_1 + age_lag_2 + agesq_lag_1 + agesq_lag_2 + disab_lag_1 + disab_lag_2 + lnwg_lag_2
#' # estimate the parameters with two-step GMM
#' model_results = panelGMM(model, panel = id, time = year, twostep = TRUE, data = hours_wages_gmm)
#' summary(model_results)


panelGMM <- function(formula, panel, time, twostep = TRUE, intercept = FALSE, data) {


  # BEGIN SET-UP ------------------------------------------------------------

  # model stuff
  call <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "time", "data"), names(mf), 0)
  mf <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  formula <- Formula::as.Formula(formula)
  mf$formula <- formula
  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  mt <- terms(formula, data = data)

  # response vector
  y <- model.response(mf, "numeric")

  # matrix of independent variables
  mtX <- terms(formula, data = data, rhs = 1)
  X <- model.matrix(mtX, mf)

  # matrix of instruments
  mtZ <- delete.response(terms(formula, data = data, rhs = 2))
  Z <- model.matrix(mtZ, mf)

  # drop the intercept unless user asks for it
  if(!intercept) {
    X <- X[,-1]
    Z <- Z[,-1]
  }

  # get the instrument names, use these in summary()
  instrument_names <- attr(mtZ,"term.labels")

  ## unique cross sections
  panel_name <- deparse(substitute(panel))
  panel_numeric <- as.numeric(data[[panel_name]])
  if(any(is.na(panel_numeric))) stop("panel column cannot contain non-numeric values", call. = FALSE)
  cs <- length(unique(data[[panel_name]]))

  ## unique time (net lags/differences)
  ### limitation here is that all NA's are assumed to be in lag/differenced variables (so I can use na.omit())
  ### to do: work around this limitation in case user has missing data
  time_name <- deparse(substitute(time))
  time_numeric <- as.numeric(data[[time_name]])
  if(any(is.na(panel_numeric))) stop("time column cannot contain non-numeric values", call. = FALSE)
  t <- length(unique(na.omit(data)[[time_name]]))

  ## number of instruments
  r <- ncol(Z)
  ## number of observations (just the rows of Z)
  N <- nrow(Z)
  ## number of independent variables
  K <- ncol(X)

  # END SET-UP --------------------------------------------------------------

  # BEGIN ESTIMATION --------------------------------------------------------

  ## BEGIN ONE-STEP GMM ##

  # calculate weighting matrix W = (Z'Z)^-1
  W <- solve(crossprod(Z))
  # estimate the coefficients
  ## [X'ZWZ'X]^1 X'ZWZ'y
  beta <- crossprod(solve(crossprod(X,Z) %*% (W %*% crossprod(Z,X))), (crossprod(X,Z) %*% (W %*% crossprod(Z,y))))
  # get the predicted values
  prediction <- do_mv(X,beta)
  # residuals
  e <- y - prediction
  # now the standard errors by cluster
  ## calculate ZuuZ for each panel and return a list
  ZuuZ_list <- do_ZuuZ(Z, e, cs, t)
  ## collapse the list of clusters into a weighting matrix
  S <- matrix(apply(matrix(unlist(ZuuZ_list), ncol = r * r, byrow = T), MARGIN = 2, FUN = sum), ncol = r, byrow = T)

  # variance-covariance matrix for one-step GMM
  if(!twostep){

    XZWZX <- crossprod(X,Z) %*% (W %*% crossprod(Z,X))

    XZWSWZX <- (crossprod(X,Z) %*% W) %*% (tcrossprod(S,t(W)) %*% crossprod(Z,X))

    vcov <- solve(XZWZX) %*% (XZWSWZX %*% solve(XZWZX))

  }
  ## END ONE-STEP GMM ##

  ## BEGIN TWO STEP GMM ##

  if(twostep){

    # recalculate the coefficients using the weighting matrix S
    beta <- crossprod(solve(crossprod(X,Z) %*% solve(S) %*% crossprod(Z,X)),(crossprod(X,Z) %*% solve(S) %*% crossprod(Z,y)))
    # recalculate predictions...
    prediction <- do_mv(X,beta)
    # .. and then errors
    e <- y - prediction

    # CLUSTERED STANDARD ERRORS

    ## re-calculate ZuuZ for each panel and return a list
    ZuuZ2 <- do_ZuuZ(Z, e, cs, t)
    ## collapse the list of clusters into a new weighting matrix
    S2 <- matrix(apply(matrix(unlist(ZuuZ2), ncol = r * r, byrow = T), MARGIN = 2, FUN = sum), ncol = r, byrow = T)
    ## use the new weighting matrix to re-calculate the variance-covariance matrix
    vcov <- solve(crossprod(X,Z) %*% (solve(S2) %*% crossprod(Z,X)))


    # OVERIDENTIFICATION TEST

    ## first calculate Zu' for each panel and return
    Zu_list <- do_Zu(Z, e, cs, t)
    ## now collapse the list
    Zu_mat <- matrix(apply(matrix(unlist(Zu_list), ncol = 1 * r, byrow = T), MARGIN = 2, FUN = sum), ncol = r, byrow = T)
    ## calculate the test statistic...
    OIR <- Zu_mat %*% tcrossprod(solve(S2),Zu_mat)
    ## ...and it's p-value
    OIR_pvalue <- pchisq(OIR, df = r - K, lower.tail = FALSE)
  }

  ## END TWO STEP GMM ##

  # END ESTIMATION ----------------------------------------------------------


  # GENERATE OUTPUT AND RETURN ----------------------------------------------

  ## instantiate the output as a list
  output <- list()
  ## estimated coefficients
  output$coefficients <- beta
  ## fitted values
  output$fitted.values <- prediction
  ## residuals
  output$residuals <- as.matrix(e)
  ## variance-covariance matrix
  output$vcov <- vcov
  ## standard errors
  se <- sqrt(diag(vcov))
  output$standard_errors <- se
  ## p-values
  output$pvalues <- 2*pnorm(-abs(beta) / se)
  ## LHS
  output$y <- y
  ## RHS
  output$x <- list(regressors = X, instruments = Z)
  ## r (# of instruments)
  output$r  <- r
  ## K (# of regressors)
  output$K <- K
  ## cs
  output$cs <- cs
  ## t
  output$t <- t

  # model stuff
  ## call
  output$call <- call
  ## formula
  output$formula <- formula(formula)
  ## terms
  output$terms <- list(regressors = mtX, instruments = mtZ, full = mt)
  ## levels
  output$levels <- .getXlevels(mt, mf)
  ## instruments
  output$instrument_names <- instrument_names

  ## OIR if twostep == TRUE
  if(twostep) {
    output$OIR <- OIR
    output$OIR_pvalue <- OIR_pvalue
  }

  ## RMSE
  output$RMSE <- sqrt(mean(as.matrix(e)^2))

  # set the class
  class(output) <- "panelGMM"
  # return
  return(output)

}


print.panelGMM <- function(object, digits = max(3L, getOption("digits") - 3L), ...){

  x <- object

  cat("\nCall:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")

  if(length(coef(x))) {
    cat("Coefficients:\n")
    print.default(format(coef(x)[,1], digits = digits),
                  print.gap = 3L, quote = FALSE)
  } else cat("No coefficients\n")

  cat("\n")

  invisible(object)

}

summary.panelGMM <- function(object, ...){

  x <- object

  # instantiate the output
  output <- list()

  # get the call
  output$call <- x$call
  # get the coefficients
  output$coefficients <- coef(x)[,1]
  # get the standard errors
  output$standard_errors <- x$standard_errors
  # get the pvalues
  output$pvalues <- x$pvalues
  # root mean squared error
  output$RMSE <- x$RMSE
  # cs and t
  output$cs <- x$cs
  output$t <- x$t
  # Z
  output$instrument_names <- x$instrument_names

  # if twostep == TRUE extract the OIR test stat and its pvalue
  if(x$call$twostep) {
    output$OIR <- x$OIR
    output$OIR_pvalue <- x$OIR_pvalue
    output$r <- x$r
    output$K <- x$K
  }

  # set class and return
  class(output) <- "summary.panelGMM"
  return(output)

}


print.summary.panelGMM <- function(object, digits = max(3L, getOption("digits") - 3L), ...) {

  x <- object

  # print the call
  cat("\n-------------------------------------------------------------------\n")
  cat("CALL\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")

  # print details
  cat("\n-------------------------------------------------------------------\n")
  cat("MODEL DETAILS\n\n")
  cat("  Unique cross-sections (N): ", x$cs)
  cat("\n  Unique obs. per cross-section (T): ", x$t)
  cat("\n  Total number of obs. (N x T): ", x$cs * x$t)
  cat("\n  Number of instruments: ", x$r)

  # print the coefficient matrix
  cat("\n\n-------------------------------------------------------------------\n")
  cat("COEFFICIENTS\n")

  res <- matrix(NA,
                nrow = length(x$coefficients),
                ncol = 4,
                dimnames = list(c(1:5), c("Estimate","Std.Error","z-value", "Pr(>|z|)")))

  res[,1] <- x$coefficients
  res[,2] <- x$standard_errors
  res[,3] <- x$coefficients /  x$standard_errors
  res[,4] <- x$pvalues
  rownames(res) <- names(x$coefficients)

  printCoefmat(res, digits = digits, has.Pvalue = TRUE, P.values = TRUE, signif.stars = TRUE, signif.legend = TRUE)

  # print the RMSE
  cat("\n\n-------------------------------------------------------------------\n")
  cat("MODEL SUMMARY\n")
  cat("\nRoot mean square error (RMSE):", round(x$RMSE, digits = digits))

  # if twostep == TRUE print the OIR test
  if(x$call$twostep) {
    r <- x$r
    K <- x$K
    dof <- r - K
    cat("\n\nOveridentification Restrictions Test (OIR)\n")
    cat(
      "   J-statistic:", paste0(round(x$OIR, digits = digits),","),
      paste0(" p-value: ", round(x$OIR_pvalue, digits = digits), ","),
      paste0(" DOF (r - k) = ", dof)
    )
  }

  invisible(x)
}


