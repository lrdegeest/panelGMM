#' Tidy a `panelGMM` object
#'
#' Turns a `panelGMM` object into a tidy data frame.
#'
#' @author Lawrence R De Geest, \email{lrdegeest@@gmail.com}
#' @param object a `panelGMM` object 
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
#' # view the results (coefficients, standard errors, p-values) as a data frame:
#' tidy_panelGMM(model_results)


tidy_panelGMM <- function(object) {
  
  x <- object
  
  res <- data.frame("terms" = rownames(x$coefficients),
                    "estimates" = x$coefficients,
                    "standard.errors" = x$standard_errors,
                    "z-values" = x$coefficients / x$standard_errors,
                    "p-values" = x$pvalues)
  
  rownames(res) <- NULL
  
  return(res)
}
