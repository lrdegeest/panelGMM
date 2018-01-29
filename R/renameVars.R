#' Rename variables in a data frame
#'
#' Append prefixes "y_" and "x_" respectively to the names of dependent and indenpent variables. This facilitates differencing and lagging variables in batches using \code{panelGMM::makeDIFFS} and \code{panelGMM::makeLAGS}.
#'
#' @author Lawrence R De Geest, \email{lrdegeest@@gmail.com}
#' @param data save csv files of each year's data to your directory. Defaults to FALSE.
#' @param yvars vector (\code{c()}) or single dependent variable name(s)
#' @param xvars vector (\code{c()}) or single independent variable name(s)
#' @return \code{data.frame} with "x_" prepended to all independent variables names and "y_" prepended to all dependent variable names
#' @export
#' @examples
#' # Using the hours and wages data provided by the panelGMM package:
#' data("hours_wages", package = "panelGMM")
#' hours_wages_gmm <- renameVars("lnhr", c("lnwg", "kids", "age", "agesq", "disab"), hours_wages)

renameVars <- function(data, yvars, xvars) {
  names <- colnames(data)
  for(i in names) {
    if(i %in% yvars) names[names == i] <- paste0("y_", i)
    else if(i %in% xvars) names[names == i] <- paste0("x_", i)
  }
  colnames(data) <- names
  return(data)
}
