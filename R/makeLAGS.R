#' Lag multiple variables
#'
#' Create \emph{n}-lagged independent and dependent variables in a data frame. Assumes dependent variable names begin with "y_" and independent variable names begin with "x_". This makes it easier to lag many variables at once. If you variables are not so named, you can rename them with \code{panelGMM::renameVars}.
#'
#' @author Lawrence R De Geest, \email{lrdegeest@@gmail.com}
#' @param data data of class \code{data.frame}
#' @param panel the panel or cross-section variable name in the provided data
#' @param nlags the number of lags (e.g. 1, 2, ...)
#' @param x difference the indenpendent variables (defaults to TRUE)
#' @param y difference the indenpendent variables (defaults to TRUE)
#' @return \code{data.table} with lagged variables
#' @note Requires \code{library(data.table)}. The function will automatically coerce the data.frame object to \code{data.table}. Note that \code{data.table} objects retain their \code{data.frame} class.
#' @export
#' @examples
#' # Using the hours and wages data provided by the panelGMM package:
#' data("hours_wages", package = "panelGMM")
#' # twice lag the independent variables:
#' hours_wages_gmm <- makeLAGS(hours_wages, id, 2, y = FALSE)

makeLAGS <- function(data, panel, nlags, x = TRUE, y = TRUE) {
  if(!(x) && !(y)) stop("Please select independent and/or dependent variables to lag.")
  require(data.table)
  if(class(data)[1] != "data.table") data <- data.table(data)
  input <- match.call()
  panel_id <- input[["panel"]]
  if(!(x) && (y)) pattern <- "^y_"
  else if((x) && !(y)) pattern <- "^x_"
  else pattern <- "^y_|^x_"
  names_data <- grep(pattern, colnames(data), value = T)
  if(length(names_data) == 0) stop("Dependent variables should begin with 'y_', independent variables with 'x_'.\nUse panelGMM::renameVars to rename variables accordingly.")
  names_lag <- paste(paste0("l",nlags), names_data, sep = "_")
  data_lag <- data[, `:=`((names_lag), (shift(.SD, nlags))),
                   by = panel_id,
                   .SDcols = names_data]
  return(data_lag)
}
