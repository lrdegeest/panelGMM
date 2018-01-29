#' Difference multiple variables
#'
#' Create \emph{n}-differenced independent and dependent variables in a data frame. Assumes dependent variable names begin with "y_" and independent variable names begin with "x_". This makes it easier to difference many variables at once. If you variables are not so named, you can rename them with \code{panelGMM::renameVars}.
#'
#' @author Lawrence R De Geest, \email{lrdegeest@@gmail.com}
#' @param data data of class \code{data.frame}
#' @param panel the panel or cross-section variable name in the provided data
#' @param ndiffs the number of differences (e.g. 1, 2, ...)
#' @param x difference the indenpendent variables (defaults to TRUE)
#' @param y difference the indenpendent variables (defaults to TRUE)
#' @return \code{data.table} with differenced variables
#' @note Requires \code{library(data.table)}. The function will automatically coerce the data.frame object to \code{data.table}. Note that \code{data.table} objects retain their \code{data.frame} class.
#' @export
#' @examples
#' # Using the hours and wages data provided by the panelGMM package:
#' data("hours_wages", package = "panelGMM")
#' # first difference all variables
#' hours_wages_gmm <- makeDIFFS(hours_wages, id, 1)

makeDIFFS <- function(data, panel, ndiffs, x = TRUE, y = TRUE) {
  if(!(x) && !(y)) stop("Please select independent and/or dependent variables to difference")
  require(data.table)
  if(class(data)[1] != "data.table") data <- data.table(data)
  input <- match.call()
  panel_id <- input[["panel"]]
  if(!(x) && (y)) pattern <- "^y_"
  else if((x) && !(y)) pattern <- "^x_"
  else pattern <- "^y_|^x_"
  names_data <- grep(pattern, colnames(data), value = T)
  if(length(names_data) == 0) stop("Dependent variables should begin with 'y_', independent variables with 'x_'.\nUse panelGMM::renameVars to rename variables accordingly.")
  names_d <- paste(paste0("d",ndiffs), names_data, sep = "_")
  data_diff <- data[, `:=`((names_d), (.SD - shift(.SD, ndiffs))),
                    by = panel_id,
                    .SDcols = names_data]
  return(data_diff)
}
