#' Shift columns to create lags, leads or differences
#'
#' Create \emph{n}-lagged/leaded/differenced columns in a data frame with automatic names.
#'
#' @author Lawrence R De Geest, \email{lrdegeest@@gmail.com}
#' @param data data of class \code{data.frame}
#' @param panel the panel or cross-section variable name in the data
#' @param type shift type. either "lag", "lead" or "difference"
#' @param shifts the number of lags, leads or differences
#' @param columns the columns in `data` to be lagged, leaded or differenced
#' @return \code{data.frame} / \code{data.table} with new variables
#' @note The function will automatically coerce the data.frame object to a \code{data.table} object.  (\code{data.table} objects retain their \code{data.frame} class.)
#' @import data.table
#' @examples
#' # Using the hours and wages data provided by the panelGMM package:
#' data("hours_wages")
#' # shiftCols() is designed to work nicely with the pipe operator `%>%` from `magrittr`:
#' hours_wages_gmm = hours_wages %>%
#'    shiftCols(data = ., panel=id, type = "difference", shifts = 1, columns=c("lnhr", "lnwg", "kids", "age", "agesq", "disab")) %>%
#'    shiftCols(data = ., panel=id, type = "lag", shifts = 1:4, columns=c("lnhr", "lnwg", "kids", "age", "agesq", "disab"))
#' @export

shiftCols <- function(data, panel, type=c("lag", "lead", "difference"), shifts, columns) {

  
  # Sanity checks
  ## check if data frame
  if(any(!("data.frame" %in% class(data)))) stop("Data must be a data.frame object", call. = FALSE)
  ## use the right type
  if(any(!(type %in% c("lag", "lead", "difference")))) stop("please choose 'lag', 'lead', or 'difference'", call. = FALSE)
  ## columns better be in the data
  if(any(!(columns %in% names(data)))) stop("Columns must be in the data. Check that you spelled them correctly.", call. = FALSE)
  # coerce to data.table
  if(!("data.table" %in% class(data))) setDT(data)

  # extract the panel id
  input <- match.call()
  panel_id <- input[["panel"]]

  # LAGS
  if(type=="lag") {
    new_vars = data[, shift(.SD, n=shifts, type='lag', fill=NA, give.names = TRUE), by=panel_id, .SDcols = columns][,-1]
    return_data = cbind(data, new_vars)
    return(return_data)
  }

  # LEADS
  if(type=="lead") {
    new_vars = data[, shift(.SD, n=shifts, type='lead', fill=NA, give.names = TRUE), by=panel_id, .SDcols = columns]
    return_data = cbind(data, new_vars)
  }

  # DIFFERENCES
  if(type=="difference"){
    diff_list = vector(mode = "list", length = length(shifts))
    for(i in seq_along(shifts)) {
      col_names <- paste0(columns, "_diff_", i)
      diff_list[[i]] <- setnames(data[, (.SD - shift(.SD, i, type='lag')), by=panel_id, .SDcols = columns][,-1], col_names)
    }
    diff_frames = do.call(cbind, diff_list)
    return_data = cbind(data, diff_frames)
  }

  # RETURN
  return(return_data)
}





