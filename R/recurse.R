#' @title Computes revisitation metrics for trajectory data
#'
#' @description Computes revisitation metrics for trajectory data, such as the number of revisitations for each location 
#' as well as the time spent for that visit and the time since the previous visit. Also includes functions to 
#' plot data.
#' 
#' @details The function \code{\link{getRecursions}} computes the revisit metrics, which can be plotted with 
#' \code{\link{plot.recurse}}. Alternatively, \code{\link{getRecursionsAtLocations}} computes revisit metrics
#' for specified locations, rather than all locations in the movement trajectory.
#' 
#' @author Chloe Bracis <cbracis@uw.edu>
#' 
#' @name recurse
#' @docType package
#'@importFrom Rcpp evalCpp
#'@useDynLib recurse
NULL

#' Sample trajectory (track).
#'
#' A dataset containing a sample trajectory with revisits.
#'
#' \itemize{
#'   \item x. x-coordinate
#'   \item y. y-coordinate
#'   \item t. time
#'   \item id. identifier
#' }
#'
#' @docType data
#' @keywords datasets
#' @name track
#' @usage data(track)
#' @format A data frame with 100 rows and 4 columns
NULL

#' Sample trajectory (martin).
#'
#' A dataset containing a sample trajectory with revisits.
#'
#' \itemize{
#'   \item x. x-coordinate
#'   \item y. y-coordinate
#'   \item t. time
#'   \item id. identifier
#' }
#'
#' @docType data
#' @keywords datasets
#' @name martin
#' @usage data(martin)
#' @format A data frame with 600 rows and 4 columns
NULL

#' Sample trajectory (wren).
#'
#' A dataset containing a sample trajectory with revisits.
#'
#' \itemize{
#'   \item x. x-coordinate
#'   \item y. y-coordinate
#'   \item t. time
#'   \item id. identifier
#' }
#'
#' @docType data
#' @keywords datasets
#' @name wren
#' @usage data(wren)
#' @format A data frame with 600 rows and 4 columns
NULL