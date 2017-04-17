#' @title Computes revisitation metrics for trajectory data
#'
#' @description Computes revisitation metrics for trajectory data, such as the number of revisitations for each location 
#' as well as the time spent for that visit and the time since the previous visit. Also includes functions to 
#' plot data and generate correlated random walks.
#' 
#' @details The function \code{\link{getRecursions}} computes the revisit metrics, which can be plotted with 
#' \code{\link{plot.recurse}}.
#' 
#' Functions are also available to generate correlated random walks from probability distributions or 
#' empirical distributions of steps and angles with \code{\link{generateCRW}} and 
#' \code{\link{generateEmpiricalCRW}}. The walk can also be bounded by a polygon (for example to keep it in
#' the correct habitat, land, water, etc.) with \code{\link{generateBoundedEmpiricalCRW}}.
#'
#' @author Chloe Bracis <cbracis@uw.edu>
#' 
#' @name recurse
#' @docType package
#'@importFrom Rcpp evalCpp
#'@useDynLib recurse
NULL

#' Sample trajectory.
#'
#' A dataset containing a sample trajectory with revisits.
#'
#' \itemize{
#'   \item x. x-coordinate
#'   \item y. y-coordinate
#'   \item t. time
#' }
#'
#' @docType data
#' @keywords datasets
#' @name track
#' @usage data(track)
#' @format A data frame with 100 rows and 3 columns
NULL