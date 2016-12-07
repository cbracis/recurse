#' @title Calculates recursion information from the trajectory
#' 
#' @description A circle of radius R is drawn around each point in the trajectory. The number of
#' revists is calculated as the number of segments of the trajectory passing through that circle. 
#'  
#' @details For each point in the trajectory, a circle of radius R is drawn around that point.
#' Then the number of segments of the trajectory passing through that circle is counted, this is
#' the number of revisits, so each point will have at least one revisit (the initial visit). For each 
#' revist, the time spent inside the circle is calculated, as well as the time since the last 
#' visit (NA for the first visit). In order to calculate the time values, the crossing time of the
#' radius is calucluated by assuming linear movement at a constant speed between the points inside
#' and outside the circle.
#' 
#' \strong{Projection.} Consider the projection used. Since segments are counted passing through circles
#' drawn around points, an equal area projection would ensure similar size comparisons. 
#' 
#' Either single or multiple individuals are supported, but be aware that this function will be slow with
#' large amounts of data (e.g. tens of thousands of rows), so consider running the versions that pre-specify
#' recursion points or use clustering. Multiple individuals are handles via the \code{id} column of the 
#' data.frame or using a MoveStack object.


#' 
#' @param x Either a data frame, Move, or MoveStack object. For a data frame, 
#' the trajectory data with four columns, \code{x} the x-coordinate, 
#' \code{y} the y-cordinate, \code{t} the datetime, and \code{id} the numeric animal id. 
#' @param radius numeric. radius to use in units of the location data to detect recursions.
#' @param threshold a time diference (in units \code{timeunits}) to ignore excursions outside the radius.
#' @param timeunits character string specifying units to calculate time differences in for the time spans inside the radius and since the 
#' visit in \code{revisitStats}. Defaults to hours.
#' @param verbose \code{TRUE} to output complete information (can be large for large input data frames) or 
#' \code{FALSE} to output basic information.
#' 
#' @return A list with several components. \code{revisits}, \code{firstPassageTime}, \code{residenceTime}
#' are vectors of the same length as the x dataframe. \code{revisits} is the number of revisits for each 
#' location, where 1 means that there were 
#' no revisits, only the initial visit. \code{firstPassageTime} is the first passage time for the visit 
#' through the focal coordinate. \code{residenceTime} is the total time spent withing the radius. \code{radius}
#' is the spcified radius used for all the calculations. When \code{verbose = TRUE}, additional inforamtion 
#' is also returned, \code{dists} and \code{revisitStats}.Next, \code{dists} gives the distance matrix between
#' all ocations. Finally, \code{revisitStats} gives further statistics on each visit. These are calculated 
#' per location (i.e., no aggregation of nearby points is performed), and give the index and location
#' of the point of the track at the center of the radius, the radius entrace and exit time of the track for that 
#' visit, how much time was spent inside the radius, and how long since the last visit (NA for the first visit).
#' 
#' @export
#' @name getRecursions
#' 
getRecursions <- function(x, radius, threshold = 0, timeunits = c("hours", "secs", "mins", "days"), verbose = TRUE)
{
	UseMethod('getRecursions', x)
}



