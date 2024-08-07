#' @title Calculates recursion information from the trajectory for specific locations
#' 
#' @description A circle of radius R is drawn around each specified location. The number of
#' revisits is calculated as the number of segments of the trajectory passing through that circle. 
#'  
#' @details For specified location, a circle of radius R is drawn around that point. This method differs 
#' from \code{\link{getRecursions}} in that only specified locations are used, rather than all points in the
#' trajectory.
#' Then the number of segments of the trajectory passing through that circle is counted. This is
#' the number of revisits to that location. For each 
#' revisit, the time spent inside the circle is calculated, as well as the time since the last 
#' visit (NA for the first visit). In order to calculate the time values, the crossing time of the
#' radius is calculated by assuming linear movement at a constant speed between the points inside
#' and outside the circle.
#' 
#' \strong{Projection.} Consider the projection used. Since segments are counted passing through circles
#' drawn around points, an equal area projection would ensure similar size comparisons (e.g., \link[move]{spTransform}). 
#' 
#' Either single or multiple individuals are supported, but be aware that this function will be slow with
#' large amounts of data (e.g. millions of points), so consider pre-specifying the locations 
#' (\code{\link{getRecursionsAtLocations}}) or use clustering. Multiple individuals are handled via the \code{id} column of the 
#' data.frame or using a move2 or \link[move]{MoveStack} object.
#' 
#' @param x Either a data frame, move2, \link[move]{Move-class}, or \link[move]{MoveStack} object. For a data frame, 
#' the trajectory data with four columns (the x-coordinate, the y-coordinate, the datetime, and the animal id). 
#' @param locations A data frame with x and y locations at which to calculate the recursions.
#' @param radius numeric radius to use in units of the (x,y) location data to detect recursions.
#' @param threshold a time difference (in units \code{timeunits}) to ignore excursions outside the radius. Defaults to 0.
#' @param timeunits character string specifying units to calculate time differences in for the time spans inside the radius and since the 
#' visit in \code{revisitStats}. Defaults to hours.
#' @param verbose \code{TRUE} to output complete information (can be large for large input data frames) or 
#' \code{FALSE} to output basic information.
#' 
#' @return A list with several components, \code{revisits} and \code{residenceTime}
#' are vectors of the same length as the \code{x} dataframe. \code{revisits} is the number of revisits for each 
#' location, where 1 means that there were 
#' no revisits, only the initial visit. \code{residenceTime} is the total time spent withing the radius. \code{radius}
#' is the specified radius used for all the calculations. \code{timeunits} is the specified time units used to specify
#' timespans.
#' 
#' When \code{verbose = TRUE}, additional information 
#' is also returned, \code{dists} and \code{revisitStats}. Next, \code{dists} gives the distance matrix between
#' all locations. Finally, \code{revisitStats} gives further statistics on each visit. These are calculated 
#' per location (i.e., no aggregation of nearby points is performed), and give the index and location
#' of the point of the track at the center of the radius, the radius entrance and exit time of the track for that 
#' visit, how much time was spent inside the radius, and how long since the last visit (\code{NA} for the first visit).
#' 
#' 
#' @author Chloe Bracis <cbracis@uw.edu>
#' @seealso \code{\link{getRecursions}}
#' 
#' @examples
#' data(martin)
#' locations = data.frame(x = c(-10, 0, 20), y = c(5, 0, 0))
#' revisits = getRecursionsAtLocations(martin, locations, radius = 1)
#' plot(revisits, locations, legendPos = c(10, -15), 
#'      alpha = 1, pch = 17, xlim = range(martin$x), ylim = range(martin$y))
#' points(martin$x, martin$y, pch = ".", col = "gray50")
#' drawCircle(10, -10, 1)
#' 
#' @export
#' @name getRecursionsAtLocations
#' 
getRecursionsAtLocations = function(x, locations, radius, threshold = 0, timeunits = c("hours", "secs", "mins", "days"), verbose = TRUE)
{
	UseMethod('getRecursionsAtLocations', x)
}
