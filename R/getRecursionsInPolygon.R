#' Calculates recursion information from the trajectory inside a polygon
#'
#' @description The number of  revisits to a polygon is calculated as the number of segments of the trajectory 
#' passing through the polygon. 
#'  
#' @details The number of segments of the trajectory passing through the polygon is counted as 
#' the number of revisits. For each  revisit, the time spent inside the polygon is calculated, as 
#' well as the time since the last visit (NA for the first visit). In order to calculate the time values, 
#' the crossing time of the polygon is calculated by assuming linear movement at a constant speed between 
#' the points inside and outside the polygon. Note the polygon must be convex as described in further detail below.
#' 
#' \strong{Projection.} Consider the projection used. Since segments are counted passing through the
#' polygon, an equal area projection would ensure similar size comparisons. 
#' A geographic projection is not appropriate. The projection for the 
#' polygon and the trajectory must be the same.
#' 
#' \strong{Polygon.} The polygon must be specified as a \link[sf]{st_polygon} object. It should consist
#' of a single polygon (i.e. \link[sf]{st_geometry_type} = POLYGON). It should further be convex, though this
#' requirement is not enforced, calculations for non-convex polygons will not necessarily be accurate. It may
#' be advantageous to simplify complex geometry in order to shorten the time to run. If it is necessary to use a
#' non-convex polygon, one approach would be to split it into convex pieces that can be run one-by-one. However,
#' some visits would then be double-counted and would need to be combined back together based on the
#' entrance/exit times and sequence of trajectory locations. Multiple polygons would need to be handled with multiple
#' calls with the output then concatenated together.
#' 
#' Either single or multiple individuals are supported, but be aware that this function will be slow with
#' large amounts of data (e.g. millions of points). Multiple individuals are handled via the \code{id} column of the 
#' data.frame.
#'
#' @param trajectory Either a data frame, move2, Move-class, or MoveStack object. For a data frame, the trajectory data with four columns (the x-coordinate, the y-coordinate, the datetime, and the animal id)
#' @param polygon A \link[sf]{st_polygon} object with a single convex polygon.
#' @param threshold A time difference (in units \code{timeunits}) to ignore excursions outside the radius. Defaults to 0.
#' @param timeunits Character string specifying units to calculate time differences in for the time spans inside the radius and since the 
#' visit in \code{revisitStats}. Defaults to hours.
#' @param verbose \code{TRUE} to output complete information (can be large for large input data frames) or 
#' \code{FALSE} to output basic information.
#'
#' @return A list with several components. \code{revisits} is the number of revisits to the polygon. \code{residenceTime} is the total time 
#' spent withing the polygon. \code{radius} is NA in the case of polygons. \code{timeunits} is the specified time units used to specify timespans.
#' 
#' When \code{verbose = TRUE}, additional information 
#' is also returned in \code{revisitStats}. Next, \code{dists} gives the distance matrix between
#' all locations. Finally, \code{revisitStats} gives further statistics on each visit. These are calculated 
#' per location (i.e., no aggregation of nearby points is performed), and give the index and location
#' of the point of the track at the center of the radius (NA and 1 in the case of polygons), the radius entrance and exit time of the track for that 
#' visit, how much time was spent inside the radius, and how long since the last visit (\code{NA} for the first visit).
#' 
#' 
#' @author Chloe Bracis <cbracis@uw.edu>
#' @seealso \code{\link{getRecursions}}
#'
#' @examples
#' if (requireNamespace("sf")) 
#' {
#'     data(track)
#'     poly = sf::st_polygon(list(cbind(c(4,6,6,3,4), c(1,2,4,3,1))))
#'     poly = sf::st_sfc(poly, crs = "EPSG:3410")
#'     revisits = getRecursionsInPolygon(track, poly)
#' }
#' @export
#' @name getRecursionsInPolygon
#' 
getRecursionsInPolygon = function(trajectory, polygon, threshold = 0, timeunits = c("hours", "secs", "mins", "days"), verbose = TRUE)
{
	UseMethod('getRecursionsInPolygon', trajectory)
}