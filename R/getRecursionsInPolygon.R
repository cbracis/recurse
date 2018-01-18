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
#' polygon, an equal area projection would ensure similar size comparisons (e.g., \link[move]{spTransform}). 
#' A geographic projection is not appropriate. The projection for the 
#' polygon and the trajectory must be the same.
#' 
#' \strong{Polygon.} The polygon must be specified as a \link[sp]{SpatialPolygons} object. It should consist
#' of a single polygon (rather than a list of multiple polygons). It should further be convex, though this
#' requirement is not enforced, canculations for non-convex polygons will not necessarily be accurate. It may
#' be advantagous to simplify complex geometry in order to shorten the time to run. If it is necessary to use a
#' non-convex polygon, one approach would be to split it into convex pieces that can be run one-by-one. However,
#' some visits would then be double-counted and would need to be combined back together based on the
#' entrance/exit times and sequence of trajectory locations.
#' 
#' Either single or multiple individuals are supported, but be aware that this function will be slow with
#' large amounts of data (e.g. millions of points). Multiple individuals are handled via the \code{id} column of the 
#' data.frame.
#'
#' @param trajectory A data frame with four columns (the x-coordinate, the y-coordinate, the datetime, and the animal id).
#' @param polygon A \link[sp]{SpatialPolygons} object with a single convex polygon.
#' @param threshold A time difference (in units \code{timeunits}) to ignore excursions outside the radius. Defaults to 0.
#' @param timeunits Character string specifying units to calculate time differences in for the time spans inside the radius and since the 
#' visit in \code{revisitStats}. Defaults to hours.
#' @param verbose \code{TRUE} to output complete information (can be large for large input data frames) or 
#' \code{FALSE} to output basic information.
#'
#' @return A list with several components, \code{revisits} and \code{residenceTime}
#' are vectors of the same length as the \code{trajectory} dataframe. \code{revisits} is the number of revisits for each 
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
#' data(track)
#' poly = sp::SpatialPolygons( list(
#' 	 	sp::Polygons( list(sp::Polygon(cbind(c(4,6,6,3,4),c(1,2,4,3,1)))), ID = 1 )
#' 	 	))
#' revisits = getRecursionsInPolygon(track, poly)

#' @export
getRecursionsInPolygon = function(trajectory, polygon, threshold = 0, timeunits = c("hours", "secs", "mins", "days"), verbose = TRUE)
{
	if (!requireNamespace("sp", quietly = TRUE)) 
	{
		stop("sp package needed for this function to work. Please install it.",
			 call. = FALSE)
	}
	if (!requireNamespace("rgeos", quietly = TRUE)) 
	{
		stop("rgeos package needed for this function to work. Please install it.",
			 call. = FALSE)
	}
	
	stopifnot(is.data.frame(trajectory))
	stopifnot(ncol(trajectory) == 4)
	stopifnot(inherits(polygon, "SpatialPolygons"))
	stopifnot(length(polygon@polygons) == 1)
	stopifnot(length(polygon@polygons[[1]]@Polygons) == 1)
	#TODO convex?
	timeunits = match.arg(timeunits)
	
	# data used in function
	nTraj = nrow(trajectory) # number of trajectory locations
	t = trajectory[,3]
	idIdx = 4
	isNewTrack = as.logical(c(1, diff(as.numeric(trajectory[,idIdx]))))
	revisits = 0
	proj4str = sp::CRS(sp::proj4string(polygon))
	
	calculateCrossingPercentage = function(x1, y1, x2, y2)
	{
		lines <- matrix(c(x1, x2, y1, y2), 2, 2)
		line = sp::SpatialLines(
			list(
				sp::Lines(list( sp::Line(lines)), ID = '1') ),
			proj4string = proj4str)
		
		line.in = rgeos::gIntersection(polygon, line)
		percent = rgeos::gLength(line.in) / rgeos::gLength(line)
		return( percent )
	}
	
	
	revisitStats = NULL
	i = 1
	
	# index of some statistics columns since these aren't named
	statsColNames = c("id", "x", "y", "coordIdx", "visitIdx", 
					  "entranceTime", "exitTime", 
					  "timeInside", "timeSinceLastVisit")
	exitTimeIdx = which(statsColNames == "exitTime")
	timeInsideIdx = which(statsColNames == "timeInside")
	
	
	# find relocations within radius (use column for loc i)
	inPoly = !is.na(sp::over(sp::SpatialPoints(trajectory[,1:2], proj4string = proj4str), polygon))

	
	for (j in 1:nTraj) 
	{
		if (isNewTrack[j])
		{
			if (j != 1)
			{
				# need to report final revisit from previous track
				if (stillInside)
				{
					# last segment j-1 is in radius
					radiusExitTime = t[j-1]
					timeInside = difftime(radiusExitTime, radiusEntranceTime, units = timeunits)
					
					if (appendToPreviousRevisit)
					{
						# update time inside with brief excursion time
						lastRowIdx = nrow(revisitStats)
						revisitStats[lastRowIdx, timeInsideIdx] = revisitStats[lastRowIdx, timeInsideIdx] + timeInside
						revisitStats[lastRowIdx, exitTimeIdx] = radiusExitTime
					}
					else
					{
						revisits = revisits + 1
						stats = data.frame(trajectory[j-1, idIdx], NA, NA, i, revisits, radiusEntranceTime, radiusExitTime, 
										   timeInside, timeSinceLastVisit)
						names(stats) = statsColNames
						revisitStats = rbind(revisitStats,  stats)	
					}
				}
			} # end i
			
			# reset variables for new trajectory
			stillInside = inPoly[j] # start with animal inside radius?
			appendToPreviousRevisit = FALSE
			radiusEntranceTime = if (stillInside) { t[j] } else { NA } # avoid ifelse which converts posix to numeric
			radiusExitTime = NA
			timeSinceLastVisit = NA
		} # end if new track
		else
		{
			if (!inPoly[j]) # is location outside radius?
			{
				if (stillInside) 
				{
					# animal just moved outside
					stillInside = FALSE
					percentIn = calculateCrossingPercentage(trajectory[j-1, 1], trajectory[j-1, 2], 
															trajectory[j, 1], trajectory[j, 2])
					radiusExitTime = t[j-1] + percentIn * (t[j] - t[j-1])
					timeInside = difftime(radiusExitTime, radiusEntranceTime, units = timeunits)
					
					if (appendToPreviousRevisit)
					{
						# update exit time and time inside with current 'visit'
						lastRowIdx = nrow(revisitStats)
						revisitStats[lastRowIdx, timeInsideIdx] = revisitStats[lastRowIdx, timeInsideIdx] + timeInside
						revisitStats[lastRowIdx, exitTimeIdx] = radiusExitTime
					}
					else
					{
						revisits = revisits + 1
						stats = data.frame(trajectory[j,idIdx], NA, NA, i, revisits, radiusEntranceTime, radiusExitTime, 
										   timeInside, timeSinceLastVisit)
						names(stats) = statsColNames
						revisitStats = rbind(revisitStats, stats)
					}
				}
			}
			else # location inside circle
			{
				if (!stillInside) 
				{
					# animal just moved inside
					stillInside = TRUE
					percentIn = calculateCrossingPercentage(trajectory[j-1, 1], trajectory[j-1, 2], 
															trajectory[j, 1], trajectory[j, 2])
					radiusEntranceTime = t[j] - percentIn * (t[j] - t[j-1])
					timeSinceLastVisit = difftime(radiusEntranceTime, radiusExitTime, units = timeunits)
					
					# use threshold to ignore brief trips outside
					appendToPreviousRevisit = if (!is.na(timeSinceLastVisit) & timeSinceLastVisit < threshold) 
					{ TRUE } else { FALSE }
					
					if (appendToPreviousRevisit)
					{
						# update time inside with brief excursion time
						lastRowIdx = nrow(revisitStats)
						revisitStats[lastRowIdx, timeInsideIdx] = revisitStats[lastRowIdx, timeInsideIdx] + timeSinceLastVisit
					}
				}
			}	
		}
	} # j loop
	
	# report final revisit if any
	if (stillInside)
	{
		# last segment is in radius
		radiusExitTime = t[j]
		timeInside = difftime(radiusExitTime, radiusEntranceTime, units = timeunits)
		
		if (appendToPreviousRevisit)
		{
			# update time inside with brief excursion time
			lastRowIdx = nrow(revisitStats)
			revisitStats[lastRowIdx, timeInsideIdx] = revisitStats[lastRowIdx, timeInsideIdx] + timeInside
			revisitStats[lastRowIdx, exitTimeIdx] = radiusExitTime
		}
		else
		{
			revisits[i] = revisits[i] + 1
			stats = data.frame(trajectory[j, idIdx], NA, NA, i, revisits, radiusEntranceTime, radiusExitTime, 
							   timeInside, timeSinceLastVisit)
			names(stats) = statsColNames
			revisitStats = rbind(revisitStats,  stats)	
		}
	}
	
	# done for point i, summarize
	# drop = TRUE should be the default but otherwise sometimes returns data.fram instead of atomic value
	rt = sum(revisitStats[,timeInsideIdx, drop = TRUE], na.rm = TRUE)
	
	results = list(revisits = revisits, 
				   residenceTime = as.difftime(rt, units = timeunits),
				   radius = NA)
	class(results) = "recurse"
	
	if (verbose)
	{
		#names(revisitStats) = statsColNames
		revisitStats$timeSinceLastVisit = as.difftime(revisitStats$timeSinceLastVisit, units = timeunits) # becasue first is always NA, loses difftime class
		
		results = c(results, list(revisitStats = revisitStats))
		class(results) = c("recurse", "recurse.verbose")
	}
	
	return(results)
	
}