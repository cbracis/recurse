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
#' @param xyt data.frame. the trajectory data with three columns, x the x-coordinate, 
#' y the y-cordinate, and t the datetime.
#' @param radius numeric. radius to use in units of the location data to detect recursions.
#' @param threshold a time diference (in units \code{timeunits}) to ignore excursions outside the radius.
#' @param timeunits character string specifying units to calculate time differences in for the time spans inside the radius and since the 
#' visit in \code{revisitStats}. Defaults to hours.
#' @param verbose \code{TRUE} to output complete information (can be large for large input data frames) or 
#' \code{FALSE} to output basic information.
#' 
#' @return A list with several components. \code{revisits}, \code{firstPassageTime}, \code{residenceTime}
#' are vectors of the same length as the xyt dataframe. \code{revisits} is the number of revisits for each 
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
#' 
getRecursions = function(xyt, radius, threshold = 0, timeunits = c("hours", "secs", "mins", "days"), verbose = TRUE)
{
	# ideas for large data sets:
	# optionally specify locations
	# specify number of clusters and take median of each cluster	
	
	stopifnot(is.data.frame(xyt))
	stopifnot(ncol(xyt) == 3)
	stopifnot(radius > 0)
	timeunits = match.arg(timeunits)
	
	n = nrow(xyt) # number of data points
	z = complex(real = xyt[,1], imaginary = xyt[,2])
	t = xyt[,3]
	
	getDists <- function(a, b)		
	{
		return(Mod(a - b))
	}
	
	# speed improvements:
	# outer on subtract instead, then mod - test if faster
	# get rid of complex numbers
	distMatrix = outer(z, z, getDists)
	
	revisits = rep(0, n) # start with 0 revisit per location
	fpt = vector("numeric", n)
	rt = vector("numeric", n)
	
	allRevisitStats = vector(mode="list", n)
	
	# index of some statistics columns since these aren't named
	statsColNames = c("x", "y", "coordIdx", "visitIdx", 
					  "entranceTime", "exitTime", 
					  "timeInside", "timeSinceLastVisit")
	exitTimeIdx = which(statsColNames == "exitTime")
	timeInsideIdx = which(statsColNames == "timeInside")
	
	# for each location, calculate
	for (i in 1:n)
	{
		revisitStats = NULL
		
		# find relocations within radius
		inRadius = (distMatrix[,i] <= radius)
		
		# forewards (next locations that are revisits)
		stillInside = ifelse(inRadius[1], TRUE, FALSE) # start with animal inside radius?
		appendToPreviousRevisit = FALSE
		radiusEntranceTime = if (stillInside) { t[1] } else { NA } # avoid ifelse which converts posix to numeric
		radiusExitTime = NA
		timeSinceLastVisit = NA
		revisitIndex = 0
		
		for (j in 2:n) 
		{
			if (!inRadius[j]) # is location outside radius?
			{
				if (stillInside) 
				{
					# animal just moved outside
					stillInside = FALSE
					percentIn = .calculateCrossingPercentageCmplx(z[i], z[j-1], z[j], radius)
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
						revisits[i] = revisits[i] + 1
						stats = data.frame(Re(z[i]), Im(z[i]), i, revisits[i], radiusEntranceTime, radiusExitTime, 
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
					percentIn = .calculateCrossingPercentageCmplx(z[i], z[j], z[j-1], radius)
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
		} # j loop
		
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
				stats = data.frame(Re(z[i]), Im(z[i]), i, revisits[i], radiusEntranceTime, radiusExitTime, 
								   timeInside, timeSinceLastVisit)
				names(stats) = statsColNames
				revisitStats = rbind(revisitStats,  stats)	
			}
		}
		
		# done for point i, summarize
		# drop = TRUE should be the default but otherwise sometimes returns data.fram instead of atomic value
		fpt[i] = revisitStats[1, timeInsideIdx, drop = TRUE]
		rt[i] = sum(revisitStats[,timeInsideIdx, drop = TRUE], na.rm = TRUE)

		
		if (verbose)
		{
			allRevisitStats[[i]] = revisitStats
		}
		
	} # i loop
		
	results = list(revisits = revisits, 
				   firstPassageTime = as.difftime(fpt, units = timeunits),
				   residenceTime = as.difftime(rt, units = timeunits),
				   radius = radius)
	class(results) = "recurse"
	
	if (verbose)
	{
		allRevisitStats = do.call("rbind", allRevisitStats)
		names(allRevisitStats) = statsColNames
		allRevisitStats$timeSinceLastVisit = as.difftime(allRevisitStats$timeSinceLastVisit, units = timeunits) # becasue first is always NA, loses difftime class
		
		results = c(results, list(dists = distMatrix, revisitStats = allRevisitStats))
		class(results) = c("recurse", "recurse.verbose")
	}
	
	return(results)
}



