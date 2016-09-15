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
#' @param units character string specifying units to calculate time differences in for the time spans inside the radius and since the 
#' visit in \code{revisitStats}. Defaults to hours.
#' 
#' @return A list with several components. First, \code{revisits} is a vector of the same length as the xyt 
#' dataframe that gives the number of revisits for each location, where 1 means that there were 
#' no revisits, only the initial visit. Next, \code{dists} gives the distance matrix between
#' all ocations. Finally, \code{revisitStats} gives further statistics on each visit. These are calculated 
#' per location (i.e., no aggregation of nearby points is performed), and give the index and location
#' of the point of the track at the center of the radius, the radius entrace and exit time of the track for that 
#' visit, how much time was spent inside the radius, and how long since the last visit (NA for the first visit).
#' 
#' @export
#' 
getRecursions = function(xyt, radius, units = c("hours", "secs", "mins", "days"))
{
	stopifnot(is.data.frame(xyt))
	stopifnot(ncol(xyt) == 3)
	stopifnot(radius > 0)
	units = match.arg(units)
	
	n = nrow(xyt) # number of data points
	z = complex(real = xyt[,1], imaginary = xyt[,2])
	t = xyt[,3]
	
	getDists <- function(a, b)		
	{
		return(Mod(a - b))
	}
	
	distMatrix = outer(z, z, getDists)
	
	revisits = rep(0, n) # start with 1 revisit per location
	revisitStats = NULL
	
	# for each location, calculate
	for (i in 1:n)
	{
		# find relocations within radius
		inRadius = (distMatrix[,i] <= radius)
		
		# forewards (next locations that are revisits)
		stillInside = ifelse(inRadius[1], TRUE, FALSE) # start with animal inside radius?
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
					
					revisits[i] = revisits[i] + 1
					stats = data.frame(Re(z[i]), Im(z[i]), i, revisits[i], radiusEntranceTime, radiusExitTime, 
									   difftime(radiusExitTime, radiusEntranceTime, units = units), 
									   timeSinceLastVisit)
					revisitStats = rbind(revisitStats, stats)	# setNames(stats, names(revisitStats))
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
					timeSinceLastVisit = difftime(radiusEntranceTime, radiusExitTime, units = units)
					
					# could use time calculation to ignore brief trips outside
					
				}
			}		
		} # j loop
		
		if (stillInside)
		{
			# last segment is in radius
			revisits[i] = revisits[i] + 1
			radiusExitTime = t[j]
			stats = data.frame(Re(z[i]), Im(z[i]), i, revisits[i], radiusEntranceTime, radiusExitTime, 
							   difftime(radiusExitTime, radiusEntranceTime, units = units), 
							   timeSinceLastVisit)
			revisitStats = rbind(revisitStats,  stats)					
		}
		
	} # i loop
	
	names(revisitStats) = c("x", "y", "coordIdx", "visitIdx", 
							"entranceTime", "exitTime", 
							"timeInside", "timeSinceLastVisit")
	revisitStats$timeSinceLastVisit = as.difftime(revisitStats$timeSinceLastVisit, units = units) # becasue first is always NA, loses difftime class
	results = list(revisits = revisits, dists = distMatrix, revisitStats = revisitStats)
	attr(results, "class") = "recurse"
	
	return(results)
}



