#' @title Calculates residence time within user-specified breaks
#' 
#' @description Using the results from \code{\link{getRecursions}} or \code{\link{getRecursionsAtLocations}},
#' calculates the residence time during user-specified intervals (rather than the entire trajectory period) in
#' the radius around each location.
#' 
#' @details When recursions are calculated, the residence time in the radius around each location is also 
#' calculated. This method allows the user to post-process the results from calculating recursions to calculate
#' residence time over user-specified intervals, rather than the entire trajectory. This allows the calculation
#' of residence time on biologically relevant scales, such as seasons, and in cases where large gaps between 
#' visits (e.g., a seasonal migrant) may make splitting up the residence time preferable.
#' 
#' Note that care should be taken to use the same time zone when specifying the break points as used in the
#' datetime for the movement trajectory.
#' 
#' @param x \code{recurse} object returned from call to \code{\link{getRecursions}} or 
#' \code{\link{getRecursionsAtLocations}} with \code{verbose = TRUE}
#' @param breaks vector of POSIX datetimes describing the interval boundaries
#' @param labels (optional) vector or names for the intervals
#' 
#' @return A matrix of residence times where the columns are the coordinate indices of the locations (either 
#' movement trajectory locations or user-specified locations) and the rows are the time intervals.
#' 
#' @seealso \code{\link{getRecursions}}, \code{\link{getRecursionsAtLocations}}
#' 
#' @examples
#' data(martin)
#' revisits = getRecursions(martin, radius = 1)
#' breaks = strptime(c("2000-01-01 00:00:00", "2000-01-15 00:00:00", "2000-02-01 00:00:00"), 
#' format = "")
#' intervalResTime = calculateIntervalResidenceTime(revisits, breaks)
#' 
#' @author Chloe Bracis <cbracis@uw.edu>
#' @export
#' 
calculateIntervalResidenceTime = function (x, breaks, labels = NULL) 
{
	if (!inherits(x, "recurse") | !("revisitStats" %in% names(x)))
		stop("x must be a recuse object with revisitStats data frame")
	if (!(inherits(breaks, "POSIXct") | inherits(breaks, "POSIXt")))
		stop("breaks must be a vector of POSIX date times")
	if (length(breaks) < 2)
		stop("breaks must contain 2 or more datetimes")
	if (is.unsorted(breaks))
		stop("breaks must be in increasing order")
	
	nIntervals = length(breaks) - 1 
	timeunits = x$timeunits

	if (is.null(timeunits))
		stop("timeunits must be specified")
	if (is.null(labels))
	{
		labels = paste("Int", 1:nIntervals, sep = "_")
	} else
	{
		if (length(labels) != nIntervals)
			stop("There must be 1 fewer label than break")
	}
	
	x$revisitStats$coordFactor = as.factor(x$revisitStats$coordIdx) #otherwise some levels dropped in subset
	out = matrix(NA, nrow = length(x$revisits), ncol = nIntervals)
	
	for (i in 1:nIntervals)
	{
		statsSubset = x$revisitStats[x$revisitStats$entranceTime >= breaks[i] & x$revisitStats$exitTime <= breaks[i + 1],]
		rt = tapply(statsSubset$timeInside, statsSubset$coordFactor, sum)
		intervalRT = replace(rt, is.na(rt), 0)

		# check for partial overlaps
		
		# start of interval, section from breaks[i] to exit time
		statsSubset = x$revisitStats[x$revisitStats$entranceTime < breaks[i] & x$revisitStats$exitTime > breaks[i],]
		rt = tapply(difftime(statsSubset$exitTime, breaks[i], units = timeunits), 
										 statsSubset$coordFactor, sum)
		intervalRT = intervalRT + replace(rt, is.na(rt), 0)
		
		# end of interval, section from entrance time to breaks[i+1]
		statsSubset = x$revisitStats[x$revisitStats$entranceTime < breaks[i + 1] & x$revisitStats$exitTime > breaks[i + 1],]
		rt = tapply(difftime(breaks[i + 1], statsSubset$entranceTime, units = timeunits), 
										 statsSubset$coordFactor, sum)
		intervalRT = intervalRT + replace(rt, is.na(rt), 0)
		
		# entire interval
		statsSubset = x$revisitStats[x$revisitStats$entranceTime < breaks[i] & x$revisitStats$exitTime > breaks[i + 1],]
		intervalRT = intervalRT + table(statsSubset$coordFactor) * difftime(breaks[i + 1], breaks[i], units = timeunits)

		out[,i] = intervalRT
	}
	
	colnames(out) = labels
	rownames(out) = names(intervalRT)
	return(out)
}