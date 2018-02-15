#' @describeIn getRecursionsInPolygon Get recursions inside a polygon for a trajectory data.frame object consisting of columns x, y, datetime, and id
#' @method getRecursionsInPolygon data.frame
#' @export
getRecursionsInPolygon.data.frame = function(trajectory, polygon, threshold = 0, timeunits = c("hours", "secs", "mins", "days"), verbose = TRUE)
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