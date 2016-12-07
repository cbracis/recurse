.getRecursions = function(trajectory, locations, radius, threshold = 0, timeunits = c("hours", "secs", "mins", "days"), verbose = TRUE)
{
	stopifnot(is.data.frame(trajectory))
	stopifnot(ncol(trajectory) == 4)
	stopifnot(radius > 0)
	timeunits = match.arg(timeunits)
	
	# data used in function
	nTraj = nrow(trajectory) # number of trajectory locations
	nLoc = nrow(locations) # number of locations to examine for revisits
	z = complex(real = trajectory[,1], imaginary = trajectory[,2])
	loc = complex(real = locations[,1], imaginary = locations[,2])
	t = trajectory[,3]
	idIdx = 4
	isNewTrack = as.logical(c(1, diff(as.numeric(trajectory[,idIdx]))))
	
	# internal functions
	
	# distance betwen 2 points as complex numbers
	getDists = function(a, b)		
	{
		return(Mod(a - b))
	}	
	
	# this is actually quite fast, other than using dist() and then have
	# triangular dist obj rather than symmetric matrix
	distMatrix = outer(z, loc, getDists)
	
	revisits = rep(0, nLoc) # start with 0 revisit per location
	fpt = vector("numeric", nLoc)
	rt = vector("numeric", nLoc)
	
	allRevisitStats = vector(mode="list", nLoc)
	
	# index of some statistics columns since these aren't named
	statsColNames = c("id", "x", "y", "coordIdx", "visitIdx", 
					  "entranceTime", "exitTime", 
					  "timeInside", "timeSinceLastVisit")
	exitTimeIdx = which(statsColNames == "exitTime")
	timeInsideIdx = which(statsColNames == "timeInside")
	
	# for each location, calculate
	for (i in 1:nLoc)
	{
		revisitStats = NULL
		
		# find relocations within radius (use column for loc i)
		inRadius = (distMatrix[,i] <= radius)
		
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
							revisits[i] = revisits[i] + 1
							stats = data.frame(trajectory[i, idIdx], Re(z[i]), Im(z[i]), i, revisits[i], radiusEntranceTime, radiusExitTime, 
											   timeInside, timeSinceLastVisit)
							names(stats) = statsColNames
							revisitStats = rbind(revisitStats,  stats)	
						}
					}
				} # end if new track
				
				# reset varaibles for new trajectory
				stillInside = ifelse(inRadius[j], TRUE, FALSE) # start with animal inside radius?
				appendToPreviousRevisit = FALSE
				radiusEntranceTime = if (stillInside) { t[j] } else { NA } # avoid ifelse which converts posix to numeric
				radiusExitTime = NA
				timeSinceLastVisit = NA
			}
			else
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
							stats = data.frame(trajectory[i,idIdx], Re(z[i]), Im(z[i]), i, revisits[i], radiusEntranceTime, radiusExitTime, 
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
				stats = data.frame(trajectory[i, idIdx], Re(z[i]), Im(z[i]), i, revisits[i], radiusEntranceTime, radiusExitTime, 
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