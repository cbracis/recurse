#' @describeIn getRecursionsAtLocations Get recursions at specified locations for a data.frame object
#' @method getRecursionsAtLocations data.frame
#' @export
getRecursionsAtLocations.data.frame = function(x, locations, radius, threshold = 0, timeunits = c("hours", "secs", "mins", "days"), verbose = TRUE)
{
	stopifnot(is.data.frame(x))
	stopifnot(ncol(x) == 4)
	stopifnot(is.data.frame(locations))
	stopifnot(ncol(locations) == 2)
	stopifnot(radius > 0)
	timeunits = match.arg(timeunits)
	
	results = getRecursionsCpp(x[,1], x[,2], x[,3], x[,4], locations[,1], locations[,2], 
							   radius, threshold, timeunits, verbose)
	results$timeunits = timeunits
	
	class(results) = "recurse"
	
	if (verbose)
	{
		class(results) = c("recurse", "recurse.verbose")
		
		dataTz = attr(x[,3], "tzone")
		if (!is.null(dataTz))
		{
			# set timezone becasue Rcpp doesn't do it correctly for Datetime
			# https://stackoverflow.com/questions/42919588/setting-datetime-timezone-in-rcpp
			attr(results$revisitStats$entranceTime, "tzone") = dataTz
			attr(results$revisitStats$exitTime, "tzone") = dataTz
		}
	}
	
	return(results)
	
}
