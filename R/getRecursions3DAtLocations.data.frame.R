#' @describeIn getRecursions3DAtLocations Get recursions at specified locations for a data.frame object
#' @method getRecursions3DAtLocations data.frame
#' @export
getRecursions3DAtLocations.data.frame = function(x, locations, radius, threshold = 0, timeunits = c("hours", "secs", "mins", "days"), verbose = TRUE)
{
	stopifnot(is.data.frame(x))
	stopifnot(ncol(x) == 5)
	stopifnot(is.data.frame(locations))
	stopifnot(ncol(locations) == 3)
	stopifnot(radius > 0)
	timeunits = match.arg(timeunits)
	
	results = getRecursions3DCpp(x[,1], x[,2], x[,3], x[,4], x[,5], locations[,1], locations[,2], locations[,3],
								 radius, threshold, timeunits, verbose)
	results$timeunits = timeunits
	
	class(results) = "recurse3D"
	
	if (verbose)
	{
		warning("Verbose mode not fully implemented, time calculations not correct")
		class(results) = c("recurse3D", "recurse.verbose")

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
