#' @describeIn getRecursions Get recursions for a data.frame object consisting of columns x, y, datetime, and id
#' @method getRecursions data.frame
#' @export
getRecursions.data.frame = function(x, radius, threshold = 0, timeunits = c("hours", "secs", "mins", "days"), verbose = TRUE)
{
	# ideas for large data sets:
	# optionally specify locations
	# specify number of clusters and take median of each cluster
	
	
	stopifnot(is.data.frame(x))
	stopifnot(ncol(x) == 4)
	stopifnot(radius > 0)
	timeunits = match.arg(timeunits)
	
	results = getRecursionsCpp(x[,1], x[,2], x[,3], x[,4], x[,1], x[,2], 
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

