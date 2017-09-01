#' @describeIn getRecursionsAtLocations Get recursions at specified locations for a \link[move]{MoveStack} object
#' @method getRecursionsAtLocations MoveStack
#' @export
getRecursionsAtLocations.MoveStack = function(x, locations, radius, threshold = 0, timeunits = c("hours", "secs", "mins", "days"), verbose = TRUE)
{
	#check for move package
	if (!requireNamespace("move", quietly = TRUE)) 
	{
		stop("move package needed for this function to work. Please install it.",
			 call. = FALSE)
	}	
	
	xyt = data.frame(x@coords, t = x@timestamps, id = x@trackId)
	
	return( getRecursionsAtLocations(xyt, locations, radius, threshold, timeunits, verbose) )
}
