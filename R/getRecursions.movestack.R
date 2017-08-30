#' @describeIn getRecursions Get recursions for a MoveStack object
#' @method getRecursions MoveStack
#' @export
getRecursions.MoveStack = function(x, radius, threshold = 0, timeunits = c("hours", "secs", "mins", "days"), 
								   verbose = TRUE)
{
	
	#check for move package
	if (!requireNamespace("move", quietly = TRUE)) 
	{
		stop("move package needed for this function to work. Please install it.",
			 call. = FALSE)
	}	
	#if (isLonLat(x)) 
	#{
	#	stop("A longitude latitude projection is not recommended for this function. To transform your coordinates use the spTransform function.")
	#}
	
	xyt = data.frame(x@coords, t = x@timestamps, id = x@trackId)
	
	return( getRecursions(xyt, radius, threshold, timeunits, verbose) )
}
