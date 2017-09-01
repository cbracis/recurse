#' @describeIn getRecursions Get recursions for a \link[move]{MoveStack} object
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

	xyt = data.frame(x@coords, t = x@timestamps, id = x@trackId)
	
	return( getRecursions(xyt, radius, threshold, timeunits, verbose) )
}
