#' @describeIn getRecursionsInPolygon Get recursions in polygon for a \link[move]{MoveStack} trajectory
#' @method getRecursionsInPolygon MoveStack
#' @export
getRecursionsInPolygon.MoveStack = function(trajectory, polygon, threshold = 0, timeunits = c("hours", "secs", "mins", "days"), 
								   verbose = TRUE)
{
	
	#check for move package
	if (!requireNamespace("move", quietly = TRUE)) 
	{
		stop("move package needed for this function to work. Please install it.",
			 call. = FALSE)
	}	
	
	xyt = data.frame(trajectory@coords, t = trajectory@timestamps, id = trajectory@trackId)
	
	return( getRecursionsInPolygon(xyt, polygon, threshold, timeunits, verbose) )
}
