#' @describeIn getRecursionsInPolygon Get recursions in polygon for a \link[move]{Move-class} trajectory
#' @method getRecursionsInPolygon Move
#' @export
getRecursionsInPolygon.Move = function(trajectory, polygon, threshold = 0, timeunits = c("hours", "secs", "mins", "days"), 
							  verbose = TRUE)
{
	
	#check for move package
	if (!requireNamespace("move", quietly = TRUE)) 
	{
		stop("move package needed for this function to work. Please install it.",
			 call. = FALSE)
	}	
	
	id = move::idData(trajectory)
	
	if (length(id) > 1)
	{
		id = id$individual.local.identifier
	}
	
	# problem with id or individual.local.identifier doesn't exist, only have 1 individual
	if (length(id) == 0) 
	{
		id = 1
	}
	
	
	xyt = data.frame(trajectory@coords, t = trajectory@timestamps, id = id)
	
	return( getRecursionsInPolygon(xyt, polygon, threshold, timeunits, verbose) )
}

