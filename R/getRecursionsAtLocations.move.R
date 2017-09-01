#' @describeIn getRecursionsAtLocations Get recursions at specified locations for a \link[move]{Move-class} object
#' @method getRecursionsAtLocations Move
#' @export
getRecursionsAtLocations.Move = function(x, locations, radius, threshold = 0, timeunits = c("hours", "secs", "mins", "days"), verbose = TRUE)
{
	#check for move package
	if (!requireNamespace("move", quietly = TRUE)) 
	{
		stop("move package needed for this function to work. Please install it.",
			 call. = FALSE)
	}	
	
	id = move::idData(x)
	
	if (length(id) > 1)
	{
		id = id$individual.local.identifier
	}
	
	# problem with id or individual.local.identifier doesn't exist, only have 1 individual
	if (length(id) == 0) 
	{
		id = 1
	}
	
	xyt = data.frame(x@coords, t = x@timestamps, id = id)
	
	return( getRecursionsAtLocations(xyt, locations, radius, threshold, timeunits, verbose) )
}

