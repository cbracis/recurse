#' @describeIn getRecursions Get recursions for a Move object
#' @method getRecursions Move
#' @export
getRecursions.Move = function(x, radius, threshold = 0, timeunits = c("hours", "secs", "mins", "days"), 
							  verbose = TRUE)
{
	
	#check for move package
	if (!requireNamespace("move", quietly = TRUE)) 
	{
		stop("move package needed for this function to work. Please install it.",
			 call. = FALSE)
	}	
	if (isLonLat(x)) 
	{
		stop("A longitude latitude projection is not recommended for this function. To transform your coordinates use the spTransform function.")
	}
	
	id = x@data$individual.local.identifier
	
	if (length(id) == 0) # can be for some move objects (ie split from movestack?)
	{
		id = habiba@idData$individual.local.identifier
	}

	xyt = data.frame(x@coords, t = x@timestamps, id = id)
	
	return( getRecursions(xyt, radius, threshold, timeunits, verbose) )
}

