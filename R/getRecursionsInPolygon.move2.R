#' @describeIn getRecursionsInPolygon Get recursions in polygon for a move2 object (for details see \code{vignette("programming_move2_object", package = "move2")})
#' @method getRecursionsInPolygon move2
#' @export
getRecursionsInPolygon.move2 = function(trajectory, polygon, threshold = 0, timeunits = c("hours", "secs", "mins", "days"), 
							  verbose = TRUE)
{
	
  #check for move2 package
  if (!requireNamespace("move2", quietly = TRUE)) 
  {
    stop("move2 package needed for this function to work. Please install it.",
         call. = FALSE)
  }	
  
  stopifnot(move2::mt_is_time_ordered(trajectory))
  stopifnot(move2::mt_is_track_id_cleaved(trajectory))
  
  xyt = data.frame(
    x = sf::st_coordinates(trajectory)[,1],
    y = sf::st_coordinates(trajectory)[,2],
    t = move2::mt_time(trajectory),
    id = move2::mt_track_id(trajectory)
  )
  
	return( getRecursionsInPolygon(xyt, polygon, threshold, timeunits, verbose) )
}

