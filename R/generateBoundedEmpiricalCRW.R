#' @title Generates a bounded correlated random walk from an empirical set of steps and angles
#' 
#' @description Generates a correlated random walk starting at (startX, startY) using pairs of steps and 
#' turning angles drawn from the provided empirical distributions. The random walk is bounded by a polygon by 
#' testing if each step will end outside the polygon and redrawing a new step if necessary.
#' 
#' @inheritParams generateEmpiricalCRW
#' @param boundry a \link[sp]{SpatialPolygons} describing the boundary of the walk
#' 
#' @return a data frame of the generated random walk with columns x and y describing the location 
#' and t for the time of each step
#' 
#' @export
#' 
generateBoundedEmpiricalCRW = function(n, startX, startY, steps, turnAngles, boundry)
{	
	if (!requireNamespace("prevR", quietly = TRUE)) 
	{
		stop("prevR package needed for this function to work. Please install it.",
			 call. = FALSE)
	}	
	stopifnot(length(steps) == length(turnAngles))
	nSteps = length(steps)
	
	X = vector("numeric", n)
	Y = vector("numeric", n)
	
	X[1] = startX
	Y[1] = startY
	Phi = 0
	
	for (i in 2:n)
	{
		tries = 0
		violateBoundary = FALSE
		
		repeat
		{
			# draw random index
			idx <- sample(1:nSteps, 1, replace = TRUE)
			
			# draw step and turning angle
			step <- steps[idx]
			theta <- turnAngles[idx]
			
			# cumulative angle (absolute orientation)
			Phi <- Phi + theta
			
			# step length components
			dX <- step*cos(Phi)
			dY <- step*sin(Phi)
			
			# actual X-Y values
			x <- X[i-1] + dX
			y <- Y[i-1] + dY
			
			if (prevR::point.in.SpatialPolygons(x, y, boundry))
			{
				# point is inside boundry 
				X[i] = x
				Y[i] = y
				break
			}
			if (tries > 50)
			{
				print(paste("next point outside boundary, i =", i,  "(", X[i-1], Y[i-1], ")"))
				violateBoundary = TRUE
				break
			}
			tries = tries + 1
		} 
	}
	
	results = if (violateBoundary) { NULL } else { data.frame(x = X, y = Y, t = 1:n) }
	return( results )
}