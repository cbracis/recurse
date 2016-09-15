#' @title Generates a correlated random walk 
#' 
#' @description Generates a correlated random walk starting at (0,0) using a Weibull distribution for step
#' length and a wrapped Cauchy distribution for turning angles.
#' 
#' @param n number of steps to generate
#' @param weibull_shape shape parameter of the Weibull distribution
#' @param weibull_scale scale parameter of the Weibull distribution
#' @param cauchy_mu mu parameter of the wrapped Cauchy distribution
#' @param cauchy_rho rho parameter of the wrapped Cauchy distribution
#' 
#' @return a data frame of the generated random walk with columns x and y describing the location 
#' and t for the time of each step
#' 
#' @seealso \code{\link{rweibull}}, \code{\link[circular]{rwrappedcauchy}}
#' 
#' @export
#' 
generateCRW = function(n, weibull_shape = 2, weibull_scale = 1, cauchy_mu = 0, cauchy_rho = 0.8)
{
	if (!requireNamespace("circular", quietly = TRUE)) 
	{
		stop("circular package needed for this function to work. Please install it.",
			 call. = FALSE)
	}	
	
	# make weibull distributed steps
	steps <- rweibull(n, weibull_shape, weibull_scale)
	
	# make clustered turning angles
	theta <- circular::rwrappedcauchy(n, cauchy_mu, cauchy_rho)
	
	# cumulative angle (absolute orientation)
	Phi <- cumsum(theta)
	
	# step length components
	dX <- steps*cos(Phi)
	dY <- steps*sin(Phi)
	
	# actual X-Y values
	X<-cumsum(dX)
	Y<-cumsum(dY)
	
	return( data.frame(x = X, y = Y, t = 1:n))
}