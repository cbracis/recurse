generateCRW = function(n, weibull_shape = 2, weibull_scale = 1 , cauchy_mu = 0, cauchy_rho = 0.8)
{
	require(circular)
	
	# make weibull distributed steps
	steps <- rweibull(n, weibull_shape, weibull_scale)
	
	# make clustered turning angles
	theta <- rwrappedcauchy(n, cauchy_mu, cauchy_rho)
	
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