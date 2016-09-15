#' @title Calculates percentage of trajectory segment within circle
#'
#' @description Calculates the percentage of a segment that lies within a circle for a point
#' A inside the circle and point B outside the circle for a circle with center C and radius R.
#' 
.calculateCrossingPercentageCmplx = function(Cz, Az, Bz, R)
{
	return(.calculateCrossingPercentage(Re(Cz), Im(Cz), Re(Az), Im(Az), Re(Bz), Im(Bz), R))
}

.calculateCrossingPercentage = function(Cx, Cy, Ax, Ay, Bx, By, R)
{
	# algorithm based on 
	# http://stackoverflow.com/questions/1073336/circle-line-segment-collision-detection-algorithm
	# note that the origianl code description talk about 0 <= t <= 1, but though the
	# Dx and Dy vectors are directionless, the line equations go from A at t = 0 to B at t = LAB
	
	
	# compute the euclidean distance between A and B
	LAB = sqrt( (Bx-Ax)^2 + (By-Ay)^2 )
	
	# compute the direction vector D from A to B
	Dx = (Bx-Ax) / LAB
	Dy = (By-Ay) / LAB
	
	# Now the line equation is x = Dx*t + Ax, y = Dy*t + Ay with 0 <= t <= LAB.
	
	# compute the value t of the closest point to the circle center (Cx, Cy)
	t = Dx*(Cx-Ax) + Dy*(Cy-Ay) 
	
	# This is the projection of C on the line from A to B.
	
	# compute the coordinates of the point E on line and closest to C
	Ex = t*Dx+Ax
	Ey = t*Dy+Ay
	
	# compute the euclidean distance from E to C
	LEC = sqrt( (Ex-Cx)^2 + (Ey-Cy)^2 )
	
	# test if the line intersects the circle
	if( LEC < R )
	{
		# compute distance from t to circle intersection point
		dt = sqrt( R^2 - LEC^2)
		
		# compute first intersection point
		#Fx = (t-dt)*Dx + Ax
		#Fy = (t-dt)*Dy + Ay
		
		# compute second intersection point
		Gx = (t+dt)*Dx + Ax
		Gy = (t+dt)*Dy + Ay
				
		p = (t + dt) / LAB
	}
	
	# else test if the line is tangent to circle
	else if( LEC == R )
	{
		# tangent point to circle is E
		if (Ex == Ax) # point E is A
		{
			p = 0
		} else # point E is B
		{
			p = 1
		}
	}
	else
	{
		stop("line doesn't touch circle")
	}
	
#	return(list(LAB = LAB, t = t, p = p, Gx = Gx, Gy = Gy))
	return(p)
}