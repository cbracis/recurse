library(recurse)

context("recurse")

distance = function(x1, y1, x2, y2) {sqrt((x1-x2)^2 + (y1-y2)^2)}

randomComparisons = function(n, mean, sd)
{
	for (i in 1:n)
	{
		Cx = rnorm(1, mean, sd)
		Cy = rnorm(1, mean, sd)
		Ax = Cx + rnorm(1, mean, sd)
		Ay = Cy + rnorm(1, mean, sd)
		Bx = Cx + rnorm(1, mean, sd)
		By = Cy + rnorm(1, mean, sd)
		LAC = distance(Cx, Cy, Ax, Ay)
		LBC = distance(Cx, Cy, Bx, By)
		r = runif(1, min(LAC, LBC), max(LAC, LBC))
		
		# determine if A or B is closer to circle center C
		if (LAC < LBC)
		{
			method1 = calculateCrossingPercentage(Cx, Cy, Ax, Ay, Bx, By, r)
			method2 = calculateCrossingPercentage2(Cx, Cy, Ax, Ay, Bx, By, r)
		} else
		{
			method1 = calculateCrossingPercentage(Cx, Cy, Bx, By, Ax, Ay, r)
			method2 = calculateCrossingPercentage2(Cx, Cy, Bx, By, Ax, Ay, r)
		}
		expect_equal(method1, method2, info = cat("C=(", Cx, "'", Cy, "), pts ", 
												  "(", Ax, "'", Ay, ") (", Bx, "'", By, "), r=", r))
	}
}

test_that("compare crossing to alternate implementation",
		 {
		 	randomComparisons(100, 0, 10)
		 })

test_that("point tangent",
		  {
		  	expect_equal(calculateCrossingPercentage(1, 1, 1, 2, 2, 3, 1), 0)
		  	expect_equal(calculateCrossingPercentage(1, 1, 0.5, 0.5, 1, 2, 1), 1)
		  })
