library(recurse)

context("recurse")

# support functions
#------------------------------------------------------------------------------------------------

getRevisits = function(data, radius)
{
	return(getRecursions(data, radius)$revisits)
}

createTimeVector = function(n, tz = "")
{
	return(as.POSIXct("2009-5-1 1:00:00", tz = tz) + (1:n) * 60 * 60) # every hour
}

createMoveObj = function(df)
{
	if (requireNamespace("move", quietly = TRUE)) 
	{
		# remove projection in order to remove sp dependency (was proj = sp::CRS("+proj=aeqd"))
		moveObj = move::move(x = df$x, y = df$y, time = df$t, animal = df$id) 
		move::idData(moveObj) = df$id[1] # move ignores id, so set it directly
	}
	else
	{
		moveObj = NULL
	}
	return(moveObj)
}

testTz = function(df)
{
	output = getRecursions(df, 1)
	expect_equal(attr(output$revisitStats$entranceTime, "tzone"), attr(df$t, "tzone"))
	expect_equal(attr(output$revisitStats$exitTime, "tzone"), attr(df$t, "tzone"))
}

# data
#------------------------------------------------------------------------------------------------

# expected revisits for data "track" included in pacakge and calculated using an alternate method
# as.double(.Call("nvisits", df = track, radius = 1, maxt = 0, PACKAGE = "adehabitatHR"))
expectedRevisistsRadius1 = c(2, 2, 3, 2, 2, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 2, 2, 2, 3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 1, 1, 1, 2, 2, 2, 2, 1, 1, 2)

# points on a line so distances between are unit lengths
simplePts = data.frame(x = 1, y = 1:5, t = createTimeVector(5), id = "A1")
simplePtsDist = matrix(0, nrow = 5, ncol = 5)
simplePtsDist =  abs(row(simplePtsDist) - col(simplePtsDist))

# different locations than simplePts, but still x = 1 so with easy distances
simpleLocs = data.frame(x = c(1, 1, 1), y = c(1, 0, 10))
simpleLocsDist = matrix(0, nrow = 5, ncol = 3)
simpleLocsDist = cbind(0:4, 1:5, 9:5)

# two tracks, one vertical x = 1, one horizontal y = 3
horzTrack = data.frame(x = -1:3, y = 3, t = createTimeVector(5), id = "B2")
twoTracks = rbind(simplePts, horzTrack)

# one track that revisits one location
oneTrack = twoTracks
oneTrack$t[5:10] = oneTrack$t[5:10] + 1 * 24 * 60 * 60 # make second part of track a day later
oneTrack$id = 1

# track with data points on radius boundries so time in/out is easy to caluculate
gridTrack = data.frame(
	x = c(0, 1, 2, 1, 0, 1, 2, 2, 3, 5, 6, 3, 1, 3, 4),
	y = rep(0, 15),
	t = createTimeVector(15),
	id = rep(1, 15)
)
#plot(gridTrack$x, gridTrack$t/10, type = "b")

# tests
#------------------------------------------------------------------------------------------------

test_that("correct number of revisits",
		  {
		  	expect_equal(length(getRevisits(track, 1)), 100)
		  	
		  	expect_equal(getRevisits(track, 1), expectedRevisistsRadius1)
		  	
		  	expect_equal(getRevisits(twoTracks, 0.5), rep(c(1, 1, 2, 1, 1), 2)) # middle pt overlaps
		  })

test_that("distance matrix",
		  {
		  	expect_equal(getRecursions(simplePts, 1, verbose = TRUE)$dists, simplePtsDist)
		  	expect_equal(getRecursionsAtLocations(simplePts, simpleLocs, 1, verbose = TRUE)$dists, simpleLocsDist)
		  })

test_that("time in radius",
		  {
		  	# examine visits to point at x=2 (use first time at t=3)
		  	stats = getRecursions(gridTrack, 1)$revisitStats
		  	expect_equal(as.numeric(stats[stats$coordIdx==3, "timeInside"]),
		  				 c(2, 3, 2))
		  	expect_equal(as.numeric(stats[stats$coordIdx==3, "timeSinceLastVisit"]),
		  				 c(NA, 2, 3))
		  	})

test_that("verbose",
		  {
		  	verbose = getRecursions(simplePts, 1, verbose = TRUE)
		  	nonVerbose = getRecursions(simplePts, 1, verbose = FALSE)
		  
		  })

test_that("revisit stats",
		  {
		  	stats = getRecursions(simplePts, 0.5)$revisitStats
		  	n = nrow(simplePts)
		  	
		  	expect_equal(stats$id, simplePts$id)
		  	expect_equal(stats$x, simplePts$x)
		  	expect_equal(stats$y, simplePts$y)
		  	expect_equal(stats$coordIdx, 1:n)
		  	expect_equal(stats$visitIdx, rep(1, n))
		  	expect_equal(stats$entranceTime, c(simplePts$t[1], simplePts$t[2:n] - as.difftime(30, units = "mins")))
		  	expect_equal(stats$exitTime, c(simplePts$t[1:(n-1)] + as.difftime(30, units = "mins"), simplePts$t[n]))
		  	expect_equal(stats$timeInside, c(0.5, 1, 1, 1, 0.5))
		  	expect_equal(stats$timeSinceLastVisit, c(1, rep(NA, n))[-1])
		  	
		  	
		  	stats2 = getRecursions(twoTracks, 0.5)$revisitStats
		  	expectedCoordIdx = c(1, 2, 3, 3, 4, 5, 6, 7, 8, 8, 9, 10)
		  	expectedIdIdx = c(1, 1, 1, 2, 1, 1, 2, 2, 1, 2, 2, 2)
		  	
		  	expect_equal(stats2$id, unique(twoTracks$id)[expectedIdIdx]) #unique orders A1 B2
		  	expect_equal(stats2$x, twoTracks$x[expectedCoordIdx])
		  	expect_equal(stats2$y, twoTracks$y[expectedCoordIdx])
		  	expect_equal(stats2$coordIdx, expectedCoordIdx)
		  	expect_equal(stats2$visitIdx, rep(c(1, 1, 1, 2, 1, 1), 2))
		  	expect_equal(stats2$entranceTime, twoTracks$t[expectedCoordIdx] - as.difftime(rep(c(0, rep(30, 5)), 2), units = "mins"))
		  	expect_equal(stats2$exitTime, twoTracks$t[expectedCoordIdx] + as.difftime(rep(c(rep(30, 5), 0), 2), units = "mins"))
		  	expect_equal(stats2$timeInside, rep(c(0.5, 1, 1, 1, 1, 0.5), 2))
		  	expect_equal(stats2$timeSinceLastVisit, c(1, rep(NA, length(expectedCoordIdx)))[-1]) 
		  	
		  })

test_that("threshold",
		  {
		  	expect_equal(sum(getRecursions(oneTrack, 0.5, threshold = 0)$revisits), 12)
		  	expect_equal(sum(getRecursions(oneTrack, 0.5, threshold = 1)$revisits), 12)
		  	expect_equal(sum(getRecursions(oneTrack, 0.5, threshold = 23)$revisits), 12)
		  	expect_equal(sum(getRecursions(oneTrack, 0.5, threshold = 24)$revisits), 10)
		  	expect_equal(sum(getRecursions(oneTrack, 0.5, threshold = 100)$revisits), 10)
		  })

test_that("move objects",
		  {
		  	if (requireNamespace("move", quietly = TRUE)) 
		  	{
			  	movePts = createMoveObj(simplePts)
			  	expect_equal( getRecursions(movePts, 1), getRecursions(simplePts,1) )
			  	
			  	moveStackPts = createMoveObj(twoTracks)
			  	expect_equal( getRecursions(moveStackPts, 1), getRecursions(twoTracks,1) )
		  	}
		  })

test_that("timezone",
		  {
		  	defaultTz = data.frame(x = 1, y = 1:5, t = createTimeVector(5), id = "default")
		  	utcTz = data.frame(x = 1, y = 1:5, t = createTimeVector(5, tz = "UTC"), id = "UTC")
		  	sydneyTz = data.frame(x = 1, y = 1:5, t = createTimeVector(5, tz = "Australia/Sydney"), id = "Australia/Sydney")
		  	limaTz = data.frame(x = 1, y = 1:5, t = createTimeVector(5, tz = "America/Lima"), id = "America/Lima")

		  	testTz(defaultTz)
		  	testTz(utcTz)
		  	testTz(sydneyTz)
		  	testTz(limaTz)
		  })

test_that("interval res time",
		  {
		  	vis = getRecursions(simplePts, 0.5)
		  	
		  	expect_equal(as.vector(calculateIntervalResidenceTime(vis, breaks = simplePts$t[c(1,5)])),
		  				 vis$residenceTime)
		  	
		  	expectedMatrix = matrix(c(0.5, 1, 0.5, 0, 0, 0, 0, 0.5, 1, 0.5), ncol = 2, 
		  							dimnames = list(1:5, c("A", "B")))
		  	expect_equal(calculateIntervalResidenceTime(vis, breaks = simplePts$t[c(1,3,5)], labels = c("A", "B")),
		  				 expectedMatrix)
		  	
		  })

test_that("polygon",
		  {
		  	require(sf)
		  	poly = sf::st_polygon(list(cbind(c(4,6,6,3,4), c(1,2,4,3,1))))
		  	polyc = sf::st_sfc(poly, crs = "EPSG:4326")
		  	recursions = getRecursionsInPolygon(track, polyc)
		  	expect_equal(recursions$revisits, 2)
		  	expect_equal(round(as.numeric(recursions$revisitStats$timeInside[1]), digits = 2), 44.99)
		  	expect_equal(round(as.numeric(recursions$revisitStats$timeInside[2]), digits = 2), 108.9)
		  })

