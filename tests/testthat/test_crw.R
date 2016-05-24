library(recurse)

context("recurse")

# just runs code, TODO verify metrics
test_that("generate CRW",
{
	expect_equal(nrow(generateCRW(n = 100)), 100)
})