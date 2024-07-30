test_that("direction to labels with numeric works", {
  codes <- c(0, 1, 2, 3)
  expect_equal(directioncheck(class(codes), codes), "labels")
})

test_that("direction to labels with numbers as text works", {
  codes <- c("01", "02", "03")
  expect_equal(directioncheck(class(codes), codes), "labels")
})

test_that("direction to labels with section codes works", {
  codes <- c("A", "B", "C")
  expect_equal(directioncheck(class(codes), codes), "labels")
})

test_that("direction to values with texts works", {
  codes <- c("This", "could", "be", "a", "label")
  expect_equal(directioncheck(class(codes), codes), "values")
})

test_that("direction to values with full caps texts works", {
  codes <- c("THIS", "COULD", "BE", "A", "SECTION", "LABEL")
  expect_equal(directioncheck(class(codes), codes), "values")
})
