
test_that("flip_endian() works", {

  
  vec <- c(1, 2, 3, 4, 5, 6, 7, 8)
  expect_identical(flip_endian(vec, 1), vec)
  expect_identical(flip_endian(vec, 2), c(2, 1, 4, 3, 6, 5, 8, 7))
  expect_identical(flip_endian(vec, 4), c(4, 3, 2, 1, 8, 7, 6, 5))
  
  expect_error(
    flip_endian(vec, 3)
  )
  
})
