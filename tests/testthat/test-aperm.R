

test_that("aperm() works", {
  
  
  arr <- array(rep(c('r', 'g', 'b'), each = 6), c(2, 3, 3))

  linear <- aperm_array_to_vector(arr, dst = c('planes', 'cols', 'rows'))
  
  expect_equal(
    linear,
    c("r", "g", "b", "r", "g", "b", "r", "g", "b", "r", "g", "b", "r", "g", "b", "r", "g", "b")
  )
  
  
  arr2 <- aperm_vector_to_array(linear, src = c(planes = 3, cols = 3, rows = 2))
  arr2  
  expect_identical(arr2, arr)
    
})
