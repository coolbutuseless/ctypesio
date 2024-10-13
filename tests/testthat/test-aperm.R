

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


test_that("aperm_array_to_vector works with a matrix", {
  
  mat <- matrix(1:6, 2, 3)
  vec <- aperm_array_to_vector(mat, dst = c('planes', 'cols', 'rows'), flipy = TRUE)
  vec
  expect_equal(
    vec, 
    c(2, 4, 6, 1, 3, 5)
  )
  
  mat2 <- aperm_vector_to_array(vec, src = c(planes = 1, cols = 3, rows = 2), flipy = TRUE)
  expect_identical(
    mat2, 
    mat
  )
  
})



test_that("aperm combinational check", {
  
  
  nrows   <- 2
  ncols   <- 3
  nplanes <- 4
  
  arr <- array(c(paste0('r', 1:6), 
                 paste0('g', 1:6), 
                 paste0('b', 1:6),
                 paste0('a', 1:6)), c(nrows, ncols, nplanes))
  arr
  
  
  # Convert to package RGB in column-major format
  (v <- aperm_array_to_vector(arr, dst = c('planes', 'cols', 'rows')))
  res <- aperm_vector_to_array(v, src = c(planes = 4, cols = 3, rows = 2))
  expect_identical(res, arr)
  
  (v <- aperm_array_to_vector(arr, dst = c('planes', 'rows', 'cols')))
  res <- aperm_vector_to_array(v, src = c(planes = 4, rows = 2, cols = 3))
  expect_identical(res, arr)
  
  (v <- aperm_array_to_vector(arr, dst = c('cols', 'planes', 'rows')))
  res <- aperm_vector_to_array(v, src = c(cols = 3, planes = 4, rows = 2))
  expect_identical(res, arr)
  
  (v <- aperm_array_to_vector(arr, dst = c('cols', 'rows', 'planes')))
  res <- aperm_vector_to_array(v, src = c(cols = 3, rows = 2, planes = 4))
  expect_identical(res, arr)
  
  (v <- aperm_array_to_vector(arr, dst = c('rows', 'cols', 'planes')))
  res <- aperm_vector_to_array(v, src = c(rows = 2, cols = 3, planes = 4))
  expect_identical(res, arr)
  
  (v <- aperm_array_to_vector(arr, dst = c('rows', 'planes', 'cols')))
  res <- aperm_vector_to_array(v, src = c(rows = 2, planes = 4, cols = 3))
  expect_identical(res, arr)
  
  
})
