
test_that("raw roundtrips work", {
  
  input <- as.raw(1:255)
  N     <- length(input)
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Write data
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  con <- rawConnection(raw(), open = "w")
  write_raw(con, input)  
  dat <- rawConnectionValue(con)  
  close(con)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read it back
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  con <- rawConnection(dat, open = 'r')
  output <- read_raw(con, N)
  close(con)
  
  expect_equal(input, output, label = paste("roundtrip mismatch: ", "'raw'"))
  
  
})


