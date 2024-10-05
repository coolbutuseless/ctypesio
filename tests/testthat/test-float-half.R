



test_that("reading half-precision little endian works", {
  
  # Equivalent to 0x0201, 0x0403  (little endian bytes)
  # https://float.exposed/0x0201
  # https://float.exposed/0x0403
  raw_vec <- as.raw(c(0x01, 0x02, 0x03, 0x04))
  
  expect <- c(0.000030577182769775390625, 0.000061213970184326171875)
  
  expect_identical(
    chalf_to_rdbl(raw_vec), expect
  )
  
  
  # https://float.exposed/0x4ad8
  expect_identical(
    chalf_to_rdbl(as.raw(c(0xd8, 0x4a))), # 0x4ad8
    13.6875
  )  
  
  # https://float.exposed/0x0000
  expect_identical(
    chalf_to_rdbl(as.raw(c(0x00, 0x00))), # 0x4ad8
    0
  )

  # denormalised  
  # https://float.exposed/0x0271
  expect_identical(
    chalf_to_rdbl(as.raw(c(0x71, 0x02))), # 0x4ad8
    0.000037252902984619140625
  )
})



test_that("writing half-precision little endian works", {
  
  # Equivalent to 0x0201, 0x0403  (little endian bytes)
  # https://float.exposed/0x0201
  # https://float.exposed/0x0403
  raw_vec <- as.raw(c(0x01, 0x02, 0x03, 0x04))
  float   <- c(0.000030577182769775390625, 0.000061213970184326171875)
  
  expect_identical(
    rdbl_to_chalf(float),
    raw_vec
  )
  
  
  # https://float.exposed/0x4ad8
  expect_identical(
    rdbl_to_chalf(13.6875), 
    as.raw(c(0xd8, 0x4a))
  )  
  
  # https://float.exposed/0x0000
  expect_identical(
    rdbl_to_chalf(0), # 0x4ad8
    as.raw(c(0x00, 0x00))
  )
  
  # denormalised  
  # https://float.exposed/0x0271
  expect_identical(
    rdbl_to_chalf(0.000037252902984619140625), 
    as.raw(c(0x71, 0x02))
  )
})


test_that("f16 round trip", {
  
  input <- seq(-1000, 1000, length.out = 2001)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Write data
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  con <- rawConnection(raw(), open = "w")
  write_f16(con, input)  
  dat <- rawConnectionValue(con)  
  close(con)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read it back
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  con <- rawConnection(dat, open = 'r')
  output <- read_f16(con, length(input))
  close(con)
  
  expect_equal(input, output, label = paste("roundtrip mismatch: ", "f16"))

})


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# f16 has less precision than double, so there will be some precision issues
# for most floating point values
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
test_that("f16 round trip with aliasing", {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Remove numbers below the denormalised limit. 
  # especially those that would be truncated to zero in F16 format
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  input <- seq(-65504, 65504, length.out = 2001)
  length(input)
  input <- input[abs(input) > (2^(-13))]
  length(input)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Write data
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  con <- rawConnection(raw(), open = "w")
  write_f16(con, input)  
  dat <- rawConnectionValue(con)  
  close(con)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read it back
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  con <- rawConnection(dat, open = 'r')
  output <- read_f16(con, length(input))
  close(con)
  
  relative_error <- abs( (output - input) / input )
  
  expect_true(max(relative_error) < 1e-3)
  
})


















