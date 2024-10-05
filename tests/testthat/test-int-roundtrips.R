

writers <- list(
  uint8  = write_uint8,
  uint16 = write_uint16,
  uint32 = write_uint32,
  uint64 = write_uint64,
  
  int8   = write_int8,
  int16  = write_int16,
  int32  = write_int32,
  int64  = write_int64
)

readers <- list(
  uint8  = read_uint8,
  uint16 = read_uint16,
  uint32 = read_uint32,
  uint64 = read_uint64,
  
  int8   = read_int8,
  int16  = read_int16,
  int32  = read_int32,
  int64  = read_int64
)

types <- names(writers)


test_that("integer roundtrips work", {
  
  type  <- types[1]
  input <- 1:127
  N     <- length(input)
  
  for (type in types) {
    writer <- writers[[type]]
    reader <- readers[[type]]
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Write data
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    con <- rawConnection(raw(), open = "w")
    writer(con, input)  
    dat <- rawConnectionValue(con)  
    close(con)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Read it back
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    con <- rawConnection(dat, open = 'r')
    output <- reader(con, N)
    close(con)
    
    expect_equal(input, output, label = paste("roundtrip mismatch: ", type))
  }
})




if (FALSE) {
  
  input <- 1:15
  N <- length(input)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Write data
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  con <- rawConnection(raw(), open = "w")
  write_uint64(con, input)  
  dat <- rawConnectionValue(con)  
  close(con)
  dat
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read it back
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  con <- rawConnection(dat, open = 'r')
  output <- read_uint64(con, N)
  close(con)
  
  input
  output
  
}