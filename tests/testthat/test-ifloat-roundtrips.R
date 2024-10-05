

writers <- list(
  f16 = write_f16,
  f32 = write_f32,
  f64 = write_f64
)

readers <- list(
  f16 = read_f16,
  f32 = read_f32,
  f64 = read_f64
)

types <- names(writers)


test_that("float roundtrips work", {
  
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
  con <- rawConnection(raw(), open = "w") |> set_eof_check('ignore')
  write_f32(con, input)  
  dat <- rawConnectionValue(con)  
  close(con)
  dat
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read it back
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  con <- rawConnection(dat, open = 'r')
  output <- read_f32(con, N)
  output <- read_f32(con, N)
  close(con)
  
  input
  output
  
}


if (FALSE) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Write data
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  con <- rawConnection(raw(), open = "w")
  write_utf8_raw(con, "123.456 a 999.11")  
  dat <- rawConnectionValue(con)  
  close(con)
  dat
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read it back
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  con <- rawConnection(dat, open = 'r')
  output <- read_utf8(con)
  close(con)
  output
  
  con <- rawConnection(dat, open = 'r')
  scan(con, nmax = 2, quiet = TRUE)
  close(con)
  
  
  
}





