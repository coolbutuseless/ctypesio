

test_that("int32: Basic read hex ok", {
  
  con <- rawConnection(raw(), "wb")
  write_int32(con, c(1, 2))  
  dat <- rawConnectionValue(con)
  close(con)

  con <- rawConnection(dat, "rb")
  expect_identical(read_hex(con, 1, size = 4), "00000001")
  expect_identical(read_hex(con, 1, size = 4), "00000002")
  close(con)
})




test_that("int32: Basic write hex ok", {
  con <- rawConnection(raw(), "wb")
  write_int32(con, c("00000001", "00000002"))  
  dat <- rawConnectionValue(con)
  close(con)
  
  con <- rawConnection(dat, "rb")
  expect_identical(read_hex(con, 1, size = 4), "00000001")
  expect_identical(read_hex(con, 1, size = 4), "00000002")
  close(con)
})



test_that("int32: Incorrect hex length raises error", {
  con <- rawConnection(raw(), "wb")
  on.exit(close(con))
  expect_error(
    write_int32(con, c("00000001ff"))
  )
})



test_that("int32: Nice error message when character is not well formed hex", {
  con <- rawConnection(raw(), "wb")
  on.exit(close(con))
  expect_error(
    write_int32(con, c("hellhell"))
  )
})




test_that("int32: 0x prefix allowed", {
  con <- rawConnection(raw(), "wb")
  write_int32(con, c("0x00000001", "0x00000002"))  
  dat <- rawConnectionValue(con)
  close(con)
  
  con <- rawConnection(dat, "rb")
  expect_identical(read_hex(con, 1, size = 4), "00000001")
  expect_identical(read_hex(con, 1, size = 4), "00000002")
  close(con)
})




