

test_that("read_str_raw() robust against multibyte strings", {
  
  tst <- as.raw(c(0x4d, 0xc3, 0xa9, 0x74, 0x61, 0x6c, 0x20, 0x63, 0x6f, 0x66, 0x66, 0x72, 0x65))
  
  con <- rawConnection(tst)
  on.exit(close(con))
  
  str <- read_str_raw(con, 13)
  
  # The raw vector is 13, but the string length is 12
  # When doing the EOF check, check the number of raw bytes read, not
  # the number of characters returned.
  expect_length(tst, 13)
  expect_true(nchar(str) == 12)
})
