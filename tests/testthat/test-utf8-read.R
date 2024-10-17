

test_that("utf8 read not super broken", {
  
  filename <- tempfile()
  txt_init <- "привет"
  writeLines(txt_init, filename, sep = "")
  
  con <- file(filename, "rb")
  txt_read <- read_utf8_raw(con, file.size(filename))
  close(con)
  
  expect_identical(txt_read, txt_init)
  
})
