test_that("utf8 roundtrips work", {
  input <- 
    "二項分布\xF0\x9F\x8E\xB2の英語表記は「Binomial distribution」である。"
  # 二項分布[dice]の英語表記は「Binomial distribution」である。
 

  con <- rawConnection(raw(), open = "w")
  write_utf8(con, input)
  dat <- rawConnectionValue(con)
  close(con)

  con <- rawConnection(dat, open = "r")
  output <- read_utf8(con)
  close(con)

  expect_equal(output, input, label = paste("roundtrip mismatch: ", "'utf8'"))
})




test_that("basic utf8 writing works", {
  
  con <- rawConnection(raw(), open = "w")
  write_utf8_raw(con, "hi")
  dat <- rawConnectionValue(con)
  close(con)
  
  expect_length(dat, 2)
  
  
  
  con <- rawConnection(raw(), open = "w")
  write_utf8(con, "hi")
  dat <- rawConnectionValue(con)
  close(con)
  
  expect_length(dat, 3)
})