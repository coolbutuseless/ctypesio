



test_that("half-precision little endian works", {
  
  # https://float.exposed/b0x3f81
  expect_identical(
    bfloat_to_rdbl(as.raw(c(0x81, 0x3f))), 
    1.0078125
  )
  
  
  # https://float.exposed/b0x3983
  expect_identical(
    bfloat_to_rdbl(as.raw(c(0x83, 0x39))), # 0x4ad8
    0.0002498626708984375
  )  
  
  # https://float.exposed/b0x0000
  expect_identical(
    bfloat_to_rdbl(as.raw(c(0x00, 0x00))), # 0x0000
    0
  )

  # denormalised  
  # https://float.exposed/b0x0041
  expect_identical(
    bfloat_to_rdbl(as.raw(c(0x41, 0x00))), # 0x4ad8
    5.9693072502694287514e-39
  )
})
