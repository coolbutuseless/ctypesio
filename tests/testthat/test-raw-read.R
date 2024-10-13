


test_that("reading ints from raw() works", {

  expect_identical(
    read_raw(as.raw(1)),
    as.raw(1)
  )
  
  expect_identical(
    read_int8(as.raw(1)),
    1L
  )
  
  expect_identical(
    read_uint8(as.raw(1)),
    1L
  )
  
  expect_identical(
    read_int16(as.raw(c(1, 0))),
    1L
  )
  
  expect_identical(
    read_uint16(as.raw(c(1, 0))),
    1L
  )
  
  expect_identical(
    read_int32(as.raw(c(1, 0, 0, 0))),
    1L
  )
  
  expect_identical(
    read_uint32(as.raw(c(1, 0, 0, 0))),
    1.0
  )
  
  expect_identical(
    read_int64(as.raw(c(1, 0, 0, 0, 0, 0, 0, 0))),
    1.0
  )
  
  expect_identical(
    read_uint64(as.raw(c(1, 0, 0, 0, 0, 0, 0, 0))),
    1.0
  )
})



test_that("reading float from raw() works", {
  
  expect_equal(
    read_f16(as.raw(c(0, 1))),
    1.52587890625e-05
  )  
  
  rv <- as.raw(c(0xd8, 0x4a))
  expect_equal(
    read_f16(rv),
    13.6875
  )
  
  rv <- as.raw( c(43, 43, 43, 43) )
  expect_equal(
    read_f32(rv),
    readBin(rv, 'double', size = 4)
  )
  
  rv <- as.raw( c(255, 255, 255, 255, 255, 255, 255, 32) )
  expect_equal(
    read_f64(rv),
    readBin(rv, 'double')
  )
  

})




test_that("writing int to raw() works", {
  
  # writing to empty raw results in data
  res <- write_int32(raw(), 1)  
  res
  expect_equal(res, as.raw(c(1, 0, 0, 0)))
  
  # Writing to raw with existing data appends the new data
  res <- write_int32(as.raw(c(1, 0, 0, 0)), 2)  
  res
  expect_equal(res, as.raw(c(1, 0, 0, 0, 2, 0, 0, 0)))
  
  # Can write multiple appending values  
  res <- raw()
  res <- write_uint8 (res, 1)
  res <- write_uint16(res, 1)
  res <- write_uint32(res, 1)
  res <- write_uint64(res, 1)
  
  expect_equal(
    res,
    as.raw(c(
      1, 
      1, 0,
      1, 0, 0, 0,
      1, 0, 0, 0, 0, 0, 0, 0
    ))
  )
  
})



test_that("writing float to raw() works", {
  
  # writing to empty raw results in data
  res <- write_f16(raw(), 1)  
  expect_equal(res, as.raw(c(0x00, 0x3c)))
  
  # Writing to raw with existing data appends the new data
  res <- write_f16(as.raw(c(0x00, 0x3c)), 2)  
  expect_equal(res, as.raw(c(0x00, 0x3c, 0x00, 0x40)))
  
  # Can write multiple appending values  
  res <- raw()
  res <- write_f16(res, 1)
  res <- write_f32(res, 1)
  res <- write_f64(res, 1)

  expect_length(res, 14)
  expect_equal(
    res,
    as.raw(c(
      0x00, 0x3c,
      0x00, 0x00, 0x80, 0x3f,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f
    ))
  )
  
})


test_that("writing utf8 to raw() works", {
  
  # writing to empty raw results in data
  res <- write_utf8(raw(), 'a')  
  expect_equal(res, as.raw(c(0x61, 0x00)))
  
  # Writing to raw with existing data appends the new data
  res <- write_utf8(as.raw(c(0x61, 0x00)), 'a')  
  expect_equal(res, as.raw(c(0x61, 0x00, 0x61, 0x00)))
  
  # Can write multiple appending values  
  res <- raw()
  res <- write_utf8(res, "a")
  res <- write_utf8(res, "a")
  res <- write_utf8_raw(res, "a")
  res <- write_utf8_raw(res, "a")
  
  expect_length(res, 6)
  expect_equal(
    res,
    as.raw(c(
      0x61, 0x00,
      0x61, 0x00,
      0x61, 0x61
    ))
  )
  
})

