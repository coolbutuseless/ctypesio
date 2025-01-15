


test_that("set_endian() on raw() works", {
  
  res <- raw() |> 
    set_endian('big') |> 
    write_uint32(1)
  expect_equal(res, as.raw(c(0, 0, 0, 1)), ignore_attr = TRUE)
  
  res <- raw() |> 
    set_endian('little') |> 
    write_uint32(1)
  expect_identical(res, as.raw(c(1, 0, 0, 0)), ignore_attr = TRUE)
  
  res <- raw() |> 
    set_endian('big') |>
    write_f64(1)
  expect_identical(res, rev(as.raw(c(0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f))), 
                   ignore_attr = TRUE)
  
  res <- raw() |> 
    set_endian('little') |>
    write_f64(1)
  expect_identical(res, as.raw(c(0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f)), ignore_attr = TRUE)
  
  
  res <- raw() |> 
    set_endian('big') |>
    write_hex('faf0')
  expect_identical(res, as.raw(c(0xfa, 0xf0)), ignore_attr = TRUE)
  
  res <- raw() |> 
    set_endian('little') |>
    write_hex('faf0')
  expect_identical(res, as.raw(c(0xf0, 0xfa)), ignore_attr = TRUE)
})
