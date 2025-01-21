
test_that("writing a raw vector with attributes works", {

  rv <- raw() |> 
    set_endian('big') |>
    write_uint32(1)

  expect_true(!is.null(attributes(rv)))
  
  expect_no_error({  
    write_raw(raw(), rv)
  })
})
