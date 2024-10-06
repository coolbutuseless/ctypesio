


test_that("na_check  works", {
  
  con <- rawConnection(raw(), "wb")
  expect_no_error({
    write_dbl(con, c(1, 2, 3))
  })
  close(con)
  
  
  con <- rawConnection(raw(), "wb") |> set_na_check('ignore')
  expect_no_error({
    write_dbl(con, c(1, 2, 3))
  })
  close(con)
  
  
  con <- rawConnection(raw(), "wb") |> set_na_check('warn')
  expect_no_error({
    write_dbl(con, c(1, 2, 3))
  })
  close(con)
  
  
  con <- rawConnection(raw(), "wb") |> set_na_check('error')
  expect_no_error({
    write_dbl(con, c(1, 2, 3))
  })
  close(con)
  
  
  
  con <- rawConnection(raw(), "wb")
  expect_error({
    write_dbl(con, c(1, 2, NA_real_))
  })
  close(con)
  
  
  con <- rawConnection(raw(), "wb") |> set_na_check('ignore')
  expect_no_error({
    write_dbl(con, c(1, 2, NA_real_))
  })
  close(con)
  
  
  con <- rawConnection(raw(), "wb") |> set_na_check('warn')
  expect_warning({
    write_dbl(con, c(1, 2, NA_real_))
  })
  close(con)
  
  
  con <- rawConnection(raw(), "wb") |> set_na_check('error')
  expect_error({
    write_dbl(con, c(1, 2, NA_real_))
  })
  close(con)
  
  
  
})
