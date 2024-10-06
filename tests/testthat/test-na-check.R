


test_that("na_check  works", {
  
  expect_no_error({
    con <- rawConnection(raw(), "wb")
    write_dbl(con, c(1, 2, 3))
    close(con)  
  })
  
  
  expect_no_error({
    con <- rawConnection(raw(), "wb") |> set_na_check('ignore')
    write_dbl(con, c(1, 2, 3))
    close(con)  
  })
  
  
  expect_no_error({
    con <- rawConnection(raw(), "wb") |> set_na_check('warn')
    write_dbl(con, c(1, 2, 3))
    close(con)  
  })
  
  
  expect_no_error({
    con <- rawConnection(raw(), "wb") |> set_na_check('error')
    write_dbl(con, c(1, 2, 3))
    close(con)  
  })
  
  
  
  expect_error({
    con <- rawConnection(raw(), "wb")
    write_dbl(con, c(1, 2, NA_real_))
    close(con)  
  })
  
  
  expect_no_error({
    con <- rawConnection(raw(), "wb") |> set_na_check('ignore')
    write_dbl(con, c(1, 2, NA_real_))
    close(con)  
  })
  
  
  expect_warning({
    con <- rawConnection(raw(), "wb") |> set_na_check('warn')
    write_dbl(con, c(1, 2, NA_real_))
    close(con)  
  })
  
  
  expect_error({
    con <- rawConnection(raw(), "wb") |> set_na_check('error')
    write_dbl(con, c(1, 2, NA_real_))
    close(con)  
  })
  
  
  
})
