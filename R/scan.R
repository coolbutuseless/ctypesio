

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Read values encoded as characters strings
#' 
#' These functions are useful when the numeric values are encoded as strings written 
#' to the file, rather than as binary data.  Values must be delimited
#' by whitespace or other specified separator.  See documentation for 
#' \code{scan()} for more information.
#' 
#' @inheritParams read_uint8
#' @param ... further arguments passed to \code{scan()}
#' @param quiet Default: TRUE
#' @return Value of the given type
#' @examples
#' con <- textConnection(r"(
#'   type
#'   20 30
#'   3.14159
#' )")
#' 
#' scan_str(con)
#' scan_int(con)
#' scan_int(con)
#' scan_dbl(con)
#' close(con)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scan_dbl <- function(con, n = 1, quiet = TRUE, ...) {
  res <- scan(con, what = double(), n = n, quiet = quiet, ...)
  
  eof_check(con, n, length(res))
  res
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scan_dbl
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scan_int <- function(con, n = 1, quiet = TRUE, ...) {
  res <- scan(con, what = integer(), n = n, quiet = quiet, ...)
  
  eof_check(con, n, length(res))
  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scan_dbl
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scan_str <- function(con, n = 1, quiet = TRUE, ...) {
  res <- scan(con, what = character(), n = n, quiet = quiet, ...)
  
  eof_check(con, n, length(res))
  res
}



if (FALSE) {
  
  con <- textConnection(r"(
type
20 30
3.14159
)")

  scan_str(con)
  scan_int(con, n = 2)
  scan_dbl(con)
    
  
}