

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
#' @family data input functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scan_dbl <- function(con, n = 1, quiet = TRUE, ...) {
  res <- scan(con, what = double(), n = n, quiet = quiet, ...)
  
  do_eof_check(con, n, length(res))
  res
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scan_dbl
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scan_int <- function(con, n = 1, quiet = TRUE, ...) {
  res <- scan(con, what = integer(), n = n, quiet = quiet, ...)
  
  do_eof_check(con, n, length(res))
  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scan_dbl
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scan_str <- function(con, n = 1, quiet = TRUE, ...) {
  res <- scan(con, what = character(), n = n, quiet = quiet, ...)
  
  do_eof_check(con, n, length(res))
  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Print formatted strings to a connection
#' 
#' \code{fprintf_raw()} writes the text without a nul-terminator. \code{fprintf()}
#' writes a nul-terminator
#' 
#' @inheritParams write_uint8
#' @param fmt a character vector of format strings. See \code{\link{sprintf}()}
#' @param ... values to be passed in to \code{fmt}. See \code{\link{sprintf}()}
#' @param sep If there are multiple strings to be printed, this separated will be 
#'        written after each one.
#' @param useBytes See \code{\link{writeLines}()}
#' 
#' @return The original connection is returned invisibly.
#' @examples
#' con <- rawConnection(raw(), "wb")
#' fprintf(con, "%i,%6.2f", 1, 3.14159)
#' @family data output functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fprintf <- function(con, fmt, ..., sep = "\n", useBytes = FALSE) {
  fprintf_raw(con, fmt, ..., sep = sep, useBytes = useBytes)
  write_raw(con, 0L) # Nul-terminator
  invisible(con)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname fprintf
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fprintf_raw <- function(con, fmt, ..., sep = "\n", useBytes = FALSE) {
  msg <- sprintf(fmt, ...)
  writeLines(msg, con = con, sep = sep, useBytes = useBytes)
  invisible(con)
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