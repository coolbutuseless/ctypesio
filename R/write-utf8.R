


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Write raw bytes
#' 
#' @inheritParams write_uint8
#' 
#' @return The original connection is returned invisibly.
#' @examples
#' con <- file(tempfile(), "wb")
#' write_raw(con, as.raw(1:4))
#' write_raw(con, 1:4) 
#' close(con)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_raw <- function(con, x, bounds_check = NULL) {
  
  if (is.integer(x)) {
    bounds_check <- get_bounds_check_method(con, bounds_check)
    do_bounds_check(x, bounds_check,  lo = 0, hi = 255)
    x <- as.raw(x)
  }
  
  stopifnot(is.raw(x))
  writeBin(x, con)
  
  invisible(con)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Write UTF8 string
#' 
#' @inheritParams write_uint8
#' @param x single character string
#' 
#' @return The original connection is returned invisibly.
#' @examples
#' con <- file(tempfile(), "wb")
#' write_utf8(con, "hello")
#' close(con)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_utf8 <- function(con, x) {
  
  write_utf8_raw(con, x)
  write_uint8(con, 0) # Null terminator
  
  invisible(con)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname write_utf8
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_utf8_raw <- function(con, x) {
  stopifnot(is.character(x) && length(x) == 1)
  
  bytes <- as.raw(utf8ToInt(x))
  writeBin(bytes, con)
  
  invisible(con)
}











if (FALSE) {
  
  con <- rawConnection(raw(), open = "w")
  write_int8(con, c(-1, 3, 5))  
  dat <- rawConnectionValue(con)  
  close(con)
  dat
  
  con <- rawConnection(dat, open = 'r')
  read_uint8(con, 3)
  read_uint8(con, 1)
  close(con)
  
}



if (FALSE) {
  
  con <- rawConnection(raw(), open = "w")
  write_f16(con, c(1, 2)) 
  dat <- rawConnectionValue(con)  
  close(con)
  dat
  
  con <- rawConnection(dat, open = 'r')
  read_f64(con, 3)
  read_uint8(con, 1)
  close(con)
  
}



if (FALSE) {
  
  con <- rawConnection(raw(), open = "w")
  write_utf8(con, "hello there")
  dat <- rawConnectionValue(con)  
  close(con)
  dat
  
  con <- rawConnection(dat, open = 'r')
  read_utf8(con)
  read_uint8(con, 1)
  close(con)
  
}















