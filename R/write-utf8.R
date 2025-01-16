


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Write raw bytes
#' 
#' @inheritParams write_uint8
#' 
#' @return If \code{con} is a connection then this connection is returned invisibly.
#'         If \code{con} is a raw vector then new data is appended to this vector
#"         and returned.
#' @examples
#' con <- file(tempfile(), "wb")
#' write_raw(con, as.raw(1:4))
#' write_raw(con, 1:4) 
#' close(con)
#' @family data output functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_raw <- function(con, x, bounds_check = NULL) {
  
  if (is.integer(x)) {
    bounds_check <- get_bounds_check_method(con, bounds_check)
    do_bounds_check(x, bounds_check,  lo = 0, hi = 255)
    x <- as.raw(x)
  }
  
  if (is.raw(con)) {
    raw_orig <- con
    con <- raw()
    attributes(con) <- attributes(raw_orig)
  }
  
  stopifnot(is.raw(x))
  res <- writeBin(x, con)
  
  if (is.raw(con)) {
    res <- c(raw_orig, res)
    attributes(res) <- attributes(raw_orig)
    return(res)
  } else {
    invisible(con)
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Write UTF8 string
#' 
#' \code{write_utf8_raw()} writes the string without a nul-terminator.
#' \code{write_utf8()} includes a nul-terminator
#' 
#' @inheritParams write_uint8
#' @param x single character string
#' 
#' @return If \code{con} is a connection then this connection is returned invisibly.
#'         If \code{con} is a raw vector then new data is appended to this vector
#"         and returned.
#' @examples
#' con <- file(tempfile(), "wb")
#' write_utf8(con, "hello")
#' close(con)
#' @family data output functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_utf8 <- function(con, x) {
  
  if (is.raw(con)) {
    raw_orig <- con
    con <- raw()
    attributes(con) <- attributes(raw_orig)
  }
  
  res <- write_utf8_raw(con, x)
  write_uint8(con, 0) # Null terminator
  
  if (is.raw(con)) {
    res <- c(raw_orig, res, as.raw(0))
    attributes(res) <- attributes(raw_orig)
    return(res)
  } else {
    invisible(con)
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname write_utf8
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_utf8_raw <- function(con, x) {
  stopifnot(is.character(x) && length(x) == 1)
  
  if (is.raw(con)) {
    raw_orig <- con
    con <- raw()
    attributes(con) <- attributes(raw_orig)
  }

  xb <- iconv(x, to = "UTF-8", toRaw = TRUE)[[1]]

  res <- write_raw(con, xb)
  
  if (is.raw(con)) {
    res <- c(raw_orig, res)
    attributes(res) <- attributes(raw_orig)
    return(res)
  } else {
    invisible(con)
  }
}


