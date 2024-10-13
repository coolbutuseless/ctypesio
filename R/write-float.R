



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert values to the given type and write to a connection
#' 
#' @inheritParams write_uint8
#' 
#' @return If \code{con} is a connection then this connection is returned invisibly.
#'         If \code{con} is a raw vector then new data is appended to this vector
#"         and returned.
#' @examples
#' con <- file(tempfile(), "wb")
#' write_f64(con, c(1, 2, 3, 4))
#' close(con)
#' @family data output functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_f64 <- function(con, x, endian = NULL, bounds_check = NULL, na_check = NULL) {
  endian   <- get_endian_method(con, endian)
  na_check <- get_na_check_method(con, na_check)
  
  if (is.raw(con)) {
    raw_orig <- con
    con <- raw()
  }
  
  x <- as.double(x)
  do_na_check(x, na_check)
  
  res <- writeBin(x, con, size = 8, endian = endian)
  
  if (is.raw(con)) {
    c(raw_orig, res)
  } else {
    invisible(con)
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname write_f64
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_dbl <- write_f64



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname write_f64
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_f32 <- function(con, x, endian = NULL, bounds_check = NULL, na_check = NULL) {
  endian <- get_endian_method(con, endian)
  bounds_check <- get_bounds_check_method(con, bounds_check)
  na_check <- get_na_check_method(con, na_check)
  
  if (is.raw(con)) {
    raw_orig <- con
    con <- raw()
  }
  
  x <- as.double(x)
  do_na_check(x, na_check)
  
  do_bounds_check(x, bounds_check, 
                  lo = -3.40282347E+38, hi = 3.40282347E+38, 
                  lo_str = "-3.40282347E+38", hi_str = "3.40282347E+38")
  
  res <- writeBin(x, con, size = 4, endian = endian)
  
  if (is.raw(con)) {
    c(raw_orig, res)
  } else {
    invisible(con)
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname write_f64
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_single <- write_f32


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname write_f64
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_f16 <- function(con, x, endian = NULL, bounds_check = NULL, na_check = NULL) {
  endian <- get_endian_method(con, endian)
  bounds_check <- get_bounds_check_method(con, bounds_check)
  na_check <- get_na_check_method(con, na_check)
  
  if (is.raw(con)) {
    raw_orig <- con
    con <- raw()
  }
  
  x <- as.double(x)
  do_na_check(x, na_check)
  
  do_bounds_check(x, bounds_check,  lo = -65504, hi = 65504)
  
  raw_vec <- rdbl_to_chalf(x, endian = endian)
  res <- writeBin(raw_vec, con)
  
  if (is.raw(con)) {
    c(raw_orig, res)
  } else {
    invisible(con)
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname write_f64
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_half <- write_f16





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a vector of standard R doubles to raw vector containing 16-bit floats 
#' 
#' Little endian bit layout
#' \itemize{
#'   \item{1 bit for sign (highest bit)}
#'   \item{5 bits for exponent}
#'   \item{10 bits for significand}
#' }
#' 
#' @param x R vector of standard R double precision (64-bit) floats
#' @param endian Default: 'little'.  Big endian not yet supported.
#' 
#' @return raw vector containing 16-bit floats i.e. 2 bytes per float
#' @examples
#' # Expect: c(0x02, 0x40)
#' # Reference: https://float.exposed/0x4002
#' rdbl_to_chalf(2.00390625)
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rdbl_to_chalf <- function(x, endian = 'little') {
  
  if (endian != 'little') {
    stop("Reading of half-precision floats is only implemented for little-endian")
  }
  
  xorig <- x
  
  sbit  <- as.integer(x < 0)
  x     <- abs(x)
  
  # Hnadle denormalised numbers differently from regular floats
  denorm   <- x < (2 ^ (-14))
  exponent <- ifelse(denorm, 0, floor(log2(x)))
  x        <- ifelse(denorm, x * 2^14, x / 2^(exponent))
  exponent <- ifelse(denorm, exponent, exponent + 15)
  x        <- ifelse(denorm | x < 1, x, x - 1)
  
  x
  
  
  significand <- rep_len(0, length(x))
  
  for (i in 9:0) {
    thresh <- 2^(-10 + i)
    above <- x >= thresh
    
    significand <- ifelse(above, significand + 2^i, significand)
    x        <- ifelse(above, x - thresh    , x)
  }
  
  if (FALSE) {
    # Debugging
    xorig
    sbit
    exponent
    significand
    
    rev(intToBits(exponent)[1: 5])
    rev(intToBits(significand)[1:10])
  }

  # Assemble 16-bit float  
  ints <- bitwShiftL(sbit, 15) +
    bitwShiftL(exponent, 10) + 
    significand
  
  ints <- as.integer(ints)
  
  # Convert to raw bytes
  writeBin(ints, raw(), size = 2, endian = endian)
}


