

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Uppwer and lower limits for bounds checking
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# width:           1,       2,         4,                 8
unsigned_lo <- c(   0  ,     0  , NA,     0  , NA, NA, NA,     0  )
unsigned_hi <- c( 2^8  ,  2^16  , NA,  2^32  , NA, NA, NA,  2^64  )

signed_lo <- c(-2^7  , -2^15  , NA, -2^31  , NA, NA, NA, -2^63  )
signed_hi <- c( 2^7-1,  2^15-1, NA,  2^31-1, NA, NA, NA,  2^63-1)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert standard R int32 to a bunch of different types
#' 
#' @inheritParams read_uint8
#' @param x dbl vector
#' @param type 'uint32', 'uint64', 'int64'
#' 
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dbl_to_int <- function(x, type, endian = "little") {

  stopifnot(type %in% c(
    'uint32', 
    'uint64', 'int64'
  ))
  
  stopifnot(is.double(x))

  .Call(convert_rdbl_to_cint_, x, type, endian == 'big')
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Write 
#' 
#' @inheritParams read_uint8
#' @param x vector to write
#' @param signed Logical.  Signed value?
#' @param width width of type in bytes.
#' @param bounds_check check values lie within bounds. Default: "error"
#' @param na_check check for NA values in tdata to be written
#' @return raw vector
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
convert_integer_core <- function(con, x, type, endian, bounds_check, na_check) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Figure out 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  endian <- get_endian_method(con, endian)
  bounds_check <- get_bounds_check_method(con, bounds_check)
  na_check <- get_na_check_method(con, na_check)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determine size and type
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  signed <- !startsWith(type, 'u')
  if (endsWith(type, '8')) {
    width <- 1L
  } else if (endsWith(type, '6')) {
    width <- 2L
  } else if (endsWith(type, '2')) {
    width <- 4L
  } else if (endsWith(type, '4')) {
    width <- 8L
  } else {
    stop("Bad type: ", type)
  }
  
  do_na_check(x, na_check)
  
  if (bounds_check != "ignore") {
    lo <- ifelse(signed, signed_lo[width], unsigned_lo[width])
    hi <- ifelse(signed, signed_hi[width], unsigned_hi[width])
    do_bounds_check(x, bounds_check, lo = lo, hi = hi)
  }
  
  if (is.double(x) && type %in% c('uint32', 'uint64', 'int64')) {
    raw_vec <- dbl_to_int(x, type, endian = endian)
  } else {
    x <- as.integer(x)
    raw_vec <- writeBin(x, raw(), size = width, endian = endian)
  }
  
  writeBin(raw_vec, con)
  con
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert values to the given type and write to a connection
#' 
#' @inheritParams read_uint8
#' @param x vector to write
#' @param bounds_check Check values lie within bounds of the given type.
#'        Default: NULL indicates that
#'        this option should be retrieved from the connection object if possible
#'        (where the user has used \code{set_bounds_check()}) or otherwise 
#'        will be set to \code{"error"}
#' @param na_check Check for NAs in the data to be written.
#'        Default: NULL indicates that
#'        this option should be retrieved from the connection object if possible
#'        (where the user has used \code{set_na_check()}) or otherwise 
#'        will be set to \code{"error"}
#'        
#' @return The original connection is returned invisibly.
#' @examples
#' con <- file(tempfile(), "wb")
#' write_uint8(con, 1:4)
#' close(con)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_uint8 <- function(con, x, endian = NULL, bounds_check = NULL, na_check = NULL) {
  convert_integer_core(con, x, type ='uint8' , endian = endian, 
                       bounds_check = bounds_check, na_check = na_check)
  invisible(con)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname write_uint8
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_int8 <- function(con, x, endian = NULL, bounds_check = NULL, na_check = NULL) {
  convert_integer_core(con, x, type ='int8' , endian = endian,
                       bounds_check = bounds_check, na_check = na_check)
  invisible(con)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname write_uint8
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_uint16 <- function(con, x, endian = NULL, bounds_check = NULL, na_check = NULL) {
  convert_integer_core(con, x, type ='uint16' , endian = endian, 
                       bounds_check = bounds_check, na_check = na_check)
  invisible(con)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname write_uint8
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_int16 <- function(con, x, endian = NULL, bounds_check = NULL, na_check = NULL) {
  convert_integer_core(con, x, type ='int16' , endian = endian, 
                       bounds_check = bounds_check, na_check = na_check)
  invisible(con)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname write_uint8
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_uint32 <- function(con, x, endian = NULL, bounds_check = NULL, na_check = NULL) {
  convert_integer_core(con, x, type ='uint32' , endian = endian, 
                       bounds_check = bounds_check, na_check = na_check)
  invisible(con)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname write_uint8
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_int32 <- function(con, x, endian = NULL, bounds_check = NULL, na_check = NULL) {
  convert_integer_core(con, x, type ='int32' , endian = endian, 
                       bounds_check = bounds_check, na_check = na_check)
  invisible(con)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname write_uint8
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_uint64 <- function(con, x, endian = NULL, bounds_check = NULL, na_check = NULL) {
  convert_integer_core(con, x, type ='uint64' , endian = endian, 
                       bounds_check = bounds_check, na_check = na_check)
  invisible(con)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname write_uint8
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_int64 <- function(con, x, endian = NULL, bounds_check = NULL, na_check = NULL) {
  convert_integer_core(con, x, type ='int64' , endian = endian, 
                       bounds_check = bounds_check, na_check = na_check)
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















