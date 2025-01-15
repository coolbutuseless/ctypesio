

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Uppwer and lower limits for bounds checking
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# width:           1,       2,         4,                 8
unsigned_lo <- c(   0  ,     0  , NA,     0  , NA, NA, NA,     0  )
unsigned_hi <- c( 2^8-1,  2^16-1, NA,  2^32-1, NA, NA, NA,  2^64-1)

signed_lo   <- c(-2^7  , -2^15  , NA, -2^31  , NA, NA, NA, -2^63  )
signed_hi   <- c( 2^7-1,  2^15-1, NA,  2^31-1, NA, NA, NA,  2^63-1)



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


int_size <- list(
   int8  = 1L,
  uint8  = 1L,
   int16 = 2L,
  uint16 = 2L,
   int32 = 4L,
  uint32 = 4L,
   int64 = 8L,
  uint64 = 8L
)


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
  
  if (is.raw(con)) {
    # message(">>>>> convert: it's RAW")
    raw_orig <- con
    con      <- raw()
    attributes(con) <- attributes(raw_orig)
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Figure out settings for this connection
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  endian       <- get_endian_method      (con, endian)
  bounds_check <- get_bounds_check_method(con, bounds_check)
  na_check     <- get_na_check_method    (con, na_check)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determine size and type
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  signed <- !startsWith(type, 'u')
  width <- int_size[[type]]
  if (length(width) == 0) {
    stop("Bad type: ", type)
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # bit64::integer64 type
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (inherits(x, 'integer64')) {
    
    if (!(type %in% c('int64', 'uint64'))) {
      stop("Can only write integer64 as 'int64' or 'uint64'")
    }
    
    x <- unclass(x)
    res <- write_dbl(con, x, bounds_check = FALSE)
    
    if (is.raw(con)) {
      res <- c(raw_orig, res)
      attributes(res) <- attributes(raw_orig)
      return(res)
    } else {
      return(con)
    }
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # hex string
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.character(x)) {
    
    stopifnot(!anyNA(x))
    
    # Remove 0x prefix if present
    x <- ifelse(startsWith(x, '0x'), substr(x, start = 3, stop = nchar(x)), x)
    
    # Check that hex strings are the correct length
    lens <- nchar(x)
    
    if (!all(lens == width * 2)) {
      msg <- sprintf("Writing '%s' from hex string requires nchar(x) == %i.  Actual lengths: %s",
                     type, width * 2, deparse1(sort(unique(lens))))
      stop(msg)
    }
    
    # Convert all hex to a single raw vector
    raw_vec <- hex_to_raw(x, endian = endian)
    
    res <- write_raw(con, raw_vec, bounds_check = FALSE)
    
    if (is.raw(con)) {
      res <- c(raw_orig, res)
      attributes(res) <- attributes(raw_orig)
      return(res)
    } else {
      return(con)
    }
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Numeric types
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
  
  res <- writeBin(raw_vec, con)
  
  if (is.raw(con)) {
    res <- c(raw_orig, res)
    attributes(res) <- attributes(raw_orig)
    return(res)
  } else {
    return(con)
  }
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert values to the given type and write to a connection
#' 
#' @param con Connection object or raw vector. When con is a raw vector, new
#'        data will be \emph{appended} to the vector and returned.
#'        Connection objects can be 
#'        created with \code{file()}, \code{url()}, 
#'        \code{rawConnection()} or any of the other many connection creation
#'        functions.
#' @param x vector to write
#' @param bounds_check Check values lie within bounds of the given type.
#'        Default: NULL indicates that
#'        this option should be retrieved from the connection object if possible
#'        (where the user has used \code{\link{set_bounds_check}()}) or otherwise 
#'        will be set to \code{"error"}
#' @param na_check Check for NAs in the data to be written.
#'        Default: NULL indicates that
#'        this option should be retrieved from the connection object if possible
#'        (where the user has used \code{\link{set_na_check}()}) or otherwise 
#'        will be set to \code{"error"}
#' @param endian Ordering of bytes within the file when reading multi-byte values.
#'        Possible values: 'big' or 'little'.  
#'        Default: NULL indicates that
#'        endian option should be retrieved from the connection object if possible
#'        (where the user has used \code{\link{set_endian}()}) or otherwise 
#'        will be set to \code{"little"}
#'        
#' @return If \code{con} is a connection then this connection is returned invisibly.
#'         If \code{con} is a raw vector then new data is appended to this vector
#"         and returned.
#' @examples
#' con <- file(tempfile(), "wb")
#' write_uint8(con, 1:4)
#' close(con)
#' @family data output functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_uint8 <- function(con, x, endian = NULL, bounds_check = NULL, na_check = NULL) {
  con <- convert_integer_core(con, x, type ='uint8' , endian = endian, 
                       bounds_check = bounds_check, na_check = na_check)
  
  if (is.raw(con)) {
    con
  } else {
    invisible(con)
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname write_uint8
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_int8 <- function(con, x, endian = NULL, bounds_check = NULL, na_check = NULL) {
  con <- convert_integer_core(con, x, type ='int8' , endian = endian,
                       bounds_check = bounds_check, na_check = na_check)
  
  if (is.raw(con)) {
    con
  } else {
    invisible(con)
  }
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname write_uint8
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_uint16 <- function(con, x, endian = NULL, bounds_check = NULL, na_check = NULL) {
  con <- convert_integer_core(con, x, type ='uint16' , endian = endian, 
                       bounds_check = bounds_check, na_check = na_check)
  
  if (is.raw(con)) {
    con
  } else {
    invisible(con)
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname write_uint8
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_int16 <- function(con, x, endian = NULL, bounds_check = NULL, na_check = NULL) {
  con <- convert_integer_core(con, x, type ='int16' , endian = endian, 
                       bounds_check = bounds_check, na_check = na_check)
  
  if (is.raw(con)) {
    con
  } else {
    invisible(con)
  }
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname write_uint8
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_uint32 <- function(con, x, endian = NULL, bounds_check = NULL, na_check = NULL) {
  con <- convert_integer_core(con, x, type ='uint32' , endian = endian, 
                       bounds_check = bounds_check, na_check = na_check)
  
  if (is.raw(con)) {
    con
  } else {
    invisible(con)
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname write_uint8
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_int32 <- function(con, x, endian = NULL, bounds_check = NULL, na_check = NULL) {
  con <- convert_integer_core(con, x, type ='int32' , endian = endian, 
                       bounds_check = bounds_check, na_check = na_check)
  
  if (is.raw(con)) {
    con
  } else {
    invisible(con)
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname write_uint8
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_uint64 <- function(con, x, endian = NULL, bounds_check = NULL, na_check = NULL) {
  con <- convert_integer_core(con, x, type ='uint64' , endian = endian, 
                       bounds_check = bounds_check, na_check = na_check)
  
  if (is.raw(con)) {
    con
  } else {
    invisible(con)
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname write_uint8
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_int64 <- function(con, x, endian = NULL, bounds_check = NULL, na_check = NULL) {
  con <- convert_integer_core(con, x, type ='int64' , endian = endian, 
                       bounds_check = bounds_check, na_check = na_check)
  
  if (is.raw(con)) {
    con
  } else {
    invisible(con)
  }
}


