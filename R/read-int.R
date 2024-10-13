
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Read integer data from a connection
#' 
#' Read integer values into a standard R vector of integers or alternate
#' containers for large types
#' 
#' \describe{
#'   \item{8-bit integers}{\code{read_int8()} and \code{read_uint8()}}
#'   \item{16-bit integers}{\code{read_int16()} and \code{read_uint16()}}
#'   \item{32-bit integers}{\code{read_int32()} and \code{read_uint32()}}
#'   \item{64-bit integers}{\code{read_int64()} and \code{read_uint64()}}
#' }
#' 
#' @inheritParams write_uint8
#' @param con Connection object or raw vector. Connection objects can be 
#'        created with \code{file()}, \code{url()}, 
#'        \code{rawConnection()} or any of the other many connection creation
#'        functions.
#' @param endian Ordering of bytes within the file when reading multi-byte values.
#'        Possible values: 'big' or 'little'.  
#'        Default: NULL indicates that
#'        endian option should be retrieved from the connection object if possible
#'        (where the user has used \code{\link{set_endian}()}) or otherwise 
#'        will be set to \code{"little"}
#' @param n Number of elements to read. Default: 1
#' @param promote For 'uin32', 'int64' and 'uint64' types, the range of possible
#'        values exceeds R's standard integer type.  For these integer types, 
#'        values will be promoted to a different container type.
#'        Possible options 'dbl', 'raw', 'hex' and 'bit64'.
#'        Default: NULL indicates that
#'        this option should be retrieved from the connection object if possible
#'        (where the user has used \code{\link{set_integer_promotion}()}) or otherwise 
#'        will default to \code{"dbl"}.
#'        \describe{
#'          \item{\code{dbl}}{Read integer values as double precision floating point. A 'double' will
#'                hold integer values (without loss) from -(2^53) up to (2^53).  A 
#'                further warning will be issued if an attempt is made to store 
#'                an integer value that lies outside this range}
#'          \item{\code{hex}}{Read integers as character vector of hexadecimal strings}
#'          \item{\code{raw}}{Read integer value as a sequence of raw bytes}
#'          \item{\code{bit64}}{Read integer value as a vector of 
#'                type \code{bit64::integer64}. This is valid only when reading
#'                'int64' and 'uint64' types}
#'        }
#' @return Integer data. Usually in standard R integer vector but depending on 
#'         the \code{promote} option may be returned in alternate formats
#' @examples
#' # Raw vector with 16 bytes (128 bits) of dummy data
#' data <- as.raw(c(1:7, 0, 1:8))
#' con <- rawConnection(data, 'rb')
#' read_int64(con, n = 1)
#' read_uint8(con, n = 4)
#' close(con)
#' 
#' @family data input functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_uint8 <- function(con, n = 1, endian = NULL) {
  endian <- get_endian_method(con, endian)
  
  res <- readBin(con, 'integer', n = n, size = 1, endian = endian, signed = FALSE)
  
  do_eof_check(con, n, length(res))
  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname read_uint8
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_int8 <- function(con, n = 1, endian = NULL) {
  endian <- get_endian_method(con, endian)
  
  res <- readBin(con, 'integer', n = n, size = 1, endian = endian, signed = TRUE)
  
  do_eof_check(con, n, length(res))
  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname read_uint8
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_int16 <- function(con, n = 1, endian = NULL) {
  endian <- get_endian_method(con, endian)
  
  res <- readBin(con, 'integer', n = n, size = 2, endian = endian, signed = TRUE)
  
  do_eof_check(con, n, length(res))
  res
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname read_uint8
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_uint16 <- function(con, n = 1, endian = NULL) {
  endian <- get_endian_method(con, endian)
  
  res <- readBin(con, 'integer', n = n, size = 2, endian = endian, signed = FALSE)
  
  do_eof_check(con, n, length(res))
  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname read_uint8
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_int32 <- function(con, n = 1, endian = NULL) {
  endian <- get_endian_method(con, endian)
  
  res <- readBin(con, 'integer', n = n, size = 4, endian = endian, signed = TRUE)
  
  do_eof_check(con, n, length(res))
  res
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname read_uint8
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_uint32 <- function(con, n = 1, endian = NULL, promote = NULL) {
  endian <- get_endian_method(con, endian)
  promote <- get_promote_method(con, promote, type = 'uint32')
  
  raw_vec <- readBin(con, 'raw', n = n * 4, size = 1)
  do_eof_check(con, n * 4, length(raw_vec))
  
  if (promote == 'dbl') {
    res <- .Call(convert_cint_to_rdbl_, raw_vec, 'uint32', endian == 'big')
  } else if (promote == 'raw') {
    res <- raw_vec
  } else if (promote == 'hex') {
    res <- raw_to_hex(raw_vec, size = 4, endian = endian)
  }  else {
    stop("Unknown promotion method: ", promote)
  }
  
  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname read_uint8
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_int64 <- function(con, n = 1, endian = NULL, promote = NULL, bounds_check = NULL) {
  endian <- get_endian_method(con, endian)
  promote <- get_promote_method(con, promote, type = 'int64')
  bounds_check <- get_bounds_check_method(con, bounds_check)
  
  raw_vec <- readBin(con, 'raw', n = n * 8, size = 1)
  do_eof_check(con, n * 8, length(raw_vec))
  
  if (promote == 'dbl') {
    res <- .Call(convert_cint_to_rdbl_, raw_vec, 'int64', endian == 'big')
    do_bounds_check(res, bounds_check, lo = -(2^53), hi = (2^53), lo_str = "-(2^53)", hi_str = "2^53") 

  } else if (promote == 'raw') {
    res <- raw_vec
  } else if (promote == 'hex') {
    res <- raw_to_hex(raw_vec, size = 8, endian = endian)
  } else if (promote == 'bit64') {
    rcon <- rawConnection(raw_vec, "rb")
    res <- read_dbl(rcon, n = n, endian = endian)
    close(rcon)
    class(res) <- 'integer64'
  } else {
    stop("Unknown promotion method: ", promote)
  }
  
  do_eof_check(con, n, length(res))
  res
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname read_uint8
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_uint64 <- function(con, n = 1, endian = NULL, promote = NULL, bounds_check = NULL) {
  endian <- get_endian_method(con, endian)
  promote <- get_promote_method(con, promote, type = 'uint64')
  bounds_check <- get_bounds_check_method(con, bounds_check)
  
  raw_vec <- readBin(con, 'raw', n = n * 8, size = 1)
  do_eof_check(con, n * 8, length(raw_vec))
  
  if (promote  == 'dbl') {
    res <- .Call(convert_cint_to_rdbl_, raw_vec, 'uint64', endian == 'big')
    do_bounds_check(res, bounds_check, lo = 0, hi = (2^53), lo_str = "0", hi_str = "2^53")
  } else if (promote == 'raw') {
    res <- raw_vec
  } else if (promote == 'hex') {
    res <- raw_to_hex(raw_vec, size = 8, endian = endian)
  } else if (promote == 'bit64') {
    rcon <- rawConnection(raw_vec, "rb")
    res <- read_dbl(rcon, n = n, endian = endian)
    close(rcon)
    class(res) <- 'integer64'
    # bit64::integer64 is a signed type.
    # So if we ever get back a -ve number when trying to read an uint64, it
    # means that the value is above 2^63 and not properly representable
    # in this data type
    do_bounds_check(res, bounds_check, lo = 0, hi = Inf, lo_str = "0", hi_str = "2^64")
  } else {
    stop("Unknown promotion method: ", promote)
  }
  
  
  res
}


