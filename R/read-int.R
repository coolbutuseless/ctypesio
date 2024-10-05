



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
#' @param con Connection object created with \code{file()}, \code{url()}, 
#'        \code{rawConnection()} or any of the other many connection creation
#'        functions.
#' @param endian Ordering of bytes within the file when reading multi-byte values.
#'        Possible values: 'big' or 'little'.  
#'        Default: NULL indicates that
#'        endian option should be retrieved from the connection object if possible
#'        (where the user has used \code{set_endian()}) or otherwise 
#'        will be set to \code{"little"}
#' @param n Number of elements to read. Default: 1
#' @param promote How should integer types be promoted when the type contains
#'        values larger than R's signed integer type. Possible options 'dbl', 'raw', 'hex'.
#'        Default: NULL indicates that
#'        this option should be retrieved from the connection object if possible
#'        (where the user has used \code{set_integer_promotion()}) or otherwise 
#'        will be set to \code{"dbl"}.
#'        \describe{
#'          \item{\code{dbl}}{Read integer values as double precision floating point}
#'          \item{\code{hex}}{Read integers as character vector of hexadecimal strings}
#'          \item{\code{raw}}{Read integer value as a sequence of raw bytes}
#'        }
#' @return Integer data. Usually in standard R integer vector but depending on 
#'         the \code{promote} option may be returned in alternate formats
#' @examples
#' # Raw vector with 16 bytes (128 bits) of dummy data
#' data <- as.raw(1:16)
#' con <- rawConnection(data, 'rb')
#' read_uint8(con, n = 4)
#' read_uint64(con, n = 1)
#' close(con)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_uint8 <- function(con, n = 1, endian = NULL) {
  endian <- get_endian_method(con, endian)
  
  res <- readBin(con, 'integer', n = n, size = 1, endian = endian, signed = FALSE)
  
  eof_check(con, n, length(res))
  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname read_uint8
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_int8 <- function(con, n = 1, endian = NULL) {
  endian <- get_endian_method(con, endian)
  
  res <- readBin(con, 'integer', n = n, size = 1, endian = endian, signed = TRUE)
  
  eof_check(con, n, length(res))
  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname read_uint8
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_int16 <- function(con, n = 1, endian = NULL) {
  endian <- get_endian_method(con, endian)
  
  res <- readBin(con, 'integer', n = n, size = 2, endian = endian, signed = TRUE)
  
  eof_check(con, n, length(res))
  res
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname read_uint8
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_uint16 <- function(con, n = 1, endian = NULL) {
  endian <- get_endian_method(con, endian)
  
  res <- readBin(con, 'integer', n = n, size = 2, endian = endian, signed = FALSE)
  
  eof_check(con, n, length(res))
  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname read_uint8
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_int32 <- function(con, n = 1, endian = NULL) {
  endian <- get_endian_method(con, endian)
  
  res <- readBin(con, 'integer', n = n, size = 4, endian = endian, signed = TRUE)
  
  eof_check(con, n, length(res))
  res
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname read_uint8
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_uint32 <- function(con, n = 1, endian = NULL, promote = NULL) {
  endian <- get_endian_method(con, endian)
  promote <- get_promote_method(con, promote)
  
  raw_vec <- readBin(con, 'raw', n = n * 4, size = 1)
  eof_check(con, n * 4, length(raw_vec))
  
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
read_int64 <- function(con, n = 1, endian = NULL, promote = NULL) {
  endian <- get_endian_method(con, endian)
  promote <- get_promote_method(con, promote)
  
  raw_vec <- readBin(con, 'raw', n = n * 8, size = 1)
  eof_check(con, n * 8, length(raw_vec))
  
  if (promote == 'dbl') {
    res <- .Call(convert_cint_to_rdbl_, raw_vec, 'int64', endian == 'big')
  } else if (promote == 'raw') {
    res <- raw_vec
  } else if (promote == 'hex') {
    res <- raw_to_hex(raw_vec, size = 8, endian = endian)
  } else {
    stop("Unknown promotion method: ", promote)
  }
  
  eof_check(con, n, length(res))
  res
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname read_uint8
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_uint64 <- function(con, n = 1, endian = NULL, promote = NULL) {
  endian <- get_endian_method(con, endian)
  promote <- get_promote_method(con, promote)
  
  raw_vec <- readBin(con, 'raw', n = n * 8, size = 1)
  eof_check(con, n * 8, length(raw_vec))
  
  if (promote  == 'dbl') {
    res <- .Call(convert_cint_to_rdbl_, raw_vec, 'uint64', endian == 'big')
  } else if (promote == 'raw') {
    res <- raw_vec
  } else if (promote == 'hex') {
    res <- raw_to_hex(raw_vec, size = 8, endian = endian)
  } else {
    stop("Unknown promotion method: ", promote)
  }
  
  
  res
}


if (FALSE) {
  
  con <- rawConnection(as.raw(1:255))
  read_uint32(con, n = 4, promote = 'hex')
  close(con)
  
}












