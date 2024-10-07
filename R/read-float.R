
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Read floating point values from a connection
#' 
#' Read floating point numbers into a standard R vector of doubles
#' 
#' \describe{
#'   \item{double precision}{8 byte floating point numbers. \code{read_f64()} 
#'         also available as \code{read_dbl()}}
#'   \item{single precision}{4 byte floating point numbers. \code{read_f32()} 
#'         also available as \code{read_float()}}
#'   \item{half precision}{2 byte floating point numbers. \code{read_f16()} 
#'         also available as \code{read_half()}.  Consists of 1 sign bit, 
#'         5 bits for exponent and 10 bits for fraction.}
#'   \item{bfloat}{2 byte floating point numbers in the bfloat format  \code{read_bfloat()}.
#'         Consits of 1 sign bit, 8 bits fo exponent and 7 bits for fraction.} 
#' }
#' 
#' @inheritParams read_uint8
#' @return vector of double precision floating point numbers
#' 
#' @examples
#' # Raw vector with 16 bytes (128 bits) of dummy data
#' data <- as.raw(1:16)
#' con <- rawConnection(data, 'rb')
#' read_f64(con, n = 1) # Read a 64-bit double-precision number
#' read_f16(con, n = 4) # Read 4 x 16-bit half-precision number
#' close(con)
#' 
#' @family data input functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_f64 <- function(con, n = 1, endian = NULL) {
  endian <- get_endian_method(con, endian)
  
  res <- readBin(con, 'double', n = n, size = 8, endian = endian)
  
  do_eof_check(con, n, length(res))
  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname read_f64
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_f32 <- function(con, n = 1, endian = NULL) {
  endian <- get_endian_method(con, endian)
  
  res <- readBin(con, 'double', n = n, size = 4, endian = endian)
  
  do_eof_check(con, n, length(res))
  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname read_f64
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_f16 <- function(con, n = 1, endian = NULL) {
  endian <- get_endian_method(con, endian)
  
  raw_vec <- readBin(con, 'raw', n = 2 * n, size = 1)
  res <- chalf_to_rdbl(raw_vec, endian)
  
  do_eof_check(con, n, length(res))
  res
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname read_f64
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_bfloat <- function(con, n = 1, endian = NULL) {
  endian <- get_endian_method(con, endian)
  
  raw_vec <- readBin(con, 'raw', n = 2 * n, size = 1)
  res <- bfloat_to_rdbl(raw_vec, endian)
  
  do_eof_check(con, n, length(res))
  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname read_f64
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_dbl <- read_f64

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname read_f64
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_float <- read_f32

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname read_f64
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_half <- read_f16







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a raw vector containing 16-bit floats to R vector of doubles
#' 
#' Little endian bit layout
#' \itemize{
#'   \item{1 bit for sign (highest bit)}
#'   \item{5 bits for exponent}
#'   \item{10 bits for significand}
#' }
#' 
#' @param x raw vector containing 16-bit floats i.e. 2 bytes per float
#' @param endian Default: 'little'.  Big endian not yet supported.
#' 
#' @return R vector of standard R double precision (64-bit) floats
#' @examples
#' # Expect: 2.00390625
#' # Reference: https://float.exposed/0x4002
#' chalf_to_rdbl(as.raw(c(0x02, 0x40)))
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
chalf_to_rdbl <- function(x, endian = 'little') {
  
  if (endian != 'little') {
    stop("Reading of half-precision floats is only implemented for little-endian")
  }
  
  # readBin() doesn't understand 16-bit floats
  # So read the raw data is unsigned 16-bit unsigned integers
  vals <- readBin(x, 'integer', n = length(x), size = 2, signed = FALSE, endian = 'little')
  
  # Split into the components of a 16-bit floating point value
  # significand : 10 bits
  # Exponent :  5 bits
  # Sign     :  1 bit
  (significand <- vals |>                   bitwAnd(2^10 - 1)) 
  (exponent <- vals |> bitwShiftR(10) |> bitwAnd(2^ 5 - 1))
  (sbit     <- vals |> bitwShiftR(15) |> bitwAnd(2^ 1 - 1))
  
  # Convert the components into a double precision value
  # See https://float.exposed
  part1 <- (-1) ^ sbit
  part2 <- ifelse(exponent == 0, 2^(-14), 2 ^ (exponent - 15))
  part3 <- 
    ifelse(exponent == 0, 0, 1) +
    (bitwAnd(significand,   1) > 0) * 2 ^ (-10) +
    (bitwAnd(significand,   2) > 0) * 2 ^ (- 9) +
    (bitwAnd(significand,   4) > 0) * 2 ^ (- 8) +
    (bitwAnd(significand,   8) > 0) * 2 ^ (- 7) +
    (bitwAnd(significand,  16) > 0) * 2 ^ (- 6) +
    (bitwAnd(significand,  32) > 0) * 2 ^ (- 5) +
    (bitwAnd(significand,  64) > 0) * 2 ^ (- 4) +
    (bitwAnd(significand, 128) > 0) * 2 ^ (- 3) +
    (bitwAnd(significand, 256) > 0) * 2 ^ (- 2) +
    (bitwAnd(significand, 512) > 0) * 2 ^ (- 1)
  
  part1 * part2 * part3
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a raw vector containing 16-bit bfloats to R vector of doubles
#' 
#' Little endian bit layout for \code{bfloat}
#' \itemize{
#'   \item{1 bit for sign (highest bit)}
#'   \item{8 bits for exponent}
#'   \item{7 bits for significand}
#' }
#' 
#' @param x raw vector containing 16-bit bfloats i.e. 2 bytes per float
#' @param endian Default: 'little'.  Big endian not yet supported.
#' 
#' @return R vector of standard R double precision (64-bit) floats
#' @examples
#' # Expect: 0.1259765625
#' # Reference: https://float.exposed/b0x3e01
#' bfloat_to_rdbl(as.raw(c(0x01, 0x3e)))
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bfloat_to_rdbl <- function(x, endian = 'little') {
  
  if (endian != 'little') {
    stop("Reading of bfloats is only implemented for little-endian")
  }
  
  # readBin() doesn't understand 16-bit floats
  # So read the raw data is unsigned 16-bit unsigned integers
  vals <- readBin(x, 'integer', n = length(x), size = 2, signed = FALSE, endian = 'little')
  
  # Split into the components of a 16-bit floating point value
  # significand : 7 bits
  # Exponent : 8 bits
  # Sign     : 1 bit
  (significand <- vals |>                   bitwAnd(2^ 7 - 1)) 
  (exponent <- vals |> bitwShiftR( 7) |> bitwAnd(2^ 8 - 1))
  (sbit     <- vals |> bitwShiftR(15) |> bitwAnd(2^ 1 - 1))
  
  # Convert the components into a double precision value
  # See https://float.exposed
  part1 <- (-1) ^ sbit
  part2 <- ifelse(exponent == 0, 2^(-126), 2 ^ (exponent - 127))
  part3 <- 
    ifelse(exponent == 0, 0, 1) +
    (bitwAnd(significand,   1) > 0) * 2 ^ (-7) +
    (bitwAnd(significand,   2) > 0) * 2 ^ (-6) +
    (bitwAnd(significand,   4) > 0) * 2 ^ (-5) +
    (bitwAnd(significand,   8) > 0) * 2 ^ (-4) +
    (bitwAnd(significand,  16) > 0) * 2 ^ (-3) +
    (bitwAnd(significand,  32) > 0) * 2 ^ (-2) +
    (bitwAnd(significand,  64) > 0) * 2 ^ (-1) 
  
  part1 * part2 * part3
}

