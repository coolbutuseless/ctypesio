

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Read bytes as hexadecimal strings
#' 
#' @inheritParams read_uint8
#' 
#' @param n Number of hexadecimal strings to read. Default: 1
#' @param size size in bytes of each string. Default: 1
#' 
#' @return vector of hexadecimal character strings
#' @examples
#' con <- rawConnection(as.raw(1:4))
#' read_hex(con, n = 4, size = 1)
#' close(con)
#' 
#' con <- rawConnection(as.raw(1:4))
#' read_hex(con, n = 1, size = 4)
#' close(con)
#' 
#' con <- rawConnection(as.raw(1:4))
#' read_hex(con, n = 2, size = 2, endian = "big")
#' close(con)
#' @family data input functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_hex <- function(con, n = 1, size = 1, endian = NULL) {
  endian <- get_endian_method(con, endian)
  
  raw_vec <- read_raw(con, n = n * size)
  do_eof_check(con, n * size, length(raw_vec))
  
  raw_to_hex(raw_vec, size = size, endian = endian)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Write hexadecimal string as raw bytes
#' 
#' @inheritParams write_uint8
#' 
#' @return If \code{con} is a connection then this connection is returned invisibly.
#'         If \code{con} is a raw vector then new data is appended to this vector
#"         and returned.
#' @examples
#' con <- file(tempfile(), "wb")
#' write_hex(con, c("ff80", "0102"))
#' close(con)
#' @family data output functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_hex <- function(con, x, endian = NULL) {
  endian <- get_endian_method(con, endian)
  
  if (is.raw(con)) {
    raw_orig <- con
    con <- raw()
  }
  
  raw_vec <- hex_to_raw(x, endian = endian)
  res <- write_raw(con, raw_vec)
  
  if (is.raw(con)) {
    c(raw_orig, res)
  } else {
    invisible(con)
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a raw vector to a vector of hex strings of the given size
#'
#' @param raw_vec raw vector
#' @param size size (in bytes) of each hex string. If NULL then only a 
#'        single concatenated hex string is returned. 
#' @param endian should the bytes be converted to nex strings as little or
#'        big endian. Default: little
#' 
#' @return character vector of hex strings
#' @examples
#' raw_vec <- as.raw(1:16)
#' raw_to_hex(raw_vec, size = 4)
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
raw_to_hex <- function(raw_vec, size = NULL, endian = "little") {
  hex <- sprintf("%02x", as.integer(raw_vec))
  
  if (is.null(size)) {
    hex <- paste(hex, collapse = "")
  } else {
    stopifnot(length(hex) %% size == 0)
    dim(hex) <- c(size, length(hex)/size)
    
    if (endian == "little") {
      hex[] <- hex[rev(seq_len(nrow(hex))), ]
    }
    
    hex <- apply(hex, 2, paste, collapse = "")
  }
  
  hex
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a vector of hex strings to a single raw vector
#' 
#' @param x vector of hex strings
#' @param endian should the hex strings be converted as little or
#'        big endian. Default: little
#'        
#' @return raw vector
#' @examples
#' hex_to_raw(c("0102", "0304"))
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hex_to_raw <- function(x, endian = "little") {
  x <- tolower(x)
  nibbles <- strsplit(x, "")
  
  if (endian == "little") {
    nibbles <- lapply(nibbles, rev)
  }
  
  nibbles <- unlist(nibbles, use.names = FALSE)
  stopifnot(length(nibbles) %% 2 == 0)
  nibbles <- match(nibbles, c(0:9, letters[1:6])) - 1L
  if (anyNA(nibbles)) {
    stop("Hexadecimal string contains characters other than 0123456789abcdef: ", 
         deparse1(x))
  }
  
  if (endian == "little") {
    bytes <- nibbles[c(FALSE, TRUE)] * 16 + nibbles[c(TRUE, FALSE)] 
  } else {
    bytes <- nibbles[c(TRUE, FALSE)] * 16 + nibbles[c(FALSE, TRUE)] 
  }
  
  as.raw(bytes)
}


if (FALSE) {
  
  raw_vec <- as.raw(1:16)
  raw_to_hex(raw_vec, size = 4, endian = "big") |> hex_to_raw(endian = "big")

}






