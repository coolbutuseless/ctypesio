

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Read raw bytes as a single hexadecimal string
#' 
#' @inheritParams read_uint8
#' 
#' @return Single character string
#' @examples
#' con <- rawConnection(as.raw(1:4))
#' read_hex(con, 4)
#' close(con)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_hex <- function(con, n = 1, endian = NULL) {
  endian  <- endian  %||% attr(con, 'endian' , TRUE) %||% "little"
  
  raw_vec <- read_raw(con, n = n)
  eof_check(con, n, length(raw_vec))
  
  if (endian == 'little') {
    raw_vec <- rev(raw_vec)
  }
  
  paste(sprintf("%02x", as.integer(raw_vec)), collapse = "")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Write hexadecimal string as raw bytes
#' 
#' @inheritParams write_uint8
#' 
#' @return The original connection is returned invisibly.
#' @examples
#' con <- file(tempfile(), "wb")
#' write_hex(con, "ff80")
#' close(con)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_hex <- function(con, x, endian = NULL) {
  endian  <- endian  %||% attr(con, 'endian' , TRUE) %||% "little"
  
  x <- tolower(x)
  nibbles <- strsplit(x, "")[[1]]
  stopifnot(length(nibbles) %% 2 == 0)
  nibbles <- match(nibbles, c(0:9, letters[1:6])) - 1L
  stopifnot(!anyNA(nibbles))
  
  bytes <- nibbles[c(TRUE, FALSE)] * 16 + nibbles[c(FALSE, TRUE)] 
  
  if (endian == 'little') {
    bytes <- rev(bytes)
  }
  
  write_uint8(con, bytes, endian = endian, bounds_check = 0)
  invisible(con)
}