

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Read raw bytes
#'
#' @inheritParams read_uint8
#' 
#' @return raw vector
#' @examples
#' con <- rawConnection(charToRaw("hello12.3"))
#' read_raw(con, 5)
#' close(con)
#' @family data input functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_raw <- function(con, n = 1) {
  res <- readBin(con, 'raw', n = n, size = 1)
  
  do_eof_check(con, n, length(res))
  res
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Read a character string from a connection
#' 
#' Read character string from a connection. 
#' 
#' Functions which have a suffix of \code{_raw} are for handling character 
#' strings without a nul-terminator.
#' 
#' 
#' @inheritParams read_uint8
#' @param n number of characters to read.
#' 
#' @return single character string
#' @examples
#' con <- rawConnection(c(charToRaw("hello12.3"), as.raw(0)))
#' read_str(con)
#' close(con)
#' 
#' con <- rawConnection(charToRaw("hello12.3"))
#' read_str_raw(con, 5)
#' close(con)
#' 
#' con <- rawConnection(c(charToRaw("hello12.3"), as.raw(0)))
#' read_utf8(con)
#' close(con)
#' 
#' con <- rawConnection(charToRaw("hello12.3"))
#' read_utf8_raw(con, 3)
#' close(con)
#' @family data input functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_str <- function(con) {
  res <- readBin(con, 'character', n = 1, size = 1)
  
  do_eof_check(con, 1, length(res))
  res
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname read_str
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_str_raw <- function(con, n) {
  vec <- read_raw(con, n)
  do_eof_check(con, n, length(vec))
  
  res <- rawToChar(vec)
  res
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname read_str
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_utf8   <- function(con) {
  
  bytes <- integer(0)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Keep reading bytes until we hit a null terminator
  # then convert the integers to utf8
  # This is pretty terriblt, but I want to avoid any seek() calls
  # because - according to "?seek" - support on windows is a bit shit.
  # Could pre-allocate vector and grow? Good enough for now. Mike 2024-10-05
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ch <- read_uint8(con)
  while (ch != 0) {
    bytes <- c(bytes, ch)
    ch    <- read_uint8(con)
    if (length(ch) == 0) {
      stop("Reached EOF looking for string null terminator")
    }
  }

  str <- rawToChar(as.raw(bytes))
  Encoding(str) <- "UTF-8"
  
  str
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname read_str
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_utf8_raw <- function(con, n) {
  raw_vec <- read_raw(con, n = n)
  
  # Convert string to UTF-8 and return
  res <- iconv(list(raw_vec), from = "UTF-8")
  
  do_eof_check(con, 1, length(res))
  res
}



if (FALSE) {
  
  con <- rawConnection(charToRaw("hello12.3"))
  read_str_raw(con, 5)
  
}










