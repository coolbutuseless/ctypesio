
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Standard NULL operator
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"%||%" <- function(x, y) {
  if (length(x) == 0) {
    y
  } else {
    x
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Tag a connection with the preferred endianness
#' 
#' @inheritParams read_uint8
#' @param endian Default endianness to assign to this connection. 
#'        Default: little
#' 
#' @return Modified connection object
#' @examples
#' con <- textConnection("hello")
#' con <- set_endian("big")
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set_endian <- function(con, endian = "little") {
  stopifnot(endian %in% c('big', 'little'))
  attr(con, "endian") <- endian
  con
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Tag a connection with the preferred integer promotion method
#' 
#' @inheritParams read_uint8
#' 
#' @param promote Default method of promotion for uint32, uint64 and int64.
#'        One of: dbl, raw, bitstring. Default: 'dbl'
#'        
#' @return Modified connection object
#' @examples
#' con <- textConnection("hello")
#' con <- set_integer_promotion("big")
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set_integer_promotion <- function(con, promote = 'dbl') {
  stopifnot(promote %in% c('dbl', 'raw', 'bitstring'))
  attr(con, "promote") <- promote
  con
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Check bounds on values about to be written fit into given type
#' 
#' @inheritParams read_uint8
#' @param bounds_check Default bounds checking behaviour. One of: ignore, warn, error.
#'        Default: error
#' 
#' @return Modified connection object
#' @examples
#' con <- textConnection("hello")
#' con <- set_bounds_check("warn")
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set_bounds_check <- function(con, bounds_check = 'error') {
  stopifnot(bounds_check %in% c('ignore', 'warn', 'error'))
  attr(con, "bounds_check") <- match(bounds_check, c('ignore', 'warn', 'error')) - 1L
  con
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Set EOF handling
#' 
#' @inheritParams read_uint8
#' @param eof_check ignore, warn, error.  If set to 'warn' then when EOF reached
#'        any further reads will spark a warning only.
#' 
#' @return Modified connection object
#' @examples
#' con <- textConnection("hello")
#' con <- set_eof_check("warn")
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set_eof_check <- function(con, eof_check = 'error') {
  stopifnot(eof_check %in% c('ignore', 'warn', 'error'))
  attr(con, "eof_check") <- match(eof_check, c('ignore', 'warn', 'error')) - 1L
  con
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Check bounds on values about to be written fit into given type
#' 
#' @inheritParams read_uint8
#' @param na_check one of: ignore, warn, error
#' 
#' @return Modified connection object
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set_na_check <- function(con, na_check) {
  stopifnot(na_check %in% c('ignore', 'warn', 'error'))
  attr(con, "na_check") <- match(na_check, c('ignore', 'warn', 'error')) - 1L
  con
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Standardise the EOF check
# 
# This will trigger if:
#  * 'eof_check' method set to 1 or 2 (warn or stop)
#  * n items requsted < n items read
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
eof_check <- function(con, n_requested, n_read) {
  
  if (length(n_read) == 0) n_read <- 0
  if (length(n_requested) == 1 && length(n_read) == 1 && n_requested == n_read) return();
  
  method <- attr(con, 'eof_check', exact = TRUE) %||% 2L # default to stop
  if (method == 0) return();
  
  msg <- sprintf("EOF reached. Requested %i items, only got %i", n_requested, n_read)
  
  if (method == 1) {
    warning(msg)
  } else {
    stop(msg)
  }
}


if (FALSE) {
  
  input <- 1:15
  N <- length(input)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Write data
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  con <- rawConnection(raw(), open = "w") 
  
  write_f32(con, input)  
  dat <- rawConnectionValue(con)  
  close(con)
  dat
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read it back
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  con <- rawConnection(dat, open = 'r') |> set_eof_check('warn')
  output <- read_f32(con, N)
  output <- read_f32(con, N)
  close(con)
  
  input
  output
  
}







