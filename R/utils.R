
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

get_endian_method <- function(con, endian) {
  endian <- endian %||% 
    attr(con, "endian", exact = TRUE) %||%
    "little"
  stopifnot(endian %in% c("big", "little"))
  endian
}


promote_methods <- c("dbl", "hex", "raw", 'bit64')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Tag a connection with the preferred integer promotion method for types
#' larger that R's integer type i.e. uint32, uint64, int64
#' 
#' @inheritParams read_uint8
#' 
#' @param uint32,int64,uint64 specifiy separate promotion methods for these types
#'        One of: 'dbl', 'hex', 'raw' and 'bit64' (for 64-bit types only) Default: 'dbl'
#'        \describe{
#'          \item{\code{dbl}}{Read in integers as doubles. Integer values above 2^53
#'          will lose precision.}
#'          \item{\code{hex}}{Each integer is returned as a
#'          hexadecimal string}
#'          \item{\code{raw}}{A single raw vector containing all the integers 
#'          in their original form}
#'          \item{\code{bit64}}{Return an \code{integer64} vector compatible with the
#'          \code{bit64} package.  Note. \code{integer64} is a \emph{signed} 64-bit
#'          integer}
#'        }
#'        
#' @return Modified connection object
#' @examples
#' con <- textConnection("hello")
#' con <- set_integer_promotion("big")
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set_integer_promotion <- function(con, uint32 = 'dbl', int64 = 'dbl', uint64 = 'dbl') {
  stopifnot(uint32 %in% setdiff(promote_methods, 'bit64'))
  stopifnot( int64 %in% promote_methods)
  stopifnot(uint64 %in% promote_methods)
  
  attr(con, "promote_uint32") <- uint32
  attr(con, "promote_int64" ) <-  int64
  attr(con, "promote_uint64") <- uint64
  con
}


get_promote_method <- function(con, promote, type) {
  
  attr_name <- paste0("promote_", type)
  
  promote <- promote %||% 
    attr(con, attr_name, exact = TRUE) %||%
    "dbl"
  
  stopifnot(promote %in% promote_methods)
  promote
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' For this connection, set the response when values do not fit into given type before writing.
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
  attr(con, "bounds_check") <- bounds_check
  con
}


get_bounds_check_method <- function(con, bounds_check) {
  bounds_check <- bounds_check %||% 
    attr(con, "bounds_check", exact = TRUE) %||%
    "error"
  stopifnot(bounds_check %in% c("ignore", "warn", "error"))
  bounds_check
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Set EOF handling for this connection
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
  attr(con, "eof_check") <- eof_check
  con
}


get_eof_check_method <- function(con, eof_check) {
  eof_check <- eof_check %||% 
    attr(con, "eof_check", exact = TRUE) %||%
    "error"
  stopifnot(eof_check %in% c("ignore", "warn", "error"))
  eof_check
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Check bounds on values about to be written fit into given type
#' 
#' @inheritParams read_uint8
#' @param na_check one of: ignore, warn, error
#' 
#' @return Modified connection object
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set_na_check <- function(con, na_check) {
  stopifnot(na_check %in% c('ignore', 'warn', 'error'))
  attr(con, "na_check") <- na_check
  con
}


get_na_check_method <- function(con, na_check) {
  na_check <- na_check %||% 
    attr(con, "na_check", exact = TRUE) %||%
    "error"
  stopifnot(na_check %in% c("ignore", "warn", "error"))
  na_check
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Standardise the EOF check
# 
# This will trigger if:
#  * 'eof_check' method set to 'warn' or 'error'
#  * n items requested < n items read
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
do_eof_check <- function(con, n_requested, n_read) {
  
  if (length(n_read) == 0) n_read <- 0
  if (length(n_requested) == 1 && length(n_read) == 1 && n_requested == n_read) return();
  
  method <- attr(con, 'eof_check', exact = TRUE) %||% 'error' 
  if (method == 'ignore') return();
  
  msg <- sprintf("EOF reached. Requested %i items, only got %i", n_requested, n_read)
  
  if (method == 'warn') {
    warning(msg)
  } else {
    stop(msg)
  }
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check bounds and report error
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
do_bounds_check <- function(x, bounds_check, lo, hi, lo_str = as.character(lo), hi_str = as.character(hi)) {
  if (bounds_check == 'ignore') return();

  if (any(x < lo) || any(x > hi)) {
    bad_vals <- x[x < lo | x > hi]
    message <- sprintf("Out of bounds [%s, %s] : %s", lo_str, hi_str, deparse1(bad_vals))
    if (bounds_check == "warn") {
      warning(message)
    } else {
      stop(message)
    }
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check for NA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
do_na_check <- function(x, na_check) {
  if (is.raw(x) || na_check == 'ignore') return()
  
  if (anyNA(x)) {
    bad_vals <- x[is.na(x)]  
    message <- sprintf("NAs in data: %s", deparse1(bad_vals))
    if (na_check == "warn") {
      warning(message)
    } else {
      stop(message)
    }
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







