

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Tag a connection with the preferred endianness
#' 
#' @inheritParams read_uint8
#' @param endian Default endianness to assign to this connection. One of either
#'        "little" or "big".  Default: "little".
#'        This default may be over-ridden by specifying
#'        the \code{endian} argument when calling individual functions.
#' 
#' @return Modified connection object
#' @examples
#' # Open a connection and configure it so all subsequent read/write operations
#' # use big-endian ordering.
#' con <- rawConnection(as.raw(c(0, 1, 0, 1)), "rb")
#' con <- set_endian(con, endian = "big")
#' 
#' # Future reads will be be big endian
#' read_uint16(con, n = 1)
#' 
#' # Unless over-ridden during the read
#' read_uint16(con, n = 1, endian = "little")
#' 
#' close(con)
#' 
#' @family connection configuration functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set_endian <- function(con, endian = "little") {
  stopifnot(endian %in% c('big', 'little'))
  attr(con, "endian") <- endian
  con
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Helper function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
#'        One of: 'dbl', 'hex', 'raw' and 'bit64' (for 64-bit types only) Default: 'dbl'.
#'        This default may be over-ridden by specifying
#'        the \code{promote} argument when calling individual functions.
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
#' # Open a connection and configure it so all 'uint32' values are 
#' # read as floating point and all all 'uint64' values are read as hexadecimal strings
#' con <- rawConnection(as.raw(c(1:7, 0, 1:7, 0, 1:7, 0, 1:7, 0)), "rb")
#' con <- set_integer_promotion(con, uint32 = "dbl", uint64 = "hex")
#' 
#' # Future reads of uint64 will return hex strings
#' read_uint64(con, n = 2)
#' 
#' # Unless over-ridden during the read
#' read_uint64(con, n = 1, promote = "dbl")
#' 
#' close(con)
#' 
#' @family connection configuration functions
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Helper function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
#' @param bounds_check Default bounds checking behaviour. One of: 'ignore', 'warn', 'error'.
#'        Default: 'error'.
#'        This default may be over-ridden by specifying
#'        the \code{bounds_check} argument when calling individual functions.
#'        \describe{
#'          \item{\code{ignore}}{No explicit checks will be made for
#'          out-of-bound values.
#'          The underlying R functions (e.g. \code{readBin()}, \code{writeBin()}) may still do checking.
#'          }
#'          \item{\code{warn}}{Explicit checks will be made for
#'          out-of-bound values.
#'          If any are found, then a \code{warning()} will be issued.
#'          }
#'          \item{\code{error}}{Explicit checks will be made for
#'          out-of-bound values.
#'          If any are found, then a error will be raised.
#'          }
#'        }
#' 
#' @return Modified connection object
#' @examplesIf interactive()
#' # Open a connection and configure it so out-of-bounds values
#' # will cause a warning only.
#' con <- rawConnection(as.raw(1:8), "rb")
#' con <- set_bounds_check(con, bounds_check = "warn")
#' 
#' # This line attempts to read a value from the connection which
#' # is too large to store in a double precision floating point without
#' # loss of integer precision.
#' # Usually this would cause an error to be raised, but the 'bounds_check'
#' # option has been set to give a warning only.
#' read_uint64(con, n = 1, promote = "dbl")
#' 
#' close(con)
#' 
#' @family connection configuration functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set_bounds_check <- function(con, bounds_check = 'error') {
  stopifnot(bounds_check %in% c('ignore', 'warn', 'error'))
  attr(con, "bounds_check") <- bounds_check
  con
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Helper function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_bounds_check_method <- function(con, bounds_check) {
  bounds_check <- bounds_check %||% 
    attr(con, "bounds_check", exact = TRUE) %||%
    "error"
  stopifnot(bounds_check %in% c("ignore", "warn", "error"))
  bounds_check
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check bounds and report error
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
do_bounds_check <- function(x, bounds_check, lo, hi, lo_str = as.character(lo), hi_str = as.character(hi)) {
  if (bounds_check == 'ignore') return();
  
  bad_vals <- NULL
  if (is.infinite(lo)) {
    bad_vals <- x[x > hi]
  } else if (is.infinite(hi)) {
    bad_vals <- x[x < lo]
  } else {
    bad_vals <- x[x < lo | x > hi]
  }
  
  
  if (length(bad_vals) > 0) {
    message <- sprintf("Out of bounds [%s, %s] : %s", lo_str, hi_str, paste(as.character(bad_vals), collapse = ", "))
    if (bounds_check == "warn") {
      warning(message)
    } else {
      stop(message)
    }
  }
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Set EOF (End-of-file) handling for this connection
#' 
#' When the end-of-file is reached and values are requested from the connection,
#' how should a \code{read} call check and react?
#' 
#' Note: R's \code{readBin()} does not necessarily react when the end-of-file 
#' is reached, and in many situations all that will happen is that fewer
#' data values will be returned than what was requested.
#' 
#' By setting this option on the connection, work is done to check the count
#' of returned values after every call to try and detect when the
#' end-of-file has been reached. 
#' 
#' @inheritParams read_uint8
#' @param eof_check Default EOF checking behaviour. One of: 'ignore', 'warn', 'error'
#'        Default: 'error'. 
#'        \describe{
#'          \item{\code{ignore}}{No explicit checks will be made for
#'          EOF.
#'          The underlying R functions (e.g. \code{readBin()}, \code{writeBin()}) may still do checking.
#'          }
#'          \item{\code{warn}}{Explicit checks will be made for
#'          reading data at EOF.
#'          If this occurs, then a \code{warning()} will be issued.
#'          }
#'          \item{\code{error}}{Explicit checks will be made for
#'          reading data at EOF.
#'          If any are found, then a error will be raised.
#'          }
#'        }
#' 
#' @return Modified connection object
#' @examplesIf interactive()
#' # Open a connection and configure it so reading past the end-of-file 
#' # ignored, and operations simply return fewer values than requested
#' con <- rawConnection(as.raw(1:8), "rb")
#' con <- set_eof_check(con, eof_check = "ignore")
#' 
#' # There are only 8 bytes in the connection. 
#' # Attempting to read 12 bytes will reach the end of the file.
#' # Because "eof_check" has been set to "ignore", there will just be
#' # silent truncation of the data
#' read_uint8(con, n = 12)
#' 
#' # The connection can be configured to raise an error or warning
#' # when EOF is reached
#' con <- set_eof_check(con, eof_check = "warn")
#' read_uint8(con, n = 12)
#' 
#' close(con)
#' 
#' @family connection configuration functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set_eof_check <- function(con, eof_check = 'error') {
  stopifnot(eof_check %in% c('ignore', 'warn', 'error'))
  attr(con, "eof_check") <- eof_check
  con
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Helper function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_eof_check_method <- function(con, eof_check) {
  eof_check <- eof_check %||% 
    attr(con, "eof_check", exact = TRUE) %||%
    "error"
  stopifnot(eof_check %in% c("ignore", "warn", "error"))
  eof_check
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
  
  msg <- sprintf("EOF reached. Requested %i items, only got %i", as.integer(n_requested), as.integer(n_read))
  
  if (method == 'warn') {
    warning(msg)
  } else {
    stop(msg)
  }
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Check for NAs in values before writing
#' 
#' For the majority of binary file formats, there is never the need to 
#' store or retrieve an \code{NA} value.  The default behaviour of this
#' package is to raise an error if any attempt is made to write an \code{NA}
#' to file.  Set this option to \code{"warn"} or \code{"ignore"} to modify this.
#' 
#' @inheritParams read_uint8
#' @param na_check Default NA checking behaviour. One of: 'ignore', 'warn', 'error'
#'        Default: 'error'. 
#'        This default may be over-ridden by specifying
#'        the \code{na_check} argument when calling individual functions.
#'        \describe{
#'          \item{\code{ignore}}{No explicit checks will be made for
#'          NA values
#'          The underlying R functions (e.g. \code{readBin()}, \code{writeBin()}) may still do checking.
#'          }
#'          \item{\code{warn}}{Explicit checks will be made for
#'          NA values before writing.
#'          If any NAs are present, then a \code{warning()} will be issued.
#'          }
#'          \item{\code{error}}{Explicit checks will be made for
#'          NA values before writing.
#'          If any NAs are present, then an error will be raised.
#'          }
#'        }
#' 
#' @return Modified connection object
#' 
#' @examplesIf interactive()
#' # Open a connection and configure it so any attempt to write an NA
#' # value will cause a warning only (the default behaviour is to raise an error)
#' con <- rawConnection(raw(), "wb")
#' con <- set_na_check(con, na_check = "warn")
#' 
#' # This write should work without issues
#' write_dbl(con, c(1, 2, 3, 4))
#' 
#' # This write will cause a warning
#' write_dbl(con, c(1, 2, 3, NA))
#' 
#' close(con)
#' @family connection configuration functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set_na_check <- function(con, na_check) {
  stopifnot(na_check %in% c('ignore', 'warn', 'error'))
  attr(con, "na_check") <- na_check
  con
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Helper function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_na_check_method <- function(con, na_check) {
  na_check <- na_check %||% 
    attr(con, "na_check", exact = TRUE) %||%
    "error"
  stopifnot(na_check %in% c("ignore", "warn", "error"))
  na_check
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







