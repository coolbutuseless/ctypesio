% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read-float.R
\name{read_f64}
\alias{read_f64}
\alias{read_f32}
\alias{read_f16}
\alias{read_bfloat}
\alias{read_dbl}
\alias{read_float}
\alias{read_half}
\title{Read floating point values from a connection}
\usage{
read_f64(con, n = 1, endian = NULL)

read_f32(con, n = 1, endian = NULL)

read_f16(con, n = 1, endian = NULL)

read_bfloat(con, n = 1, endian = NULL)

read_dbl(con, n = 1, endian = NULL)

read_float(con, n = 1, endian = NULL)

read_half(con, n = 1, endian = NULL)
}
\arguments{
\item{con}{Connection object created with \code{file()}, \code{url()}, 
\code{rawConnection()} or any of the other many connection creation
functions.}

\item{n}{Number of elements to read. Default: 1}

\item{endian}{Ordering of bytes within the file when reading multi-byte values.
Possible values: 'big' or 'little'.  
Default: NULL indicates that
endian option should be retrieved from the connection object if possible
(where the user has used \code{\link{set_endian}()}) or otherwise 
will be set to \code{"little"}}
}
\value{
vector of double precision floating point numbers
}
\description{
Read floating point numbers into a standard R vector of doubles
}
\details{
\describe{
  \item{double precision}{8 byte floating point numbers. \code{read_f64()} 
        also available as \code{read_dbl()}}
  \item{single precision}{4 byte floating point numbers. \code{read_f32()} 
        also available as \code{read_float()}}
  \item{half precision}{2 byte floating point numbers. \code{read_f16()} 
        also available as \code{read_half()}.  Consists of 1 sign bit, 
        5 bits for exponent and 10 bits for fraction.}
  \item{bfloat}{2 byte floating point numbers in the bfloat format  \code{read_bfloat()}.
        Consits of 1 sign bit, 8 bits fo exponent and 7 bits for fraction.} 
}
}
\examples{
# Raw vector with 16 bytes (128 bits) of dummy data
data <- as.raw(1:16)
con <- rawConnection(data, 'rb')
read_f64(con, n = 1) # Read a 64-bit double-precision number
read_f16(con, n = 4) # Read 4 x 16-bit half-precision number
close(con)

}
\seealso{
Other data input functions: 
\code{\link{read_hex}()},
\code{\link{read_raw}()},
\code{\link{read_str}()},
\code{\link{read_uint8}()},
\code{\link{scan_dbl}()}
}
\concept{data input functions}
