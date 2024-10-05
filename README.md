
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ctypesio

<!-- badges: start -->

![](https://img.shields.io/badge/cool-useless-green.svg)
[![R-CMD-check](https://github.com/coolbutuseless/ctypesio/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/coolbutuseless/ctypesio/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`{ctypesio}` provides functions for reading/writing standard C types
from connections.

These operations are useful when parsing binary file types.

Key features:

- Persitant defaults per-connection for:
  - Endianness
  - Bounds checking
  - EOF behavoiur
- Support for integer types which contain values outside the range of
  R’s integer type (which is 32-bit signed)
- Support for 16-bit floating point numbers using IEEE half-precition
  and
  [bfloat](https://en.wikipedia.org/wiki/Bfloat16_floating-point_format)
  variants.

## What’s in the box

- Floating point numbers
  - `read_f64()`, `write_f64()` etc. read/write double-, single- and
    half-precision floats
  - `read_bfloat()` read 16-bit
    [bfloat](https://en.wikipedia.org/wiki/Bfloat16_floating-point_format)
    numbers
- Integers
  - `read_uint8()`, `write_uint8()` etc for reading signed/unsigned 8,
    16, 32, 64-bit integers
  - For types which don’t fit in a standard R integer (e.g. `uint64`)
    there are options to promote this to double, hexadecimal or read as
    a raw vector
- Other
  - `read_str()`, `read_str_raw()`, `read_utf8()`, `read_utf8_raw()`,
    `write_utf8()`, `write_utf8_raw()`
  - `read_raw()`, `write_raw()` Read/write raw bytes
  - `read_hex()`, `write_hex()` Read/write bytes as hex strings
- Connection configuration
  - `set_endian()` set the default *endianness* for all operations on
    this connection. Can be over-ridden within each function by
    specifying `endian` argument. If not specified on the connectino or
    in function argument, this defaults to `little` on all platforms.
  - `set_bounds_check()` set handling of out-of-bounds checks when
    writing integer/float values.
    - `ignore` do nothing and let underlying `writeBin()` truncate data
      however it wants
    - `warn` warn when data is outside the bounds for this type. E.g.
      trying to write an integer value of `500` with `write_uint8()`
      will trigger this warning.
    - `error` same as `warn` except an actual error is raised.
  - `set_eof_check()` check if the number of items returned does not
    match the number requested. This would indicate the connection
    reached EOF during data reading. Options are to `ignore`, `warn` or
    `error`

| bits | integer | floating point |
|----|----|----|
| 8 | Read/write. Signed/unsigned. | NA |
| 16 | Read/write. Signed/unsigned. | Yes. Half-precision IEEE and bfloat |
| 32 | Read/write. Signed/unsigned. (32bit unsigned integer promoted to double) | Yes |
| 64 | Read/write. Signed/unsigned. with double, hex and raw vectors | Yes |

## Installation

You can install from
[GitHub](https://github.com/coolbutuseless/ctypesio) with:

``` r
# install.packages('remotes')
remotes::install_github('coolbutuseless/ctypesio')
```

## Example: Parsing a JPEG file

JPEG files are a sequence of chunks (identified by **marker** bytes),
followed by a length and then a sequence of bytes.

These chunks continue until the `Start of Scan` chunk which contains the
compressed data continuing until the length of the file.

``` r
library(ctypesio)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read the old logo from the 'jpeg' package
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
jpeg_file <- system.file("img", "Rlogo.jpg", package="jpeg")
jpeg <- jpeg::readJPEG(jpeg_file)
plot(as.raster(jpeg))
```

<img src="man/figures/README-jpeg-1.png" width="100%" />

``` r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# What does the raw data look like?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat <- readBin(jpeg_file, raw(), n = file.size(jpeg_file)) 
head(dat, 100)
```

    #>   [1] ff d8 ff e0 00 10 4a 46 49 46 00 01 01 01 01 2c 01 2c 00 00 ff e1 00 80 45
    #>  [26] 78 69 66 00 00 4d 4d 00 2a 00 00 00 08 00 05 01 12 00 03 00 00 00 01 00 01
    #>  [51] 00 00 01 1a 00 05 00 00 00 01 00 00 00 4a 01 1b 00 05 00 00 00 01 00 00 00
    #>  [76] 52 01 28 00 03 00 00 00 01 00 02 00 00 87 69 00 04 00 00 00 01 00 00 00 5a

#### Parsing the JPEG file

- Open the connection to the file
- Set the default endianness for all operations to *big endian*
- Confirm the file contains the SOI marker ‘ffd8’
- Read the first marker and decode some information about the image.

JPEG files mostly contain *big-endian* encoded markers and integers, so
we’ll set this as the default on the connection (so we don’t have to
specify it for every read operation).

``` r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Open a connection, and tag the connection such that 
# values are read in **big endian** by default.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
jpeg_file <- system.file("img", "Rlogo.jpg", package="jpeg")
con <- file(jpeg_file, 'rb') |>
  set_endian('big')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read the first 2 bytes as HEX
# For JPEG files, this should be the "Start of Image (SOI)" marker "ffd8"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
soi <- read_hex(con, n = 1, size = 2) 
stopifnot(soi == 'ffd8')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Next marker should be start of JFIF.  Marker hex = 'ffe0'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
marker <- read_hex(con, n = 1, size = 2)
stopifnot(marker == 'ffe0')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data expected in this chunk
#  - length of this chunk    (16 bit unsigned int)
#  - the word 'JFIF'         (nul-terminated string)
#  - Major version number    ( 8 bit unsigned int)
#  - Minor version number    ( 8 bit unsigned int)
#  - Density units:          ( 8 bit unsigned int)
#       0 - no unit
#       1 - pixels-per-inch
#       2 - pixels-per-cm
#  - Xdensity                (16 bit unsigned int)
#  - Ydensity                (16 bit unsigned int)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
list(
  length   = read_uint16(con),
  jfif     = read_str(con),
  version  = read_uint8(con, 2),
  units    = c('', 'PPI', 'PPcm')[read_uint8(con) + 1],
  density = read_uint16(con, 2)
)
```

    #> $length
    #> [1] 16
    #> 
    #> $jfif
    #> [1] "JFIF"
    #> 
    #> $version
    #> [1] 1 1
    #> 
    #> $units
    #> [1] "PPI"
    #> 
    #> $density
    #> [1] 300 300

This information reveals that the JPEG is:

- a JFIF image
- Version 1.1
- with pixel density 300x300 dpi
