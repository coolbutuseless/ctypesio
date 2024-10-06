

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Swizzle the elements in a vector
#' 
#' This will create a new vector with the values reversed within the block size.
#' This is useful for changing the endianness of a set of values
#' 
#' @param x vector to swizzle. Usuually a raw vector, but can be any type
#' @param size swizzle size
#' 
#' @return vector of the same type as the initial vector with the values
#'           swizzled to flip endianness.
#' @examples
#' vec <- c(1, 2, 3, 4)
#' flip_endiann(vec, 2)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
flip_endiann <- function(x, size) {
  
  if (size <= 1) return(x)
  
  if (length(x) %% size != 0) {
    stop("Input vector length must be a multiple of the 'size'")
  }
  
  dim(x) <- c(size, length(x)/size)
  
  x[] <- x[nrow(x):1,]
  
  as.vector(x)
}

