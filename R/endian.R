

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Flip the endianness of elements in a vector
#' 
#' This will create a new vector with the values reversed within the given 
#' block size. This can be used for changing the endianness of a set of values
#' 
#' @param x vector. Usually a raw vector, but can be any type
#' @param size block size. Usually a power of 2.
#' 
#' @return A vector of the same type as the initial vector with the values
#'         within each block reversed.
#' @examples
#' vec <- c(1, 2, 3, 4)
#' flip_endian(vec, 1)   # should give: c(1, 2, 3, 4)
#' flip_endian(vec, 2)   # should give: c(2, 1, 4, 3)
#' flip_endian(vec, 4)   # should give: c(4, 3, 2, 1)
#' @family data permutation functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
flip_endian <- function(x, size) {
  
  if (size <= 1 || length(x) == 0) return(x)
  
  if (length(x) %% size != 0) {
    stop("Input vector length must be a multiple of the 'size'")
  }
  
  dim(x) <- c(size, length(x)/size)
  
  x[] <- x[nrow(x):1,]
  
  as.vector(x)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Permute a linear vector of data into an R array
#' 
#' @param x vector
#' @param src Specification of source dimensions in the order of presentation
#'        in the source data. This must a named integer vector with 
#'        the names "planes", "rows", "cols" (and their corresponding sizes)
#'        in the order in which they occur in the data.  The first 
#'        named element must always be "planes".  Use \code{planes = 1} to
#'        indicate that this is matrix data.
#' @param flipy flip the array vertically. Default: FALSE
#' @param simplify_matrix If the resulting array only has a single plane, 
#'        should this be simplified to a matrix? Default: TRUE
#'
#' @return array or matrix
#' @examples
#' # Convert a vector of packed RGB data to an array with 3 planes
#' x <- c(
#'   'r0', 'g0', 'b0',   'r1', 'g1', 'b1',   'r2', 'g2', 'b2',   
#'   'r3', 'g3', 'b3',   'r4', 'g4', 'b4',   'r5', 'g5', 'b5'
#' )
#' aperm_vector_to_array(x, src = c(planes = 3, cols = 3, rows = 2))
#' @family data permutation functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aperm_vector_to_array <- function(x, src, flipy = FALSE, simplify_matrix = TRUE) {
  
  nms  <- names(src)
  dims <- as.integer(src)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity Check
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(exprs = {
    length(src) == 3
    !anyNA(src)
    all(nms %in% c('planes', 'rows', 'cols'))
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  len <- prod(dims)
  if (len != length(x)) {
    msg <- sprintf("Length of input (%i) does not match dimensions %s (length = %i)", 
                   length(x), deparse1(dims), len)
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add dims to original data and figure out aperm() idx permutation
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dim(x) <- dims
  idx_order <- match(c("rows", "cols", "planes"), nms)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Do the actual permute
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  res <- aperm(x, perm = idx_order, resize = TRUE)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Flip Y?
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (isTRUE(flipy)) {
    res <- res[nrow(res):1 , , ]
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Collapse to matrix if possible
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (simplify_matrix && dim(res)[3] == 1) {
    dim(res) <- dim(res)[1:2]
  }
  
  
  res
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Permute an R array to a linear vector of data
#' 
#' @param x array
#' @param dst Specification of destination dimensions in the order of presentation
#'        in the source data. Character vector which contains 3 strings:
#'        'planes', 'rows', 'cols'.  The order of these strings determines
#'        the order of output in the linear data. Currently, "planes" must always
#'        be the final element.
#' @param flipy flip the array vertically. Default: FALSE
#'
#' @return vector
#' @examples
#' arr <- array(rep(c('r', 'g', 'b'), each = 6), c(2, 3, 3))
#' arr
#' aperm_array_to_vector(arr, dst = c('planes', 'cols', 'rows'))
#' @family data permutation functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aperm_array_to_vector <- function(x, dst, flipy = FALSE) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Promote matrix to array
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.matrix(x)) {
    dim(x) <- c(dim(x), 1)
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.array(x)) {
    stop("Input 'x' must a matrix or array")
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity Check
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(exprs = {
    identical(sort(dst), c('cols', 'planes', 'rows'))
  })
  
  idx_order <- match(c("rows", "cols", "planes"), dst)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Flip Y?
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (isTRUE(flipy)) {
    x <- x[nrow(x):1 , , ]
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Do the actual permute
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  res <- aperm(x, perm = idx_order, resize = TRUE)
  
  as.vector(res)
}


