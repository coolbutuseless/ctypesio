

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
#' Swizzle a linear vector of data into an R array
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
    nms[1] == 'planes'
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
#' Swizzle an R array to a linear vector of data
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
#' mat <- matrix(c(
#'   'r0', 'g0', 'b0',   'r1', 'g1', 'b1',   'r2', 'g2', 'b2',   
#'   'r3', 'g3', 'b3',   'r4', 'g4', 'b4',   'r5', 'g5', 'b5'
#' ), nrow = 2, ncol = 9,  byrow = TRUE)
#' mat
#' aperm_array_to_vector(mat, dst = c('rows', 'cols', 'planes'))
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
  stopifnot(dst[3] == 'planes')
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity Check
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(exprs = {
    identical(sort(dst), c('cols', 'planes', 'rows'))
  })
  
  idx_order <- match(c("rows", "cols", "planes"), dst)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Do the actual permute
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  res <- aperm(x, perm = idx_order, resize = TRUE)
  
  as.vector(res)
}






if (FALSE) {
  
  # Linear data read from file. Structure is known to be packed RGB
  # pixels in row-major format.
  x <- vec <- c(
    'r0', 'g0', 'b0',   'r1', 'g1', 'b1',   'r2', 'g2', 'b2',   'r3', 'g3', 'b3', 
    'r4', 'g4', 'b4',   'r5', 'g5', 'b5',   'r6', 'g6', 'b6',   'r7', 'g7', 'b7', 
    'r8', 'g8', 'b8',   'r9', 'g9', 'b9',   'ra', 'ga', 'ba',   'rb', 'gb', 'bb'
  )
  
  # Permute from linear to an R column-major format with 3 planes
  aperm_vector_to_array(vec, src = c(planes = 3, cols = 4, rows = 3))
  aperm_vector_to_array(vec, src = c(planes = 3, cols = 4, rows = 3), flipy = TRUE)

  # Ordering of elements in 'src' argument determine permutation ordering
  # The following will transpose the data
  aperm_vector_to_array(vec, src = c(planes = 3, rows = 4, cols = 3))
  
  
  
  
  arr <- x <- aperm_vector_to_array(vec, src = c(planes = 3, cols = 4, rows = 3))
  
  aperm_array_to_vector(x, dst = c('planes', 'cols', 'rows'))
  
  
  x <- c(
    'r0', 'g0', 'b0',   'r1', 'g1', 'b1',   'r2', 'g2', 'b2',   
    'r3', 'g3', 'b3',   'r4', 'g4', 'b4',   'r5', 'g5', 'b5'
  )
  aperm_vector_to_array(x, src = c(planes = 3, cols = 3, rows = 2))
  
  mat <- matrix(c(
    'r0', 'g0', 'b0',   'r1', 'g1', 'b1',   'r2', 'g2', 'b2',   
    'r3', 'g3', 'b3',   'r4', 'g4', 'b4',   'r5', 'g5', 'b5'
  ), nrow = 2, ncol = 9,  byrow = TRUE)
  mat
  aperm_array_to_vector(mat, dst = c('rows', 'cols', 'planes'))
  
}




if (FALSE) {
  
  
  flip_chunked_yumechi <- function(xs, size) {
    xs |>
      split(ceiling(seq_along(xs) / size)) |>
      purrr::map(rev) |>
      purrr::reduce(c)
  }
  
  
  
  flip_chunked_yumechi2 <- function(xs, size) {
    xs |>
      split(ceiling(seq_along(xs) / size)) |>
      lapply(rev) |> 
      unlist(recursive = FALSE, use.names = FALSE)
  }
  
  
  vec <- c(1, 2, 3, 4, 5, 6, 7, 8)
  all.equal(flip_endian(vec, 1), vec)
  all.equal(flip_endian(vec, 2), c(2, 1, 4, 3, 6, 5, 8, 7))
  all.equal(flip_endian(vec, 4), c(4, 3, 2, 1, 8, 7, 6, 5))
  
  
  all.equal(flip_chunked_yumechi(vec, 1), vec)
  all.equal(flip_chunked_yumechi(vec, 2), c(2, 1, 4, 3, 6, 5, 8, 7))
  all.equal(flip_chunked_yumechi(vec, 4), c(4, 3, 2, 1, 8, 7, 6, 5))
  
  all.equal(flip_chunked_yumechi2(vec, 1), vec)
  all.equal(flip_chunked_yumechi2(vec, 2), c(2, 1, 4, 3, 6, 5, 8, 7))
  all.equal(flip_chunked_yumechi2(vec, 4), c(4, 3, 2, 1, 8, 7, 6, 5))
  
  
  vec <- integer(1000)
  bench::mark(
    flip_endian(vec, 4),
    flip_chunked_yumechi(vec, 4),
    flip_chunked_yumechi2(vec, 4),
    flip_chunked_yumechi3(vec, 4),
    flip_endian(vec, 250),
    flip_chunked_yumechi(vec, 250),
    flip_chunked_yumechi2(vec, 250),
    flip_chunked_yumechi3(vec, 250)
  )
  
  
  
}
