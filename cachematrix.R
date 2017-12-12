# These functions work together to create a matrix and cache its inverse.
# cacheSolve takes make_matrix as an argument in this form: cacheSolve(makeCacheMatrix(matrix(1:4,2)))
# A second example of possible input is: cacheSolve(makeCacheMatrix(matrix(c(3,1,2,1),nrow=2,ncol=2)))


# The following line initializes my_inverse, which must be reset to NULL each time a new matrix is
# input.
my_inverse <- NULL

makeCacheMatrix <- function(x = matrix()){
  # This function takes a matrix and caches its inverse

  set <- function(y) {
    x <<- y
    my_inverse <<- NULL
  }#End of set function
  
  get <- function() x
  set_inverse <- function(my_inverse) my_inverse <<- solve(x) # Calculate inverse
  get_inverse <- function() my_inverse
  
  list (set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
  
}# End of make_matrix function

cacheSolve <- function(x, ...){
  #This function returns the matrix inverse from the cache if it has already been calculated
  
  my_inverse <- x$get_inverse()

  if (!is.null(my_inverse)) {
    print("Getting cached inverse of your matrix:")
    return(my_inverse)
  }#End of if
  
  print("Solving the inverse of your matrix:")
  my_matrix <- x$get()
  my_inverse <- solve(my_matrix, ...)
  x$set_inverse(my_inverse)
  my_inverse
}#End of cache_matrix