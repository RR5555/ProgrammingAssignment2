## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    ## See bottom of the file for a description of the <<- operator
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_mat_inv <- function(mat_inv) m <<- mat_inv
  get_mat_inv <- function() m
  list(set = set, get = get,
       set_mat_inv = set_mat_inv,
       get_mat_inv = get_mat_inv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$get_mat_inv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_mat_inv(m)
  m
}

## <<- : It's the 'superassignment' operator.  It does the assignment in the
## enclosing environment. That is, starting with the enclosing frame, it
## works its way up towards the global environment until it finds a
## variable called x, and then assigns to it.   If it never finds
## an existing x it creates one in the global environment.
