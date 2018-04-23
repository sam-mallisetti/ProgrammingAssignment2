## The functions below create an Inverse of a given matrix. 
## It is assumed that the given matrix is invertible.
## When calling the makeCachMatrix you need to pass an invertible matrix.
## Then call the cacheSolve funtion by passing the above result as argument.
## The inverse of the matrix will be returned (either from Cache or calculated)

## The makeCacheMatrix function has function objects to retrieve
## values from Cache. It stores the value of the inverse matrix in cache.

makeCacheMatrix <- function(x = matrix()) {
  x_inverse <- NULL
  set <- function(y) {
    x <<- y
    x_inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(n) x_inverse <<- n
  get_inverse <- function() x_inverse
  list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


## The cacheSolve function retrieves the inverse matrix from Cache. 
## If this is not NULL it returns that matrix. No other work is needed.
## If this matrix in Cache is NULL, then Inverse matrix is created and
## stored in Cache for future retrieval.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  n <- x$get_inverse()
  if(!is.null(n)) {
    message("getting cached inverse matrix...")
    return(n)
  }
  data <- x$get()
  n <- solve(data)
  x$set_inverse(n)
  n
  
}
