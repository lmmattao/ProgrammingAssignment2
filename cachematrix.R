## The purpose of these functions is to optimize the repeated use of the inverse
## matrix calculation, doing the inverse matrix calculation once and storing the
## result, so that in later calls of the function it is not necessary to redo
## the calculation, as long as the original matrix is the same.


## This function takes a matrix as an argument, and returns a list with
## functions for manipulating this matrix and calculating the inverse matrix.
## Stores the results to avoid redoing calculations between function calls.

makeCacheMatrix <- function(x = matrix()) {
  force(x)
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) m <<- inverse
  
  getinverse <- function() m
  
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
  
}


## This function takes as an argument a list constructed by the makeCacheMatrix 
## function and returns the inverse matrix of the matrix stored with the call 
## makeCacheMatrix, checks whether the inverse matrix stored in cache can be 
## used to avoid repeating the matrix inversion calculation.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
