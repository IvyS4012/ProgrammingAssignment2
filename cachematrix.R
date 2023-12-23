## makeCacheMatrix function has four steps to set a new matrix, get the current matrix data, set its calculated inverse matrix and get the matrix you cached
## cacheSolve function is to receive the special matrix object from the makeCacheMatrix function and compute its inverse matrix

## Function to create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed)
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  
  if (!is.null(inverse)) {
    message("Getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setInverse(inverseMatrix)
  inverseMatrix
}
