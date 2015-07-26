## These functions allow for the creation of matrices and quickly 
## storing/accessing their inverses.

## Creates a special "matrix", actually a list that contains getter
## and setter functions for both the matrix data and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() {x}
  setInverse <- function(i) {inverse <<- i}
  getInverse <- function() {inverse}
  list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}


## Given a special matrix, returns its inverse.
## The inverse returned is taken from the cache (if already exists)
## or computed and then cached (if it doesn't yet).

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}
