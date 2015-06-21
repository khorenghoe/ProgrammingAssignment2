## These functions are used to cache the inverse of a matrix to make the processing faster

## This following function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function () x
  setInv <- function (solve) inverse <<- solve
  getInv <- function () inverse
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## This following function computes the inverse of the cached data from "makeCacheMatrix"
## above, if the inverse has been calculated.

cacheSolve <- function(x, ...) {
  inverse <- x$getInv()
  if(!is.null(inverse)) { #aka, if there is a value, not NA,
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInv(inverse)
  inverse
  ## Return a matrix that is the inverse of 'x'
}
