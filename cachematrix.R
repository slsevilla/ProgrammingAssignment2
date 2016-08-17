## The function looks to take the inverse of a matrix. If the inverse has already been taken, it will pull the cached matrix.

## This portion of the function will make a matrix and take the inverse of the function.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will check to determine if the inverse matrix of the above function has been calculated, if it has, and
## the matrix has not changed, it will retrieve the information from the cache. If it hasn't, it will calculate it.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("Get Cached Data")
    return(inv)
  }
  matx <- x$get()
  inv <- solve(matx, ...)
  x$setinverse(inv)
  inv
}