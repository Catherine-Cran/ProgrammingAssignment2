## The function enables caching of the inverse of a matrix to save on compute costs
## How to use: Create an invertible matrix to pass to makeCacheMatrix. Assign the 
## result of makeCacheMatrix to a new variable and pass the new variable to cacheSolve.


## This function creates a special "matrix" object that can cache its inverse. 
## The list of functions returned enable cacheSolve to calculate the inverse of 
## the matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setInv <- function(solve) i <<- solve
    getInv <- function() i
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
## not changed), then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getInv()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInv(i)
    i
}
