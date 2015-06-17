## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly.
## Use makeCacheMatrix to create a special "matrix" object that can cache its inverse,
## then use cacheSolve to compute the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

## Test the code by: 
## testMatrix <- matrix(rnorm(10*10), 10, 10)
## cacheX<-makeCacheMatrix(testMatrix)
## cacheSolve(cacheX)


## This function creates a special "matrix" object that can cache its inverse.
## x should be invertible
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) {
    i <<- inv
    }
  getInverse <- function() {
    i
    }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## x should be the 'specual matrix'
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
}



