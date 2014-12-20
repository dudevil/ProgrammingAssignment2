## The following code provides an API to cache the inverse of a matrix.
## Computing the inverse of a big matrix can potentially be time-comsuming
## so it makes sense to cache the result of this computation.
##
## In order to use this code, a thin wrapper around the matrix has to be created, 
## using the makeCacheMatrix function. 
## The inverse of the matrix can then be obtained by calling the cacheSolve function. 
## The first call to this function will compute the inverse of the CacheMatrix provided 
## as an argument, while subsequent calls will return the cached inverse matrix.
##
## Sample usage:
## > big.matrix <- matrix(rnorm(2048*2048), nrow=2048)
## > cache.matrix <- makeCacheMatrix(big.matrix)
## > system.time(inverse <- cacheSolve(cache.matrix))
## user  system elapsed 
## 25.591   0.000  24.891 
## > system.time(cached.inverse <- cacheSolve(cache.matrix))
## Returning cached inverse matrix
## user  system elapsed 
## 0       0       0 
## > all(inverse == cached.inverse)
## [1] TRUE
## > all.equal(cached.inverse %*% big.matrix, diag(2048))
## [1] TRUE

## Create a wrapper around the matrix class and provide a list of handy getter-setter methods.
## Args:
##  x: matrix, which inverse will be computed
## Returns:
##  A list of functions, containing get, set, getinverse, setinverse

makeCacheMatrix <- function(x = matrix()) {
  inverse.mat <- NULL
  set <- function(m) {
    x <<- y
    inverse.mat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse.mat <<- inverse
  getinverse <- function() inverse.mat
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## Return the inverse of the argument matrix. The inverse will be calculated during the first call.
## Subsequent calls with the same argument will return the cached inverse matrix
## Args:
##  x: an object, created using the makeCacheMatrix call
## Returns:
##  The inverse of the argument matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("Returning cached inverse matrix")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setinverse(inv)
  inv
}
