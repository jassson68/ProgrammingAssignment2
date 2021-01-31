## Put comments here that give an overall description of what your
## functions do

## cachesolve access this function 

makeCacheMatrix <- function(x = matrix()) {
    p <- NULL
    set <- function(y) {
      x <<- y
      p <<- NULL
    }
    get <- function() x
    setInverse <- function(Inverse) p <<- Inverse
    getInverse <- function() p
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cachesolve function to compute matrix inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  p <- x$getInverse()
  if(!is.null(p)) {
    message("retrieving cached data") ## get cache data
    return(p)  ## return matrix value
  }
  data_comp <- x$get()
  p <- solve(data_comp, ...)
  x$setInverse(p)
  p
}

A<-matrix(c(1,2,3,4),2,2) ## matrix vector
compute<-makeCacheMatrix(A)
cacheSolve(compute)

