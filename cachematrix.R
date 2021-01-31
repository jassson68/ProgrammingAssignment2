## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  p <- x$getInverse()
  if(!is.null(p)) {
    message("retrieving cached data")
    return(p)
  }
  data_comp <- x$get()
  p <- solve(data_comp, ...)
  x$setInverse(p)
  p
}

A<-matrix(c(1,2,3,4),2,2)
compute<-makeCacheMatrix(A)
cacheSolve(compute)

