## This creates a special kind of matrix, that can cache its inverse and a special solve 
## function to either create the inverse or use the cached result

## makes a special matrix, that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## this returns the inverse of 'x', only computing and caching it, 
## if the result wasn't already cached

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
          message("loading from cache")
          return(i)
        }
        matr <- x$get()
        i <- solve(matr, ...)
        x$setinverse(i)
        i
}