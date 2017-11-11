## Pair of functions that cache the inverse of a matrix.

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  setMatrix<-function(y){
    x<<-y
    inv<<-NULL
  }
  getMatrix<-function() x
  setinverse<-function(inverse) inv<<-inverse
  getinverse<-function() inv
  list(setMatrix = setMatrix, getMatrix = getMatrix, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("Getting cached data!")
    return(inv)
  }
  data<-x$getMatrix()
  inv<-solve(data)
  x$setinverse(inv)
  return(inv)
}
