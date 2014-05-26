## This script computes the cached inverse of a given matrix using 2 functions:
## makeCacheMatrix and cacheSolve

## This function creates a vector of 4 functions : setting the value of the matrix, getting this value, setting the value of the inverse and getting this value#

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This funtion computes the inverse of the cached matrix returned from makeCacheMatrix, i.e. if the inverse matrix has been already computed, it will be directly 
## displayed without recomputing

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m 
}
