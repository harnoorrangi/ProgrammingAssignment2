## Cacheing the Inverse of a matrix

## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## cacheSolve function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
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
  ## Return a matrix that is the inverse of 'x'
##TEST 
##> a <- diag(2,5)
##> a
##[,1] [,2] [,3] [,4] [,5]
##[1,]    2    0    0    0    0
##[2,]    0    2    0    0    0
##[3,]    0    0    2    0    0
##[4,]    0    0    0    2    0
##[5,]    0    0    0    0    2
##> m<-makeCacheMatrix(a)
##> cacheSolve(m)
##[,1] [,2] [,3] [,4] [,5]
##[1,]  0.5  0.0  0.0  0.0  0.0
##[2,]  0.0  0.5  0.0  0.0  0.0
##[3,]  0.0  0.0  0.5  0.0  0.0
##[4,]  0.0  0.0  0.0  0.5  0.0
##[5,]  0.0  0.0  0.0  0.0  0.5
 


