## This function is able to cache potentialy time-consuming 
## computations to be looked up when needed rather than 
## recomputed. In this case, the functions cache and call the
## inverse of a matrix. makeCacheMatrix creates a special matrix 
## object and cacheSovle computes the inverse of the special
## matrix returned in the first. If the inverse has already been 
## calcultaed (and th ematrix has not changed), cacheSolve
## retries the inverse from the cache.

## This function creates a special matrix object that can cache 
## its inverse.

makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the matrix object returned 
## by the above function makeMacheMatrix. If the 

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
