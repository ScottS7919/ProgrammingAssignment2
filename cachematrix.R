## Put comments here that give an overall description of what your
## functions do
## this function will cache the value of the computation and before running that compuation again it will check the 
## stored cache and if the values haven't changed it will use the cached value.

## Write a short comment describing this function
## This function creates a special matrix object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## this function computes the inverse of the special matrix returend by function above and if its already been calculated 
## then it should use the cached value, if not then compute the inverse. 

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
