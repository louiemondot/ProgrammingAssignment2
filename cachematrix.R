## Below are two functions that are used in tandem to create a special List 
## object representing a Matrix with a cache of its inverse. This allows the 
## ability to avoid recalculating the inverse of a Matrix if it had been 
## previously calculated.

## makeCacheMatrix is a function that creates a special List object that 
## represents a Matrix. This object contains the Matrix passed to it and can 
## also contain a cache of its own inverse if it had already been calculated 
## and set by the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve is a function that will return the inverse of a special Matrix 
## object created by the makeCacheMatrix function. The function will first check
## if the object's cache has the inverse already. If the cache is null, the 
## function then calculates the inverse, stores the inverse in the special 
## Matrix object's cache, and finally returns the inverse.  If the cache is not
## null, the function will return the inverse Matrix from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i    
}