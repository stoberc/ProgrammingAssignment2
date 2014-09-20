# These functions create a matrix object that is able to cache its inverse.
# makeCacheMatrix is the constructor for the object and cacheSolve returns
# the inverse, managing and utilizing a cached value.

# created by Christian Stober on 9/18/14 for R Programming coursera course

# returns a list of four functions used to manage the matrix
# set - sets new data
# get - returns the matrix
# setinverse - establishes a cached inverse value
# getinverse - accesses cached inverse value
makeCacheMatrix <- function(x = matrix()) {
    
    # cached value - NULL if not available
    i <- NULL
    
    # modify the matrix, which clears the cache
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # retrieve the matrix
    get <- function() x
    
    # set the cached inverse
    setinverse <- function(inverse) i <<- inverse

    # retrieve the cached inverse
    getinverse <- function() i
    
    # return the functions as a list
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}

# finds the inverse of x, simply retrieving a cached value if available
cacheSolve <- function(x, ...) {
    
    # retrieve and use the cached inverse if possible
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    # no cached value - retrieve the data, find the inverse, cache it, and return it
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i   
     
}

