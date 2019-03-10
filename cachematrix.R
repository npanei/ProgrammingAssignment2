# R Programming - Assignment 2
# vnpanei

# these functions can be used together to invert a matrix
# and to cache the result in order to avoid excessive computation

# here, x is an invertible matrix
# this function returns a set of functions used by cacheSolve
# in order to check whether there is a cached inverse of x
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# in cacheSolve, x is the output of "makeCacheMatrix"
# this function returns the cached inverse if there is one
# or, if not, it computes the inverts x and returns and caches the result
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}