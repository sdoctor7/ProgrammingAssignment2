## These functions take a square matrix and compute and cache its inverse. The
## inverse matrix can then be retrieved from the cache.

## makeCacheMatrix creates a list of functions that set the matrix, retrieve the
## matrix, set the inverse, and retrieve the cached inverse, respectively.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inv) inv <<- inv
        getInv <- function() inv
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve takes a matrix and either returns a cached inverse or computes
## the inverse and returns that.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
#         print(matrix)
        inv <- solve(matrix, ...)
        x$setInv(inv)
        inv
}