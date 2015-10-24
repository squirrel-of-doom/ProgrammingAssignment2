## Caching matrix inverter.
## makeCacheMatrix constructs a caching object that holds the matrix
## and its inverse.
## cacheSolve performs the actual solving, if necessary, and updates
## the cache.


## Returns a list of functions ("object") that can be used to 
## set and retrieve the cached matrix (set/get) and its 
## inverse (setinv/getinv).

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Returns the inverse of the matrix stored in x. If not cached,
## it is computed.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(is.null(inv)) {
        message("recomputing")
        data <- x$get()
        # supply identity to solve to get inverse
        inv <- solve(data, diag(nrow(data)), ...)
        x$setinv(inv)
    }
    inv
}
