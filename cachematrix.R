## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix returns a list of functions that can be used to 
## set and retrieve the cached matrix (set/get) and its 
## inverse (setinv/getinv)

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(is.null(inv)) {
        message("recomputing")
        data <- x$get()
        inv <- solve(data, diag(nrow(data)), ...)
        x$setinv(inv)
    }
    inv
}
