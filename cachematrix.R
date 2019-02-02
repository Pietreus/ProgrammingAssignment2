## These functions create a matrix that can cache it's inverse, so that it
## doesn't need to be recalculated unnecessarily

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ##works like the function for the vector
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

## This function computes the inverse of the special matrix

cacheSolve <- function(x, ...) {
    ##didn't have to change much here either
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv

}
