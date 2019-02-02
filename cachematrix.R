## These functions create a matrix that can cache it's inverse, so that it
## doesn't need to be recalculated unnecessarily

## This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    ##works like the function for the vector
    inv <- NULL
    #changes the value of the matrix and deletes the cached inverse
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    #sets the inverse of the matrix object, only to be called by cacheSolve
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    #list of the functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}

## This function computes the inverse of the special matrix
cacheSolve <- function(x, ...) {
    ##didn't have to change much here either
    inv <- x$getinv()
    #return cached inverse if available
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    #calculate it if not
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv

}
