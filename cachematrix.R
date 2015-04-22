## This script is used for create an special type of matrix that caches
## its inverse

## Creates an special type of matrix
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


## Solves the inverse for a matrix created with the makeCacheMatrix function
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message('getting cache inverse')
        return(inv)
    }
    data <- x$get()
    inv <- solve(a = data, ...)
    x$setinv(inv)
    inv
}
