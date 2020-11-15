##This set of functions is used to cache the value of an inverted matrix to save time on potentially repeated calculations.

## makeCacheMatrix is a function that takes a matrix argument to be modified.

makeCacheMatrix <- function(x = matrix()){
    inv <- NULL
    set <- function(y){
        x <<- y 
        inv <<- NULL
    }
    get <- function() {x}
    setInverse <- function(inverse) {inv <<- inverse}
    getInverse <- function() {inv}
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...){
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse is already calculated (and the matrix hasn't changed), then cacheSolve should retrieve the inverse via cache. 
}
