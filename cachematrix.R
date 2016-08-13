## The following functions cache the inverse of a matrix
## in order to avoid repeatedly costly computations.

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        cache_m <- NULL
        set <- function(y){
        	    x <<- y
        	    cache_m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) cache_m <<- inverse
        getinverse <- function() cache_m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cache_m <- x$getinverse()
        if(!is.null(cache_m)) {
        	    message("getting cached data")
        	    return(cache_m)
        }
        data <- x$get()
        cache_m <- solve(data, ...)
        x$setinverse(cache_m)
        cache_m
}
