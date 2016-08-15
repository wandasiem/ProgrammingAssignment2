## makeCacheMatrix creates an environment for storing a matrix 
## or its inverse so that the value persists through several 
## layers of environments.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function (y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve calculates the inverse of the matrix created with
## the above function.  However, it first checks to see if the 
## inverse has already been calculated.  If so, it get's the 
## inverse from the cache and skips the calculation.  Otherwise,
## it calculates the inverse of the matrix and sets the value of 
## of the inverse in the cache via the 'setInverse' function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
