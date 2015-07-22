## makeCacheMatrix and cacheSolve compute and cache the inverse of a matrix
## to avoid expensive computations

## accepts a matrix as its input
## creates 4 functions for setting the matrix, getting the matrix
## setting the inverse (to be used by its partner function)
## & getting the inverse once cached

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(inverse) {
        i <<- inverse
    }
    getinverse <- function() {
        i
    }
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse)
}


## if already cached, returns the inverse of the matrix
## if not cached, calculates the inverse and caches it
## in both cases, returns the inverse of the matrix

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
