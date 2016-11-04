## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## This function checks whether the value is already cached or not and depending on that it will either return the cached value or computes the inverse and sets the value in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}


