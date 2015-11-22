## This is a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## First you set the matrix, then you get the matrix, 
## then you set the inverse, then you get the inverse!


makeCacheMatrix <- function(x = matrix()) {
        m = NULL
        set = function(y) {
                x <<- y
                m <<- NULL
        }
        get = function() x
        setinv = function(inverse) m <<- inverse 
        getinv = function() m
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m = x$getinv()
        if (!is.null(m)){
                message("getting cached data")
                return(m)
        }
        mydata = x$get()
        m = solve(mydata, ...)
        x$setinv(m)
        m
}
