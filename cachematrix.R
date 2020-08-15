## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
                g <- NULL
                set <- function(y) {
                        x <<- y
                        cache <<- NULL
                }
                get <- function() x
                setinverse <- function(inverse) g <<- inverse
                getinverse <- function() g
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
        }


## This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         g <- x$getinverse()
        if(!is.null(g)) {
                message("getting cached data")
                return(g)
        }
        data <- x$get()
        g <- solve(data, ...)
        x$setinverse(g)
        g
}
