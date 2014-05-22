## Functions makeCacheMAtrix and cacheSolve provide functionality to create 
## a special matrix that can cache its inverse.

## makeCacheMatrix creates a special matrix that can cache its inverse.
## Operations supported on this special matrix are:
## set          - initializes/sets the matrix
## get          - fetches the matrix
## setinverse   - stores the inverse in cache
## getinverse   - fetches the inverse from cache
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                # invalidate cache when 
                # matrix is updated
                inv <<- NULL
        }
        get <- function() {
                x
        }
        setinverse <- function(inverse) {
                inv <<- inverse
        }
        getinverse <- function() {
                inv
        }
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## cacheSolve returns inverse of the special matrix created by makeCacheMatrix.
## It first checks the cache.  If the inverse is not present there, it computes
## the inverse and caches it.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting data from cache")
                return(inv)
        }
        o_matrix <- x$get()
        inv <- solve(o_matrix, ...)
        x$setinverse(inv)
        inv
}
