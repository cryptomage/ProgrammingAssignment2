## Caching inverse of matrix Functions
## These functions are used to save the cost in computing the
## inverse of a matrix

## This function creates a special "matrix" 
## object that can cache its inverse.
##
## Matrix elements should be a numeric type 
## for the solve() function
## @author: James Patrick Acang
##
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL # cached inverse
    
    # set new matrix value
    set <- function(y) {
        x <<- y
        m <<- NULL  # reset inverse value
    }
    
    # get matrix
    get <- function() x
    
    # set inverse
    setinverse <- function(inverse) m <<- inverse
    
    # get inverse
    getinverse <- function() m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse 
## of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse 
## has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## 
cacheSolve <- function(x, ...) {
    ## get the cached inverse
    m <- x$getinverse()
    # if available return inverse
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # otherwise compute inverse
    # get matrix
    data <- x$get()
    # computer inverse
    m <- solve(data, ...)
    # cache inverse
    x$setinverse(m)
    # return inverse
    m
}
