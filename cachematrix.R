## makeCacheMatrix and cacheSolve calculate the inverse of a matrix and store 
# the result in the cache. If cachSolve is called again on the same matrix, 
# the result is retrieved from the cache in order to save calculation time.

# makeCacheMatrix takes a matrix x and creates a cached matrix object, which is 
# a list containing the original matrix x, the function setinverse to set the 
# inverse of matrix x, and the function getinverse to retrieve the inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(invrs) m <<- invrs
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


# cacheSolve takes a cached matrix object x as created by makeCacheMatrix, 
# checks if the inverse has already been calculated, and if not calculates the 
# inverse of x and stores it in the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}