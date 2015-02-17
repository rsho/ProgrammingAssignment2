
## This function creates a special "vector", which is really a list containing a function to:
## set the value of the matrix, get the value of the matrix,
## compute the inverse matrix, and get the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    m.inv <- NULL
    ## makes place to contain matrix m in the makeCacheMatrix function environment
    set <- function(y) {
        x <<- y
        ## assigns the matrix passed as an argument when the function makeCacheMatrix was called,
        ## in the makeCacheMatrix function environment
        m.inv <<- NULL
        ## (re-)sets m in the makeCacheMatrix function environment to NULL
        ## because the matrix has been changed (by calling "set")
        ## and a new inverse has not yet been calculated by calling "setinverse"
    }
    get <- function() x
    setinverse <- function(solve) m.inv <<- solve ##(re-)compute matrix m
    getinverse <- function() m.inv
    list(set = set, get = get,setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the matrix passed as an argument,
## unless this already exists in cache,
## in which case the cached matrix is returned (with a message stating so)
cacheSolve <- function(x, ...) {
    m.inv <- x$getinverse()
    ## get m.inv stored in list from makeCacheMatrix function
    if(!is.null(m.inv)) { 
    ## if m.inv already contains a value, return that......
      message("getting cached data")
      return(m.inv)
    }
    data <- x$get()
    ## ...and if not, get the matrix from the makeCacheMatrix.....
    m.inv <- solve(data, ...)
    x$setinverse(m.inv)
    ## ...inverse it.....
    m.inv
    ##-- and return the inversed matrix.
}
