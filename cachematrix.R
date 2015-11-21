It is expensive to caculate a matrix inverse. The makeCacheMatrix and cacheSolve functions are used to
## cache the matrix inverse. 
## an example:
## > t <- makeCacheMatrix(matrix(c(1,2,3,4),nrow=2))
## > cacheSolve(t)
## [,1] [,2]
## [1,]   -2  1.5
#  [2,]    1 -0.5

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    mi <- matrix()
    set <- function(y) {
        x <<- y
        mi <<- matrix()
    }    
    get <- function() x
    setinverse <- function(inverse) mi <<- inverse
    getinverse <- function() mi
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mi <- x$getinverse()
    if(!identical(mi, matrix())) {
        message("getting cached inverse")
        return (mi)
    }
    m <- x$get()
    mi <- solve(m, ...)
    x$setinverse(mi)
    mi
}
