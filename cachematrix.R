## A pair of functions that cache the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL                             ## initialize inv as NULL; will hold value of matrix inverse 
    set <- function(y) {                 
        x <<- y                             ## value of matrix in parent environment
        inv <<- NULL                        ## new matrix; reset inv to NULL
    }
    get <- function() x                    
       setinverse <- function(inverse) inv <<- inverse  
    getinverse <- function() inv                    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

## cacheSolve will retrieve the inverse from the cache
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
     inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
