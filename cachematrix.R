## Put comments here that give an overall description of what your
## functions do
# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly
# Analogous to the example given, makeCacheMatrix creates a list containing a 
# function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    set <- function(y) {
        x <<- y
        invm <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invm <<- inverse
    getinverse <- function() invm
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
# The cacheSolve function below Return a matrix that is the inverse of 'x' 
# It first checks to see if the inverse has already been calculated. 
# If so, it gets the result from the cache and skips the computation. 
# If not, it calculates the inverse and sets the value in the cache via
# setinverse function.
# Assumption is made that the the matrix is always invertible.
cacheSolve <- function(x, ...) {
    invm <- x$getinverse()
    if(!is.null(invm)) {
        message("getting cached data")
        return(invm)
    }
    data <- x$get()
    invm <- solve(data)
    x$setinverse(invm)
    invm
}

