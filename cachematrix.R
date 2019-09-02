## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


##makeCacheMatrix is a function that returns a makeCacheMatrix object with a list
##containing 4 different functions, set(), get(), setinv(), and getinv(). There are the
##setters and getters, or otherwhise well-known mutator and accessor methods used in
##object-oriented programming. Afterwards, cacheSolve will use the newly created
##makeCacheMatrix object as its input. In summary, makeCacheMatrix creates objects
##to store the specified matrix and its inverse which will be calculated in cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

##cacheSolve uses a makeCacheMatrix object as its input to use the matrix and the
##"inv" to store the inverse of the matrix. The inverse is calculated in this function,
##but it is had been already calculated it retrieves it from the cached data without
##the necessity to calculate it once again.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
