## makeCacheMatrix is 'special' matrix programmed below to cache its inverse


makeCacheMatrix <- function(x = matrix()) {
    h <- NULL
    set <- function(y) {
        x <<- y
        h <<- NULL
} 
    get <- function() x
    setsolve <- function(solve) h <<- solve
    getsolve <- function() h
    list(set = set, ## gives the name 'set' to the set() defined function
         ## in same way below three commands give the names
         ## 'get', 'setsolve'  and 'getsolve' below-defined functions
         ## get(), setsolve() and getsolve() respectively
         get = get,
         setsolve = setsolve,
         getsolve = getsolve) }


## cacheSolve() is required to populate and/or retrieve the 
## inverse from an object of type makeCacheMatrix()

cacheSolve <- function(x, ...) {
    h <- x$getsolve()
    if(!is.null(h)) {
        message("getting cached data")
        return(h)
    }
    data <- x$get()
    h <- solve(data, ...)
    x$setsolve(h)
    h}

