## The purpose of the below two functions is to cache the potentially time-consuming
## of inverting a matrix.
##    Definition of inverse of a matrix can be found in the url below:
##       https://github.com/jjrs/ProgrammingAssignment2.git Put comments here that give an overall description of what your
## 

## This method creates a cached

makeCacheMatrix <- function(x = matrix()) {
    ## Assign NULL value in the local (function) environment
    m <- NULL
    set <- function(y) {
        ## Assign in the parent environment
        x <<- y
        ## Asign NULL value in the parent environment
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
    
}


## This is the exposed function that will be called.
## It will first check if the provided matrix had been already inverted:
##   - in case it was, it will return the cached result
##   - in case it wasn't, it will calculate it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
