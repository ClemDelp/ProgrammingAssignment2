## matricx class (constructor, getter, setter) put matrixin cache when created
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL 
	## setter matrix
    set <- function(y) {
            x <<- y
            m <<- NULL
    }
    ## getter matrix
    get <- function() x
    ## setter inverse matrix
    setsolve <- function(solve) m <<- solve
    ## getter inverse matrix
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## function to inverse a given matriw
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
            message("getting cached data")
            return(m)
    }
    data <- x$get()
    ## inverse the given matrix
    m <- solve(data, ...)
    x$setsolve(m)
    m
}