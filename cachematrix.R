##Author: James Cichon
##Assignment: Program 2


## Creates a matrix that can cache the inverse of that matrix


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {			##set values of the matrix
        x <<- y
        m <<- NULL
    }
    get <- function() x				##get values of the matrix
    setsolve <- function(solve) m <<- solve	##set values of the inverse matrix
    getsolve <- function() m			##get values of the inverse matrix
    list(set = set, get = get,			##list inverse matrix
         setsolve = setsolve,
         getsolve = getsolve)
    
}


## Computes the inverse of a matrix returned by makeCacheMatrix. 

cacheSolve <- function(x, ...) {	##Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {			##If inverse already calculated retrieve inverse from cache
        message("getting cached data")
        return(m)
    }
    data <- x$get()			##Calculate inverse of matrix
    m <- solve(data, ...)
    x$setsolve(m)
    m
}