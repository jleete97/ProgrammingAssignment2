## Create a version of a square, invertible matrix that caches its own inverse.


## Create the "cached-inverse matrix" that stores a matrix, and will cache the
## inverse if it calculates it.
## 
## param:  x - A square, invertible matrix
## return: A list with get(), set(), getsolve() and setsolve() functions, and
##         internal state to hold the original matrix and cached inverse (once
##         calculated).
## 
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    
    list(set = set,
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Calculate the inverse of a "cached-inverse matrix," as above.
##
## param:  x - A "cached-inverse" matrix, from makeCacheMatrix() above.
## return: The inverse of the matrix.
## 
cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    
    if (is.null(s)) {
        m <- x$get()
        s <- solve(m, ...)
        x$setsolve(s)
    }
    
    s
}
