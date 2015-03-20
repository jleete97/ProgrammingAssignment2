## ----------------------------------------------------------------------------
## Create a version of a square, invertible matrix that caches its own inverse.
## ----------------------------------------------------------------------------

## Create the "cached-inverse matrix" that stores a matrix, and will cache the
## inverse if it calculates it.
## 
## param:  x - A square, invertible matrix
## return: A list with get(), set(), getsolve() and setsolve() functions, and
##         internal state to hold the original matrix and cached inverse (once
##         calculated).
makeCacheMatrix <- function(x = matrix()) {
    # Initialize cached inverse
    s <- NULL
    
    # Define accessor methods
    set <- function(y) {
        # Reset internal state: store matrix, clear cached inverse
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solved) s <<- solved
    getsolve <- function() s
    
    # Return methods as list (with internal state)
    list(set = set,
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## Calculate the inverse of a "cached-inverse matrix," as above.
##
## param:  x - A "cached-inverse" matrix, from makeCacheMatrix() above.
## return: The inverse of the matrix.
cacheSolve <- function(x, ...) {
    inverse <- x$getsolve()
    
    if (is.null(inverse)) {
        # Inverse not previously calculated, do so now.
        original.matrix <- x$get()
        inverse <- solve(original.matrix, ...)
        x$setsolve(inverse)
    } else {
        # Inverse previously calculated, just use that.
        message("Using cached data for matrix inverse.")
    }
    
    inverse
}
