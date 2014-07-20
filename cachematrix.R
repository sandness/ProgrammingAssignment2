## These functions allow you to create an environment containing a matrix
## and its cached inverse, and evaluate the inverse (making use of the
## cached value if available) on demand.

## Creates an environment containing a matrix, and a variable to cache
## its inverse.  Returns a list object that provides the following methods:
##   get():  Retrieves the value of the matrix.
##   getInverse():  Gets the inverse of the matrix.  If a cached value is
##     not available, the inverse is computed, cached, and returned.
##     Otherwise, the cached value is returned directly.
##   set():  Changes the value of the matrix.

makeCacheMatrix <- function(x = matrix()) {        
        inverse <- NULL

        get <- function() x

        getInverse <- function(...) {
                if (is.null(inverse)) inverse <<- solve(x, ...)
                inverse
        }

        set <- function(new) {
                if (!identical(x, new)) {
                        inverse <<- NULL
                        x <<- new
                }
        }

        list(get = get, getInverse = getInverse, set = set)
}


## Evalutes the inverse of a matrix that has been wrapped using
## makeCacheMatrix().  Arguments after the wrapped matrix will be passed
## through to the solve() function when a matrix inverse is calculated.

cacheSolve <- function(x, ...) {
        x$getInverse(...)
}
