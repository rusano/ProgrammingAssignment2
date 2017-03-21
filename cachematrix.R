# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing it
# repeatedly

# makeCacheMatrix function creaties a list containing a function to:

# 1- set the value of the matrix
# 2- get the value of the matrix
# 3- set the value of the inverse of the matrix
# 4- get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    set_inverse <- function(solve) s <<- solve
    get_inverse <- function() s
    list(set = set, get = get, set_inverse = set_inverse,
         get_inverse = get_inverse)
}


# This second function returns the inverse of the matrix, but it first
# checks if it has been already calculated, and in that case it takes
# the cached value, and informs of it.

# For the use of this function we assume the matrix is invertible

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$get_inverse()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data)
    x$set_inverse(s)
    s
}
