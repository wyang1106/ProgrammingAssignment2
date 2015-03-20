# The first function, makeCacheMatrix, takes a matrix and returns a list 
# containing getter and setter functions for the matrix and its cached inverse.
# The second function, cacheSolve, takes the return value of makeCacheMatrix
# and computes its inverse.


# This function creates a special "matrix" object that can cache its inverse.
#
# Args:
#   x: an invertible matrix
#
# Returns:
#   a list of functions that can get/set x and its inverse:
#       get: return matrix x
#       set: set matrix x
#       set_inverse: set cached inverse of x
#       get_inverse: return inverse x
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then cacheSolve should retrieve the inverse
# from the cache.
#
# Args:
#   x: list returned by makeCacheMatrix
#
# Returns:
#   inverse of matrix represented by x
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$get_inverse()
    if (!is.null(m)) {
        # return cache if exists
        print("3")
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$set_inverse(m)
    m    
}
