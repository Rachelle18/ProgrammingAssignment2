## makeCacheMatrix creates a special matrix object, and then cacheSolve
## calculates the inverse of the matrix.

## This function creates a special "matrix" object, by setting the value of the vector
## getting the value of the vector, setting the value of the inverse
## and getting the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The function cacheSolve returns the inverse of a matrix created with
## the makeCacheMatrix function. If the inverse is available in the cache, 
## cacheSolve skips the computation and retrieves it. If not 
## it computes it, caches, and returns it.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached inverse matrix")
                inv
        } else {
                inv <- solve(x$get())
                x$setinverse(inv)
                inv
        }
}
