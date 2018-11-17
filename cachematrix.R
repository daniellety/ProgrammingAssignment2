## makeCacheMatrix creates a matrix that can cache its inverse.
## 'x' is an invertible square matrix
makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL

## set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

## get the value of the matrix
        get <- function() x

## functions that set and get the inverse of the matrix

        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## computes the inverse of the matrix created by makeCacheMatrix function
## return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
       inv <- x$getInverse()

## check if 'inv' has a value
        if(!is.null(inv)) {
                message("Getting cached data.")
                return(inv)
        }

        data <- x$get()

## computes the inverse of the matrix
## note: solve(x) function in R returns its inverse value
        
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv

}
