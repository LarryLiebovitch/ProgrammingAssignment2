## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
### Uses cachemean, but replaces all references to mean with a and solve
### This function moves the matrix to the cache

makeCacheMatrix <- function(x = matrix()) {
        a <- NULL
        set <- function(y) {
                x <<- y
                a <<- NULL
        }
        
        get <- function() x
        setsolve <- function(solve) a <<- solve
        getsolve <- function() a
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function
### Uses makeVector, but replaces all references to mean with a and solve
### This function determines if the matrix inverse already exists in the cache
### If it exists, it returns its value, if not it computes its inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        a <- x$getsolve()
        if(!is.null(a)) {
                message("getting cached data")
                return(a)
        }
        data <- x$get()
        a <- solve(data, ...)
        x$setsolve(a)
        a
}
