## This file contains cached inverse matrix function which uses the previously 
## calculated matrix inverse, if it was present.
## 

## Creates a list of functions (set, get, setinv, getinv) for a given matrix data.
## Function set sets the given matrix to x
## Function get gets the matrix
## Function setinv calculates the inverse of the matrix
## Function getinv retruns the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
     minv = NULL;
     setmatrix <- function(y) {
       x <<- y
       minv <<- NULL;
     }
     getmatrix <- function() x
     setinverse <- function(solve) minv <<- solve
     getinverse <- function() minv
     list (set = setmatrix,
           get = getmatrix,
           setinv = setinverse,
           getinv = getinverse)
     
}


## For a given matrix, checks if inverse exists. If it exists, cached matrix inverse
## is returned. If inverse does not exist, is calculated, cached and then returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    minv <- x$getinv()
    if (!is.null(minv)) {
        message("get cached inverse of matrix")
        return(minv)
    }
    mat <- x$get()
    minv <- solve(mat,...)
    x$setinv(minv)
    minv
}
