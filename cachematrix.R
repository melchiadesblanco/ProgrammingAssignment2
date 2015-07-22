## This R script contains two functions made for the Assigment 2 of the course
## Data Science. It reproduces the techniques used in the makeVector example.

## Usage:
## source('cachematrix.R')
## z <- makeCacheMatrix()
## z$set(matrix(c(1, 2, 3, 4), 2, 2))
## cacheSolve(z)
## z$getinverse()

## This function creates a list that contains four functions:
##      get()       to get the current x matrix
##      set()       to set the current x matrix
##      setinverse  to set the i matrix
##      getinverse  to get the current i matrix

makeCacheMatrix <- function(x = matrix()) {
    ## This variable is used to store a matrix (inverse of the x matrix)
    i <- NULL
    
    ## This is used to set a value
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ## This function is used to get the matrix x
    get <- function() x
    ## This function set the i with a matrix (inverse of x)
    setinverse <- function(solve) i <<- solve
    ## This function returns the i matrix
    getinverse <- function() i
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of a matrix x and store it in itself

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
