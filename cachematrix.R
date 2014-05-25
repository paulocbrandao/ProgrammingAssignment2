## Programming assignment 2 for the R programming course 
## of Coursera's Data Science Specalization

##Paulo Brandao - 05-22-14

##This file contains two functions which goal is to calculate the inverse 
##of a matrix. In order to gain efficiency in potentially time-consuming 
##computations, the functions keep track of a cached value of the inverse.

## The makeCacheMatrix function returns a list of functions which are going
## to be used to create (set) and use (get) the matrix to have the inverse
## calculated and cached. It also returns 'setinv' and 'getinv' functions which 
## creates and uses the inverse of the cached matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The cacheSolve function first check the previous value of the inverse. If it's
## NULL the cacheSolve function calculates the inverse using the function 'solve'.
## If the previous value of the inverse is not NULL, the cacheSolve function will
## simply return the cached value. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
