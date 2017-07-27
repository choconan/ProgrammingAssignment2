## This two functions allows to caching the inverse of a matrix
## by utilising the lexical scoping of R
## - written by Yinan Zhang

## This function define the list containing various function:
## a) set the value of the matrix
## b) get the value of the matrix
## c) set the value of the inverse of the matrix
## d) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() {x}
        setInverse <- function(solve) {m <<- solve}
        getInverse <- function() {m}
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function calculates the inverse of the matrix.
## The if condition checks firstly whether the inverse
## of the matrix has been defined already. And if:
## a) the inverse has beeen defined by getInverse, then
##    the same value will be returned
## b) m is NULL, this function will calcualte the inverse
##    of the given matrix and return the m.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}