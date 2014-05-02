## These two companion functions are used for caching the inverse of the matrix
## to avoid expensive operation of the inverse operation in case it's expensive.
##
## There are two main functions here:
## makeCacheMatrix
## cacheSolve
##
## The expected usage of these two companion functions are:
## m <- matrix(c(1, 2, 3, 4), ncol = 2, nrow = 2)
## cm <- makeCacheMatrix(m)
## cacheSolve(cm) ## The inverse of the m will be calculated adhoc
## cacheSolve(cm) ## Beforing seeing the result printed, you will see a message
##                ## "Inversed Matrix retrieved from cache" printed at first

## "makeCacheMatrix" is used to create a special "matrix" with behaviors
##                   supporting caching an inversed matrix within it.
##
##                   It's actually a list containing four functions.  They are:
##                   $get - get the value of the matrix
##                   $set - set the value of the matrix
##                   $getInverse - get the inversed matrix of the input matrix
##                   $setInverse - set the inversed matrix of the input matrix

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix = NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inverseMatrix <<- inverse
    getInverse <- function() inverseMatrix

    list(get = get, set = set, setInverse = setInverse, getInverse = getInverse)
}


## "cacheSolve" is used to obtain the inverse of the "matrix" object created by
##              "makeCacheMatrix".  It might get it from cache or calculate it
##              adhoc and store it back to cache if it's not available at the
##              first time.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getInverse()

    if (!is.null(inverseMatrix)) {
        message("Inversed Matrix retrieved from cache")
        return(inverseMatrix)
    }

    m <- x$get()
    inverse <- solve(m, ...)
    x$setInverse(inverse)
    inverse
}
