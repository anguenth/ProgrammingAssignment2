## For a given matrix the function returns its inverted matrix.
## The first time the function is used the inverted matrix is calculated
## whereas any subsequent function call just reads and returns the 
## the initially calculated and stored result.

## makeCacheMatrix formaly returns a list. Practically, via the list elements 
## the entire function-environment with its stored values (matrix, inverse matrix) 
## can be accessed.

makeCacheMatrix <- function(x = matrix()) {
    inverseCached <- NULL
    get <- function() x                   ## no arg needed as get() is object-specific
    getinverse <- function() inverseCached
    setinverse <- function(inverseNew)  inverseCached <<- inverseNew
    set <- function(otherMatrix) {        ## not required by assignment
        x <<- otherMatrix
        inverseCached <<- NULL            ## new otherMatrix has not been inverted yet
    }
    list(get=get, getinverse=getinverse, setinverse=setinverse, set=set)
}


## cacheSolve uses object specific functions to access and calculate
## the inverse of a matrix.

cacheSolve <- function(x, ...) {         ## other name arguments can be passed to be used in solve() 
    inverseCached <- x$getinverse()
    if(!is.null(inverseCached)) {
        message("getting cached data")
        inverseCached 
    }    
    else {                               ## used "else" instead of "return" in if
        inverseNew <- solve(x$get(), ...)
        x$setinverse(inverseNew)
        inverseNew
    }
}
