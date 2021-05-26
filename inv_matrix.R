## This set of function uses the lexical scoping of the R programming
##The inv_matrix.R file contains two functions, makeCacheMatrix() and cacheSolve()
## The first function in the file, makeCacheMatrix() creates an R object that stores a 
## matrix and its inverse. The second function, cacheSolve() requires an argument that 
## is returned by makeCacheMatrix() in order to retrieve the inverse from the cached value 
## that is stored in the makeCacheMatrix() object's environment.


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(solveMatrix) inv <<- solveMatrix
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv      
}

