

## A function that creates a  matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inve <- NULL 
        set <- function(y){
                x <<- y
                inve <<- NULL
        }
        get <- function() x
        setInverse <- function(solveMatrix) inve <<- solveMatrix
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function that computes the inverse of a "matrix" returned by 
#makeCacheMatrix above. 


cacheSolve <- function(x, ...) {
                inv <- x$getInverse()
        if(!is.null(inve)){                      #If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
                message("getting cached data") 
                return(inve)
        }
        data <- x$get()
        inve <- solve(data) #Solve(X) returns its inverse.
        x$setInverse(inve)
        inve      
}