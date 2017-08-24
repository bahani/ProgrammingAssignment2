## This R program solves for the inverse of a matrix and caches it


## makeCacheMatrix function stores a given matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        z <- NULL
        set <- function(y){
                x <<- y
                z <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) z <<- inv
        getinverse <- function() z
        list(set = set, get = get, 
             setinverse = setinverse, getinverse = getinverse)

}

## cacheSolve function retrives the inverse of a matrix from makeCacheMatrix, 
## if not existant it will solve for the inverse and store the result in makeCacheMatrix

cacheSolve <- function(x, ...) {
        z <- x$getinverse()
        if(!is.null(z)){
                message("Retreiving cashed data")
                return(z)
        }
        data <- x$get()
        z <- solve(data,...)
        x$setinverse(z)
        z
        
}
