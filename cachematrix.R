## Functions that create an object for storing and retrieving a matrix,
## and also caching its inverse matrix for later retrieval

##
## makeCacheMatrix
## - Create a matrix object, with functions to set/retrive both the value of the matrix and its inverse matrix
##
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y)  {
                ## If matrix has changed, nullify the cached inverse matrix
                if (!identical(x,y))  {
                        x <<- y
                        i <<- NULL
                }
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##
## cacheSolve
## - Solves the inverse of a matrix object created by the makeCacheMatrix function above
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i))  {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}