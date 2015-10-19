#Code has been modified from class Example: Caching the Mean of a Vector
#The first function, makeCacheMatrix creates a list containing the following function:
#1. to set the value of the matrix
#2. to get the value of the matrix
#3. to set the value of a matrix inverse
#4. to get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL #this is the object that holds the inverse 
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
# The purpose of the cachesolve function will be to compute the 
# inverse of the matrix provided by makeCacheMatrix
# if the inverse has been calculated previously, then the cached result
# will be retrieved
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting inverse of matrix cached value")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
