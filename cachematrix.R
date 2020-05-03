## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Creates function makeCache Matrix that sets the value of the matrix, 
#gets the value of the matrix, sets the value of the inverse of that matrix,
#then gets the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## Write a short comment describing this function
#Creates an inverse of a given matrix, but first tests if the inverse has already
#been created for that matrix. If so, it outputs the inverse from the cache. If the 
#inverse does not exists, it outputs the inverse via the solve().
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
    }

