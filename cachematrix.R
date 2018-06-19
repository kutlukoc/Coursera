##These two functions are written to cache the inverse of a matrix.
##If the contents of the matrix are not changing, it is possible
##to cache its inverse so that when we need it again, 
##it can be looked up in the cache rather than recomputed.

##This function creates a special "matrix" object that can cache its inverse.
##It creates a special  matrix which is a list containing a function to 
##set the value of the matrix, get the value of the matrix,
##set the value of the inverse, get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
set <- function(y) {
        x <<- y    
        inv <<- NULL 
    }
get <- function() x
setInverse <- function(inverse) inv <<- inverse
getInverse <- function() inv
list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

#This function calculates the inverse of the special "matrix" 
#created with makeCacheMatrix above. However, it first checks to see if 
#the inverse has already been calculated. If so, it gets the inverse from
#the cache and skips the computation. Otherwise, it calculates the inverse
#of the data and sets the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
inv <- x$getInverse()
if(!is.null(inv)) { 
        message("getting cached data")
        return(inv)
    }
data <- x$get()
inv <- solve(data, ...)
x$setInverse(inv)  
inv
}
