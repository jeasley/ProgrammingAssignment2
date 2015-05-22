## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL ##set value of matrix
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
               
                get <- function() x                             ##get value of matrix
                setinverse <- function(solve) m <<- inverse     ##set value of inverse
                getinverse <- function() m                      ##get value of inverse
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)                   ##function given names so they are callable
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse() ##get chached mean
        if(!is.null(m)) { ##if mean is cached
                message("getting cached data")
                return(m) #exit
        }
        data <- x$get() #if not, store data
        m <- inverse(data) #take inverse
        x$setinverse(m) #store result
        m #print inverse
}
