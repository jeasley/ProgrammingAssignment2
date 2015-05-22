## These functions allow for the inversion of a given matrix. In the event that the inversion has already been
## store, the function will retrieve those results. If it has not, it will invert the matrix and store those results
## for future use.

## This function sets the value of a matrix, computes the inverse and stores functions to store and retrieve the inverted matrix.  

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL ##set value of matrix
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
               
                get <- function() x                             ##get value of matrix
                setinverse <- function(solve) m <<- solve     ##set value of inverse
                getinverse <- function() m                      ##get value of inverse
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)                   ##function given names so they are callable
}


## This function checks to see if the inverse is store, if it is it print that seeks cached data. If not, it compute the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse() ##get chached mean
        if(!is.null(m)) { ##if mean is cached
                message("getting cached data")
                return(m) #exit
        }
        data <- x$get() #if not, store data
        m <- solve(data) #take inverse
        x$setinverse(m) #store result
        m #print inverse
}

