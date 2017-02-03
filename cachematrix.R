
##makeCacheMatrix returns a list of functions that share the value of the parameter m through "<<-".
##setSolve sets the value of m which can later be retrieved through getsolve.
##So it caches the solve value for the next time it is called. 
##In other words, "setsolve" sets  solve() value calculated in the main function
##and getsolve returns "m" the stored value. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}


## Write a short comment describing this function
## The first time cacheSolve runs it gets m=NULL from makeCacheMatrix,  
## it stores the whole matrix in "data".  It calculates its inverse and  
## stores or "sets" the result. This way the next time cacheSolve is
## run with the same matrix it doesn't have to calculate it again.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

