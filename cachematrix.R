## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
       #set m to NULL
                m <- NULL
        # set x as y & m to NULL in parenting environment
                set <- function(y)
                {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                #set the inverse matrix
                setinverse <- function(inverse) m <<- inverse
                #get inverse matrix
                getinverse <- function() m
                list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
      
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        # if m is not NULL then inverse has been calculated and 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # otherwise inverse the matrix 
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
