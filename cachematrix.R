
## this function obtains a matrix and returns a list containing 4 functions

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, 
             get = get,
             setinv = setinv,
             getinv = getinv)
}


## this function checks that  was the computation made before
## for the passed argument if yes then it directly returns the value or else 
## computes it

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(is.null(inv)) {
        
                data <- x$get()
                
                        
                inv <- solve(data)
                x$setinv(inv)
                
                
        }

        inv
}

