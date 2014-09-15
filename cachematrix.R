## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        mx <- NULL                         ## initializes matrix object
        set <- function(y) {
                x <<- y
                mx <<- NULL
        }
        
        get <- function() {
                x                  ## Returns the value of matrix x
        }
        
        setinverse <- function(inverse) {           
                mx <<- inverse
        }
        
        getinverse <- function() {
                mx
        }
         
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## The function cacheSolve checks to see if the inverse of the matrix has
## already been calculated.  If it has, it will access the inverse
## from the cache and print it.  If it has not, it will solve for
## the inverse and then print it. Note that this assignment assumed that the
## matrix would be a square one, and, thus, possible to calcualte the inverse.
## If the matrix is singular (determinant =0), then the solve function will
## return an error message.

cacheSolve <- function(x, ...) {
        mx <- x$getinverse()
        if(!is.null(mx)) {                       ##inverse already calculated?
                message("getting cached data")   ##if yes, then gets cached inverse
                return(mx)
        }
        data <- x$get()
        mx <- solve(data, ...)          ## Else, calculates inverse. For this
                                        ## assignment, assuming a square mx
        x$setinverse(mx)
                mx
        
        ## Return a matrix that is the inverse of 'x'
}
