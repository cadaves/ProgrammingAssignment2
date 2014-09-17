## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        mx <- NULL                         ## initializes matrix object, local to current environment
        set <- function(y) {
                x <<- y
                mx <<- NULL
        }
        
        get <- function() {
                x                       ## Returns the matrix x
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
## the inverse and then print it. 

cacheSolve <- function(x, ...) {
        mx <- x$getinverse()
        if(!is.null(mx)) {                       ##inverse already calculated?
                message("getting cached data")   ##if yes, then prints msg & gets cached inverse
                return(mx)
        }
        data <- x$get()                         ## Else, calculates inverse
        
        if (nrow(data)==ncol(data)) {          ## Verifies matrix is square
                if (det(data) != 0)  {          ## verifies mx nonsingular
        mx <- solve(data, ...)          
                                       
        x$setinverse(mx)
                mx
                } else {message("determinant =0")}
        } else { message("matrix not square")}   ## if matrix not square, 
                                                 ## then prints message
        
}
