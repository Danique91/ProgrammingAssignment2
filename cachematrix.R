

makeCacheMatrix <- function(x = matrix()) {  ## define the argument with default mode of "matrix"
 inv <- NULL                             ## NULL is the value of matrix inverse 
 +    set <- function(y) {                    ## define the set function to assign a new value to the matrix in the parent
                                              ## environment
 +        x <<- y                        
 +        inv <<- NULL                        ## if there is a matrix outside the parent environment, reset inv to NULL
 +    }
 +    get <- function() x                     ## define the get function which returns value of the matrix argument
 +    
 +    setinverse <- function(inverse) inv <<- inverse  ## assigns the value of inv in parent environment
 +    getinverse <- function() inv                     ## gets the value of inv where called
 +    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## you need list in order to refer 
 +                                                                                  ## to the functions with the $ operator
}



 +## This function computes the inverse of the matrix returned by makeCacheMatrix.
 +## If the inverse has already been calculated (and the matrix has not changed),
 ## then cacheSolve will retrieve the inverse from the cache
  
  cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
 +    inv <- x$getinverse()
 +    if(!is.null(inv)) {
 +        message("getting cached data")
 +        return(inv)
 +    }
 +    data <- x$get()
 +    inv <- solve(data, ...)
 +    x$setinverse(inv)
 +    inv
  
}

