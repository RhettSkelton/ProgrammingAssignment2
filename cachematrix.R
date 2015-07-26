## Function1 Description
## makeCacheMatrix creates a special vector, which is really a list 
## containing a function to
##     1. set the value of the vector
##     2. get the value of the vector
##     3. set the value of the mean
##     4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  inverseM <- NULL
  set <- function(y) {
    x <<- y
    inverseM <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverseM <<- inverse
  getinverse <- function() inverseM
  list(set = set, get=get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}

## Function2 Description
## cacheSolve" calculates the mean of the special vector 
## created with the above function. However, it first checks 
## to see if the mean has already been calculated. If so, it 
## gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the 
## value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
  inverseM <- x$getinverse()
  if(!is.null(inverseM)) {
    return(inverseM)
  }
  data <- x$get()
  inverseM <- solve(data)
  x$setinverse(inverseM)
  inverseM
}
