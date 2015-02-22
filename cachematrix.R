## These two functions allow to cache the inverse of a given matrix.

## The function 'makeCacheMatrix' creates a list of 4 functions that can be used for setting and returning
## a matrix and its inverse. The function takes a matrix as an argument or can be called without an argument.
## Its result should be assigned to a variable for further processing (e.g. 'special.matrix).
## Then the created functions can be called as subsets of this variable e.g. special.matrix$set.

makeCacheMatrix <- function(x = matrix()) { 
  i <- NULL ## creates an empty variable i for storing the matrix's inverse
  set <- function(y) { ## The function 'set' takes a matrix 'y' as an argument and stores it for further usage 
    x <<- y ## the matrix 'y' is stored in the variable 'x' in the environment of the function 'makeCacheMatrix'
    ##(this is the same 'x' that is the function 'makeCacheMatrix''s argument) 
    i <<- NULL ## The variable 'i' in the environment of the function 'makeCacheMatrix' is reset to NULL
    ## for every new matrix stored with use of 'set'
  }
  get <- function() x ## returns the matrix stored with use of 'set'
  setinverse <- function(inverse) i <<- inverse ## stores a given inverse in the variable 'i' 
  ## in the environment of the function 'makeCacheMatrix'
  getinverse <- function() i ## returns the inverse stored in 'i'
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) ## returns the list of created functions.
}


## The function 'cacheSolve' retrieves a matrix's inverse from the cache or calculates it
## if it has not been cached yet. It takes the result of the 'makeCacheMatrix' function as an argument

cacheSolve <- function(x, ...) {
  i <- x$getinverse() ##retrieves the inverse stored from the cache using the 'getinverse' function
  if(!is.null(i)) { ## checks if the inverse has been cached earlier
    message("getting cached data")
    return(i) ## returns the inverse
  }
  data <- x$get() ## if the inverse has not been cached, it is calculated. The matrix used for calculation is
  ## retrieved with use of 'get' function and stored in 'data' variable
  i <- solve(data, ...)
  x$setinverse(i) ## stores inverse in cache
  i ## returns the inverse
  
}

