## Put comments here that give an overall description of what your
## functions do

## The first function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {  ## define the argument with default mode of "matrix"
 inv <- NULL                                 ## initialize inv as NULL; will hold value of matrix inverse
  set <- function(y) {                       ## define the set function to assign new
    x <<- y                                  ## value of matrix in parent environment
    inv <<- NULL                             ## if there is a new matrix, reset inv to NULL
  }
  get <- function() x                        ## define the get function - returns value of the matrix argument
  setinverse <- function(inverse) inv <<- inverse   ## assigns value of inv in parent environment
  getinverse <- function() inv                      ## gets the value of inv where called
  list(set = set, get = get,                        ## you need this in order to refer to the function with the $ operator
       setinverse = setinverse,
       getinverse = getinverse)
}


## given the list variable from the first function, will first check to see if there's already a cached inverse and return
## otherwise will attempt to solve its inverse and set/return it

cacheSolve <- function(x, ...) {             ## Return a matrix that is the inverse of 'x'
inv <- x$getinverse()                 ## first to check if there's sth there already
  if(!is.null(inv)) {                   ## then check if there's a cached value AND it's a matrix
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()                       ## otherwise get the matrix
  inv <- solve(data, ...)               ## try to solve the matrix and catch error and warnings
  message("Setting the value of inverse to:")
  x$setinverse(inv)                     ## whatever the case, set the value of the inverse (NULL if something went wrong)
}
