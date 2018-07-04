## This function creates a special "matrix" object that can cache its inverse

## define the argument with default mode of "matrix"
makeCacheMatrix <- function(x = matrix()) {   
  
  ## initialize inv as NULL
  inv <- NULL 
  ## define the set function to assign new value of matrix in parent env
  set <- function(y) {    
    x <<- y  
  ## if there is a new matrix, reset inv to NULL
    inv <<- NULL 
  }
  
  ## define the get fucntion - returns value of the matrix argument
  get <- function() x 
  setInverse <- function(inverse) inv <<- inverse   ## assigns value of inv in parent environment
  getInverse <- function() inv ## gets the value of inv where called
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
