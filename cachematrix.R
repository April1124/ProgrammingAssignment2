## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 # holds the cached value or NULL if nothing is cached
  # initially nothing is cached so set it to NULL
  inv <- NULL
  # store a matrix
  
  set <- function(y){
    x <<- y
      # since the matrix is assigned a new value, flush the cache
    inv <<- NULL
  }
  
   # returns the stored matrix
  get <- function() x
   # cache the given argument 
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  # get the cached value
  getInverse <- function() inv
    # return a list. Each named element of the list is a function
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
# makeCacheMatrix
cacheSolve <- function(x, ...) {
    # get the cached value
       
  inv <- x$getInverse()
    # if a cached value exists return it
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  # otherwise get the matrix, caclulate the inverse and store it in
  # the cache
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  
    # return the inverse
  inv      
}
