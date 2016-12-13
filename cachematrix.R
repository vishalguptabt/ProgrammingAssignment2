## Function to create a special matrix object
## to cache its inverse along with its setter and getter functions

makeCacheMatrix <- function(x = matrix()) {
  selfInv <- NULL
  set <- function(y) {
    x <<- y
    selfInv <<- NULL
  }
  get <- function() x
  setInverse <- function(in_inverse) selfInv <<- in_inverse
  getInverse <- function() selfInv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function will use the objects created by makeCacheMatrix 
## and calculate its inverse, if the inverse is already calculated 
## it will use the cached value

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    
  }else {
    #When inv is null, calculate the inverse and
    #set it
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
  }
  return(inv)
}
