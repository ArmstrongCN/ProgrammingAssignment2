## cachematrix.R
## 
## Build a list of function to maintain matrix and its inverse cache. 
## It's helpful while dealing with repeat inverse calculation on the
## same matrix.
##
## Example:
## M <- matrix(1:4, nrow=2, ncol=2)  
## cacheMatrix <- makeCacheMatrix(M)
## inverse <- cacheSolve(cacheMatrix)
##
## cacheMatrix$set(M) # chang the original matrix.
## M <- cacheMatrix$get() # get the original matrix.
## 

## Build up a list object that contains functions 
## for maintenaning matrix x and the cache of 
## its inverse.
##
## Params: x  a matrix object
## Return: list object, containning set, get,
##            setinverse, getinverse functions

makeCacheMatrix <- function(x = matrix()) {
  ## Initial: NOT CACHED YET.
  inv <- NULL
  ## Function for changging matrix.
  set <- function(y) {
    ## Change to a new matrix.
    x <<- y
    ## The cache should be invalidated.
    inv <<- NULL
  }
  ## Function for getting original matrix.
  get <- function() x
  ## Function for setting inverse matrix for cache.
  setinverse <- function(inverse) inv <<- inverse
  ## Function for getting cached inversed matrix.
  ## If not yet cached, return null.
  getinverse <- function() inv
  ## Put all the functions together
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
## If the inverse is already calculated, return 
## from the cache. Otherwise pass to solve() function.
##
## Params: x  the matrix needed to be inversed
## Return: The inversed matrix of x. 

cacheSolve <- function(x, ...) {
  ## Reading from cache.
  inv <- x$getinverse()
  ## Check if already cached.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## Get original matrix.
  data <- x$get()
  ## Really calculate the inverse.
  inv <- solve(data, ...)
  ## Cache the result.
  x$setinverse(inv)
  inv
}
