makeCacheMatrix <- function(x = matrix()) 
  ## creates a matrix object that makes a cache of the inverse of a matrix
  ## creates a list containing function to be used as inputs to cacheSolve
  
{
  i <- NULL
  
  set <- function(y) 
  {
    x <<- y
    i <<- NULL
  }
  ## changes the matrix stored in the main function
  ## don't need this function unless we change the matrix
  ## x <<- y substitutes the vector x with y (the input) in the main function
  ## i <<- NULL makes the inverse i  null - the old inv is not needed
  
  get <- function() x
  ## returns the matrix x stored in the main function or x that has been changed
  
  setinverse <- function(inverse) i <<- inverse
  ## store the value of the input into variable i into the main function makeCacheMatrix
  
  getinverse <- function() i
  ## returns the stored value (in i) above
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## stores the 4 functions in the function makeCacheMatrix, allows assignment of makeCacheMatrix to an object



cacheSolve <- function(x, ...) 
  ## returns inverse of matrix returned by makeCacheMatrix
  ## if inverse has been cacl and not changed, cacheSolve pulls inverse from cache
  
{
  i <- x$getinverse()
  
  if(!is.null(i)) 
  {
    message("getting cached data")
    return(i)
  }
  ## verifies the value i was stored prev with getinverse (that it exists)
  ## if exists then returns a message and the value i - gets value from cache and skips computation
  ## if no value, then calculates the inv using code below
  
  data <- x$get()
  ## gets the matrix stored with makeCacheMatrix
  
  i <- solve(data, ...)
  ## calculates inverse of the matrix
  
  x$setinverse(i)
  ## stores i (inverse of matrix) 
  
  i
  ## returns i
  
}
