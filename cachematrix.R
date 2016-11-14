## Matrix inversion is usually a costly computation so these functions can cache the value of the inverse so it
# doesn't need recalcuting every time

## The function caches the inverse of the matrix so it doesn't need recalculating

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function()
  {
    x
  }
  setinverse <- function(inverse)
  {
    m <<- inverse
  }
  getinverse <- function()
  {
    m
  }
  list(set = set,get = get,setinverse=setinverse,getinverse=getinverse)
}

## This function calculates the inverse of the matrix if it hasn't been calculated before. If it has it returns
# the cached version

cacheSolve <- function(x, ...) 
{
  m <- x$getinverse()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
