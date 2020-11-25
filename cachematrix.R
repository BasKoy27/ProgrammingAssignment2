## Assignment: Caching the Inverse of a Matrix
## Below are two functions used to create a special matrix that can cache 
## its inverse.

## The first function sets and gets the value of the matrix 
## as well as sets and gets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL				
  set <- function(y){
    x <<- y			
    inv <<- NULL			
  } 
  get <- function() x				
  setinv <- function(inverse) inv <<- inverse	
  getinv <- function() inv					
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The second function computes the inverse of the special matrix returned 
## by makeCacheMatrix function.
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of 
## the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")	
    return(inv)
  }
  
  data <- x$get()				
  inv <- solve(data, ...)			
  x$setinv(inv)				
  inv	  
  
}
