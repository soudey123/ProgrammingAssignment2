## The first function, makeCacheMatrix creates a special "matrix", which is containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
## Use function solve() to inverse the matrix

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
set <- function(y){
  x <<- y
  i <<- NULL
}
  get <- function() x
  setinverse <- function(solve) i <<- solve()
  getinverse <- function() i
  matrix(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
   i <- x$getinverse()
   if(!is.null(i)) {
     message("getting cached data")
     return(i)
   }
   data <- x$get()
   i <- solve(data, ...)
   x$setinverse(i)
   i
}
