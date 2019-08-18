## functions to create a matrix object which caches its inverse

## create a special "matrix" object which caches its inverse

makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
   set <- function(y) {
      x <<- y
      i <<- NULL
   }
   get <- function() x
   setinv <- function(inverse) i <<- inverse
   getinv <- function() i
   list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## computes inverse of special "matrix" or retrieves cached inverse if matrix has not changed

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   i <- x$getinv()
   if(!is.null(i)) {
     message("getting cached data")
     return(i)
   }
   data <- x$get()
   i <- solve(data)
   x$setinv(i)
   i
}
