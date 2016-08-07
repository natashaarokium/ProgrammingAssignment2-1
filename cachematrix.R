## My function was written to create the inverse of a special matrix. This special matrix would be stored in cache.
## I first initiallized the objects, assigned set to the function (y), then assigned get to the function (x). Assigned 
## Setinverse to find the inverse of a matrix. Each function is assigned within a list(), which returns it 
## to the parent environment.


makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
   set <- function(y) {
     x <<- y
      m <<- NULL
   }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
   list(set = set, get = get,
         setinverse = setinverse,
          getinverse = getinverse)
  }


## With cachesolve, codes are able to access the values of both x and m via get and set. Cachesolve is able to find and 
## store the inverse of a matrix for the input argument makeCacheMatrix. The functions can be accessed by $, because the list 
## elements in MakeCacheMatrix are defined with names.  

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }
    data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}
