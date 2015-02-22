## Creates a special matrix capable of caching its inverse

## Uses the `<<-` assignment operator to store the value of the given `inverse` to the
## variable `i` in a different environment which can be accessed from `getinverse`

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of special matrix x, caching its result and using it if available

## The function first checks if `getinverse` doesn't return NULL. If it does, there is
## no cached value so we proceed and calculate the matrix's inverse using the function
## `solve`. 
## On the other hand if `getinverse` returns a non-NULL value we simply return it.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  i <- solve(x$get())
  x$setinverse(i)
  i
}