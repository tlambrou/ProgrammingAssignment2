## Below are two functions that are used to create a special object that stores
## a matrix and cache's its inverse.

## The first function, makeCacheMatrix creates a matrix, which is really a list
## containing a function to...
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) s <<- solve
      getinverse <- function() s
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function solves for the inverse of the matrix created with the
## above function. However, it first checks to see if the inverse has already
## been calculated. If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it solves the inverse of the data and sets the value 
## of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
       s <- x$getinverse()
       if(!is.null(s)) {
             message("getting cached data")
             return(s)
       }
       data <- x$get()
       s <- solve(data, ...)
       x$setinverse(s)
       s       ## Return a matrix that is the inverse of 'x'
}
