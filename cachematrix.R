## Coursera R Programming course programming assignment 2: Lexical scoping
## Using the <<- operator (which can be used to assign a value to an object in 
## an environment that is different from the current environment), these
## functions are used to create a special object that store a matrix and caches
## its inverse. It is assumed that the matrix supplied is always invertible.

## The makeCacheMatrix function creates a special object that can 
## cache its inverse. It's really a list containing a function to:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse
## 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set = set, 
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## The cacheSolve function calculates the inverse of the special object created
## with the above function. First, it checks to see if the inverse has already 
## been calculated, and if so, gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the matrix and sets the 
## value of the inverse in the cache via the setinverse function.

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
