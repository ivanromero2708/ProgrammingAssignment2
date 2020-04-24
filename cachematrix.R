## Put comments here that give an overall description of what your
## functions do
## The functions allows to create a special Matrix that can cache its inverse. Then, it allows to 
## verify the existence of a cache value for Matrix Inverse, calculate if not available and retrieve
## the value if it does exist

## Write a short comment describing this function
## makeCacheMatrix allows to create a special Matrix that can cache its inverse. It creates a list of
## functions that are described next. First, initialize the objects x and inverse through the argument
## of the function and the first line of code. The set function, one of the elements inside the output 
## list, assign the NULL value to the object "inverse" and allows to load a new "x" value. The function 
## get, retrieves the x value from the parent Environment. The function setinverse, allows to assign the
## inverse value in the parent Environment.getinverse allows to get the value for inverse from the parent
## Environment. And finally, it creates a list where it stores the previous described functions.

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() {
            x
      }
      setinverse <- function(inverse_value_set = matrix()) {
            inverse <<- inverse_value_set
      }
      getinverse <- function() {
            inverse
      }
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## The function takes the result from an object created with the makeCacheMatrix function and checks if the
## result is not NULL. In case TRUE it reads the stored value in the previously created list. In FALSE, 
## calculates the inverse of the given Matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverse <- x$getinverse()
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data, ...)
      x$setinverse(inverse)
      inverse
}