## Implementation of the functions required in the 'Programming Assignment 2' of the Course 'R Programming'
## Includes functions to store the inverse of a matrix and to calculate it (if already not stored).


## Creates a list of functions able to cache the inverse and the original matrix

makeCacheMatrix <- function(x = matrix()) {
      
      inv <- NULL
      
      ## Creating 4 function for getting and setting both the matrix and its inverse
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      
      ## Output a list with the previous 4 functions
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

## Computes the inverse of the matrix (using function 'solve') 
##    if it is not already recorded and return the inverse.
## The input of the function is not a matrix, 
##    but the result of the previous function applied to the original matrix.

cacheSolve <- function(x, ...) {
      
      ## Get the inverse matrix
      inv <- x$getinv()
      
      ## If already computed and stored, do not recalculate but return the stored value.
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      
      ## If not already computed and stored, calculate the inverse
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      
      ## Output: inverse of the original matrix
      inv
}
