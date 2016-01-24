##Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. 
## This pair of functions, makeCacheMatrix and cacheSolve, work together to 
## cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      #Create empty variable for inverse matrix
      inverseM = NULL
      #Set the matrix
      set <- function(y) {
            x <<- y
            inverseM <<- NULL
      }
      #Get the matrix
      get <- function() x
      #Set the inverse
      setInv <- function(inverse) inverseM <<- inverse
      #Get the inverse
      getInv <- function() inverseM
      #List that is sent to cacheSolve function
      list(set = set, get = get, setInv = setInv, getInv = getInv)    
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
      #Return a matrix that is the inverse of 'x'
      inverseM <- x$getInv()
      #Check to see if mean has already been calculated
      if(!is.null(inverseM)) {
            #If already has been computed, returns data from cache & skips the inversion
            message("getting cached data")
            return(inverseM)
      }
      #If has not already been computed, computes the inverse of the 
      #matrix returned from makeCacheMatrix
      data <- x$get()
      inverseM <- solve(data, ...)
      #Sets results of inverted matrix in the cache
      x$setInv(inverseM)
      inverseM
}

##Test 1
source("cachematrix.R")
test_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
test_matrix$get()
test_matrix$getInv()
cacheSolve(test_matrix)
cacheSolve(test_matrix) #To test for statement return
test_matrix$getInv()


