## Caching the inverse of a matrix to avoid the overhead of calculating it more than once

## Constructor that when passed a square, inversible matrix, returns a list of functions that
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse of the matrix
# 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  #Check we have a matrix..
  if (!is.matrix(x)) {
    stop("Input is not a matrix")
  }
  
  #...and it is square
  if (!nrow(x)==ncol(x)) {
    stop("Matrix is not square")
  }
  
  i <- NULL
  
  #Stores the matrix, removes any previously stored inverse
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  #Returns the previously stored matrix
  get <- function() x
  
  #Stores the inverse of the matrix
  setinverse <- function(m) i <<- m
  
  #Returns the inverse of the matrix
  getinverse <- function() i
  
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## Given a square matrix this function returns its inverse
# If the inverse has previously been calculated it is returned from the cache,
#   otherwise it is calculated, cached and returned

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
  
  #Try and retrieve the inverse
   i <- x$getinverse()
   
   #If previously cached, return what we have
   if (!is.null(i)) {
     return(i)
   }
   
   #Otherwise, retreive the original matrix
   m <- x$get()
   
   #Calculate its inverse
   i <- solve(m)
   
   #Store the inverse for future use
   x$setinverse(i)
   
   #Return the inverse
   i
}
