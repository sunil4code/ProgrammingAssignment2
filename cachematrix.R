## makeCacheMatrix function accepts a matrix variable as input. It has functions
## that get the matrix, get the inverse matrix and sets the inverse matrx
## The cacheSolve function return an inverse matrix 

## The below function take matrix as in input. It has 4 function to  set and get the 
## matrix values and to set and get the inverse matrix values.
## When the function is called it sets the variable xinv same as the input matix. 
## Same is done when setMatrix ( set function) is called.

makeCacheMatrix <- function(x = matrix()) {
  
  xinv <- x
  setMatrix <- function(y){
    x <<- y
    xinv <<- x
  }
  getMatrix  <- function() x
  getInverse <-  function() xinv
  setInverse  <- function(minverse) xinv <<- minverse
  list(setMatrix = setMatrix,getMatrix = getMatrix,getInverse=getInverse,setInverse=setInverse)

}


## The below function checks first to see if a cached copy of the inverse matrix
## is available. if one is found that is retuned else the solve function is called
## to obtain the inverse  matrix and then it's cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  mat1 <- x$getMatrix()
  inv <- x$getInverse()
  if (!identical(mat1,inv))
  {
#        message("Getting Cached Value")
    return(inv)
  }
#    message("Solving and Cahcing the inverse matrix")
  mat1 <- x$getMatrix()
  inv <- solve(mat1)
  x$setInverse(inv)
  inv
}
