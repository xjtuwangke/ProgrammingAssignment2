## Author: Kevin
## Date: 2014-12-08
## Description: functions of makeCacheMatrix & cacheSolve

## Function: makeCacheMatrix
## Arguments:
##    self: matrix square matrix whose inverse matrix will be cached
## Returns: list retVal: an 'matrix' object with setter and getter functions for 
##                       caching its inverse matrix
##    retVal$set: function   update self to a new matrix
##    retVal$get: function   get the matrix
##    retVal$setInverse: function   set inverseMatrix
##    retVal$getInverse: function   get inverseMatrix
##    retVal$getInverseOrCreate: function  get cached inverse or create one

makeCacheMatrix <- function(self = matrix()) {
  
  ## @var inverseMatrix: matrix|NULL , cached inverse matrix in closure, 
  ##                                 NULL if nothing cached
  inverseMatrix <- NULL
  
  ## @var setter: function , update self to a new matrix, meanwhile, clear cache
  setter <- function(matrix){
    self <<- matrix
    inverseMatrix <<- NULL
  }
  
  ## @var getter: function , getter function for self
  getter <- function(){
    self
  }
  
  ## @var setInverse: function , set calculated inverse matrix to memory
  setInverse <- function(inverse){
    inverseMatrix <<- inverse
  }
  
  ## @var getInverse: function , get calculated inverse matrix to memory
  getInverse <- function(){
    inverseMatrix
  }
  
  ## @var getInverseOrCreate: function , get cached inverse or create one
  getInverseOrCreate <- function(...){
    if( is.null(inverseMatrix) ){
      inverseMatrix <<- solve(self,...)
    }
    else{
      message("getting cached data")
    }
    return(inverseMatrix)
  }
  
  
  list(
    set = setter ,
    get = getter ,
    setInverse = setInverse ,
    getInverse = getInverse ,
    getInverseOrCreate = getInverseOrCreate
  )
}


## Function: cacheSolve
## Arguments: x, makeCacheMatrix the matrix to calcalute inverse
## Description: Return a matrix that is the inverse of 'x'

cacheSolve <- function(x,...) {
  ## try to get inverse from cache
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  ## bad luck , so calculate by solve() and cache
  m <- x$get()
  inverse <- solve(m)
  x$setInverse(inverse)
  return(inverse)
  ## or
  ##return(x$getInverseOrCreate())
}
