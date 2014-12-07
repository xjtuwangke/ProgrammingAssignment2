## File: test_cachematrix.R
## Author: kevin
## Date: 2014-12-08
## Description: testcase for cachematrix

source('cachematrix.R')

x1<-matrix(c(1,3,4,2,5,6,2,3,4),nrow=3,ncol=3)
x2<-matrix( sample( 1:16 , replace = TRUE ), nrow=4 , ncol=4 )
x3<-matrix( sample( 1:25 , replace = TRUE ), nrow=5 , ncol=5 )

test(x1)
test(x2)
test(x3)

## Function: test
## Arguments: x matrix
## Description: test the cachematrix.R
test <- function( x ){
  m <- makeCacheMatrix( x )
  ## solve(x) should equals to cacheSlove( makeCacheMatrix(x) )
  compare( solve(x) , cacheSolve( m ) )
  
  m$set( solve(x) )
  
  ## while m changes, the cached value should be updated when acquiring new 
  ## cacheSolve(m)
  compare( solve( solve( x ) ) , cacheSolve(m) )
  
  ## chage this matrix
  x <- x + pi
  m$set( x )
  compare( solve(x) , cacheSolve(m) )
  
}

compare <- function( a , b ){
  if( identical( a , b ) ){
    message('ok')
  }
  else{
    message('error! test failed')
    print( a )
    print( b )
  }
}