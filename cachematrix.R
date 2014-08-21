## Author: Hai Ton
## Date: 8/21/2014
## Purpose: Calculate an inverse of a matrix. If the inverse of the matrix already
##          calculated, then pull the value from cache.
## Assumptions: the input matrix is invertible that is matrix's number of row = matrix's number of column
## References:
##    Lexical scoping: 
##       http://cran.r-project.org/doc/manuals/R-intro.html#Scope
##       http://adv-r.had.co.nz/Functions.html#lexical-scoping
##    Environment in R: 
##       http://adv-r.had.co.nz/Environments.html


# Take a matrix as parameter then create an object associated with the matrix
#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  #check if the input is matrix
  if (!is.matrix(x)){
    stop("Ipnut is not a matrix.")
  }
  
  #check if matrix is invertible. #rows == #cols
  if(nrow(x) != ncol(x)){
    stop("The matrix is not invertible.")
  }
  
  invMatrix <- NULL #this will hold the input matrix's inverse
  
  #set value of the matrix
  set <- function(y) {
    x <<- y  #save the input matrix         
    invMatrix <<- NULL  #reset matrix's inverse to NULL when new object is created
  }
  
  #return the original value of input matrix
  get <- function() {
    x
  }
  
  #calculate the matrix inverse
  setinverse <- function(solve) {
    invMatrix <<- solve  #solve is the method used to calculate the matrix's inverse
  }
  
  #get the matrix inverse
  getinverse <- function() {
    invMatrix
  }
  
  #return a list of functions along with the matrix's inverse
  list(set = set,
       get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
  
}#end function


# calculate the inverse of the matrix if matrix's inverse is currently null
#   else pull the matrix's inverse from cache.
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#  then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  #get the inverse of matrix x
  invMatrix <- x$getinverse()
  
  #if the inverse matrix of x already calculated, retrieve its from cache
  if(!is.null(invMatrix)) {
    message("getting matrix's inverse from cache.")
    return(invMatrix) #return the matrix's inverse and exit the function
  }
  
  originalMatrixData <- x$get() #get the matrix original value
  invMatrix <- solve(originalMatrixData, ...) #calculate the matrix's inverse
  x$setinverse(invMatrix)    #store the inverse matrix
  invMatrix  #return the inverse matrix
  
}#end function




# #code to test the 2 functions above, or use the solution_test.R file.
# setwd("C:/Self_Dev/coursera_classes/R_Programming_JohnHopkin_U/assignment_2/gitrepo/ProgrammingAssignment2")
# source("cachematrix.R")
# mat <- matrix(1:4, ncol=2, nrow=2)
# mat
# x <- makeCacheMatrix(mat)
# cacheSolve(x) #calculate new inversed matrix
# cacheSolve(x) #this should get the inversed matrix from cached

# mat2 <- matrix(11:14, ncol=2, nrow=2)
# x2 <- makeCacheMatrix(mat2)
# cacheSolve(x2) #calculate new inversed matrix
# cacheSolve(x2) #this should get the inversed matrix from cached

# mat3 <- matrix(c(2,1,1,1,2,1,2,1,2), ncol=3, nrow=3)
# mat3
# x3 <- makeCacheMatrix(mat3)
# cacheSolve(x3) #calculate new inversed matrix
# cacheSolve(x3) #this should get the inversed matrix from cached

# #a not invertible matrix
# mat4 <- matrix(1:6, ncol=2, nrow=3)
# mat4
# x4 <- makeCacheMatrix(mat4) #error raise
# cacheSolve(x4) #calculate new inversed matrix
# cacheSolve(x4) #this should get the inversed matrix from cached
# mat4 * cacheSolve(x4)#Error in mat4 * cacheSolve(x4) : non-conformable arrays

# #check result a matrix multiply with its inverse = identity matrix
# # Here's an example of identity matrix
# #    1    0    0
# #    0    1    0
# #    0    0    1
# mat  %*% cacheSolve(x)
# mat2 %*% cacheSolve(x2)
# mat3 %*% cacheSolve(x3)