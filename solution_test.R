#code to test the 2 functions above
setwd("C:/Self_Dev/coursera_classes/R_Programming_JohnHopkin_U/assignment_2/gitrepo/ProgrammingAssignment2")
source("cachematrix.R")
mat <- matrix(1:4, ncol=2, nrow=2)
mat
x <- makeCacheMatrix(mat)
cacheSolve(x) #calculate new inversed matrix
cacheSolve(x) #this should get the inversed matrix from cached

mat2 <- matrix(11:14, ncol=2, nrow=2)
x2 <- makeCacheMatrix(mat2)
cacheSolve(x2) #calculate new inversed matrix
cacheSolve(x2) #this should get the inversed matrix from cached

mat3 <- matrix(c(2,1,1,1,2,1,2,1,2), ncol=3, nrow=3)
mat3
x3 <- makeCacheMatrix(mat3)
cacheSolve(x3) #calculate new inversed matrix
cacheSolve(x3) #this should get the inversed matrix from cached

#a not invertible matrix
mat4 <- matrix(1:6, ncol=2, nrow=3)
mat4
x4 <- makeCacheMatrix(mat4) #error raise
cacheSolve(x4) #calculate new inversed matrix
cacheSolve(x4) #this should get the inversed matrix from cached
mat4 * cacheSolve(x4)#Error in mat4 * cacheSolve(x4) : non-conformable arrays

#check result a matrix multiply with its inverse = identity matrix
# Here's an example of identity matrix
#    1    0    0
#    0    1    0
#    0    0    1
mat  %*% cacheSolve(x)
mat2 %*% cacheSolve(x2)
mat3 %*% cacheSolve(x3)
