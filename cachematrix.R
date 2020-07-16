#Author:Kumar Anjul
#Date & Time:17 JULY 2020, 12:11 AM

# This code helps cache values of inverse of a matrix to remove redundancy of large time and
# space requirements

# In order to create a cached inverse for a matrix call function makeCacheMatrix() first and feed
# the results of this function to cacheSolve() function to get the inverse. New value of 
# matrix can be set using the setMatrix() function on the object to makeCacheMatrix() call 
# was placed and inverse of new matrix can be cached using cacheSolve() function.


# The function makeCacheMatrix creates a special matrix which is basically list containing
# four functions for setting the matrix to a new value, getting the matrix that is stored
# in cache, setting new value of inverse and getting inverse matrix of existing matrix
# from cache. Each time a new matrix is set using set function, the value of new inverse is not
# calculated until cacheSolve() function is called thus inv is initialized to NULL everytime a
# matrix is set using function makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function(){
    x
  }
  setInv<-function(y){
    inv<<-y
  }
  getInv<-function(){
    inv
  }
  return(list(setMatrix=set,getMatrix=get,setInverse=setInv,getInverse=getInv))
}


# This function takes input as special matrix created using makeCacheMatrix() function and extracts 
# value of inverse using getInverse() function called on special matrix. If it is NULL, then inverse 
# is calculated and set in using setInverse() function called on special matrix after calculating
# inverse using Inverse() function from matlib library and returned to function call. Else, 
# cached value of inverse is returned.
cacheSolve <- function(x, ...) {
  inverse<-x$getInverse()
  if(is.null(inverse)==FALSE){
    print("Getting cached value of inverse")
  }else{
    matrix<-x$getMatrix()
    inverse<-Inverse(matrix)
    x$setInv(inverse)
  }
  return(inverse)
}
