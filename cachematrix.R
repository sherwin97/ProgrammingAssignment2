## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function will create a special matrix object to cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
  inverse1 <- NULL #set inverse1 as null object
  set <- function(y) {
    x <<- y # make x to  y
    inverse1 <<- NULL #make inverse1 back to null
  }
  get <- function() x #get the matrix
  setinverse <- function(inverse2) inverse1 <<- inverse2 #get inverse2 matrix back to inverse1 matrix
  getinverse <- function() inverse1 #get result 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) #set list
}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by function above.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse1<- x$getinverse() #get the inverse matrix 
  if(!is.null(inverse1)) {
    message("getting cached data")
    return(inverse1) #load back cached matrix and return it if there is no change
  }
  data <- x$get() #if null is result, get new matrix input 
  inverse1<-solve(data, ...) #calculate using solve function
  x$setinverse(inverse1) #set new result as matrix 
  inverse1 #return matrix 
}
