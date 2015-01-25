## Put comments here that give an overall description of what your
## functions do:
## These functions calculate the inverse of an invertible matrix. The cool part about 
##these function is that they only calculate the inverse on if the inverise is not avalable 
##in the cache. Otherwise, it just returns the cached inverse value.
## Run the following code to see them in action:
# mat1<-matrix(1:4,2,2)
# mcm<-makeCacheMatrix(mat1)
# cacheSolve(mcm)
# cacheSolve(mcm)
# idmat<-matrix(c(1,0,0,1),2,2)
# mcm$set(idmat)
# cacheSolve(mcm)
# cacheSolve(mcm)

## Write a short comment describing this function
# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(mat_inv) inv <<- mat_inv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function
# The following function calculates the inverse of the special "matrix" created with the above function.
# However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse
# from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets
# the value of the invarse in the cache via the setinv function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
  
}
