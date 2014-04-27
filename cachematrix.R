## Put comments here that give an overall description of what your
## functions do
##
## These functions are to compute a inverse of matrix and chache in the object.
## When the same matrix is required to compute inverse of matrix, the values exist in the catche
##will be re-used insead of re-compute it again.
##
## Write a short comment describing this function
##
## This fuction contails four functions and return a list with functions to
## get and set the values in in object "m".
## 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(mtx) m <<- mtx
  getInvMatrix <- function() m
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)

}


## Write a short comment describing this function
##
##This function is to compute or get the inverse matrix in cache (which already computed
## and stored.
## First it will check wheather the value is exist in cache if so, it will return the value.
## If value is not exist then it will compute the inverse matrix using "Solve" function,
## and then will be set into the cache by calling "setInvMatrix" function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  
  m <- x$getInvMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInvMatrix(m)
  m
}
