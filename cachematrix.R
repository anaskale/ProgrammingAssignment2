## Put comments here that give an overall description of what your
## functions do

## With this function, firstly, we set and then get the value of a matrix. 
## After that , we set the value of the inverse of the matrix and then
## we get this value. In other words, this function creates a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function()x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv 
  
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)

}

## In this function we calculate the inverse of the special "matrix"
## that we have created in the previous function . 
## We check if the inverse of the matrix has already been calculated before and
## if we find out that there is available in the cache(and the matrix
## has not changed) then we skip the computation and we get immediately
## the inverse from the cache. In the case that the inverse has not been 
## calculated before, we calculate the inverse of the matrix with the
## function solve() and we set the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  mat <- x$get()
  
  ##we calculate the inverse with the solve function
  inv <- solve(mat,...)
  
  #we set the inverse to the object 
  x$setInverse(inv)
  
  #we return the matrix 
  inv
       
}

