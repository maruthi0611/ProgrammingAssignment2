## This function is majorly used to reduce the computation time of Matrix Inversion by
## creating a cached version. 
## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  ## inv will store the cached inverse matrix
  inv <- NULL
  ## Matrix is set here
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## Gets the matrix
  get <- function() x
  
  ## Inverse is set here
  setinverse <- function(inverse) inv <<- inverse
  
  ## Gets the inverse
  getinverse <- function() inv
  ## Return the matrix with redefined functions
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function assumes that the matrix is always invertible.It computes the inverse of the matrix.
## If the inverse is already calculated before, it returns the cached inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  ## If the calculated inverse is not in cache, it is calculated afresh below
  data <- x$get()
  inv <- solve(data)
  
  ## Cache the inverse
  x$setinverse(inv)
  
  ## Return it
  inv
}

## Example:
## > A = matrix(c(1,0,5,2,1,6,3,4,0), 
##    nrow=3, 
##    ncol=3)
## > mx = makeCacheMatrix(A)
## > mx$get()
##   [,1] [,2] [,3]
##[1,]    1    2    3
##[2,]    0    1    4
##[3,]    5    6    0

## No cache in the first run
## > cacheSolve(mx)
##     [,1] [,2] [,3]
##[1,]  -24   18    5
##[2,]   20  -15   -4
##[3,]   -5    4    1
## Second run
## > cacheSolve(mx)
## getting cached data.
##     [,1] [,2] [,3]
##[1,]  -24   18    5
##[2,]   20  -15   -4
##[3,]   -5    4    1