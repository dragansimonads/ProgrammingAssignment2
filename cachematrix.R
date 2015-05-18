## The functions below are calculating the inverse of a matrix using 
## cache to store already calcuated inverse matrix for subsequent use 
## on the same matrix.


## This function creates a special "matrix" object that will cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  d <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
 
  setsolve <- function(solve) 
  
    m <<- solve
  
  getsolve <- function() m
  
  list(set = set, 
       get = get,       
       setsolve = setsolve,       
       getsolve = getsolve
       )
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

## The function also calculates the determinat to check if the matrix has an invese 

cacheSolve <- function(x, ...) {
 
  inverse <- x$getsolve()
  if(!is.null(inverse)) {
    message("getting inverse from cached data")
    return(inverse)
  }
  
  data <- x$get()
  #verify if is a square matrix 
  if(ncol(data)!=nrow(data))
    return (NULL)
  
  #verify if determinant is !=0 so the matrix can be inverted
  d<-determinant(data)
  if(d$modulus[1]==0)
   return (NULL) ## matrix cannot be inversed
  
  inverse <- solve(data, ...)
  x$setsolve(inverse)
  inverse
}

## Examples how to verify the functions above
a <- matrix(c(1,2,3,4),2,2)
mm <- makeCacheMatrix(a)
b<-cacheSolve(mm)
#multiplying a matrix with its inverse should return the identity matrix  
if (!is.null(b)) a %*% b else message("Inversed matrix could not be calculated")

#this matrix has no inverse matrix because it is not square
a <- matrix(c(1,2,3,4,5,6),3,2)
mm <- makeCacheMatrix(a)
b<-cacheSolve(mm)
if (!is.null(b)) a %*% b else message("Inverse matrix could not be calculated")




