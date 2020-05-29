## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  h <- NULL         ## initializing
  set <- function(y){  ## define set function
    x <<- y     ## value in parent environment
    h <<- NULL  ##initializing in parent environment
  }
  get <- function()x  
  setInverse <- function(inverse) h <<- inverse  ## assign value of inverse in parent environment
  getInverse <- function() h 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of the matrix
  h <- x$getInverse()        ## use getInverse Feature
  if(!is.null(h)){          ## check if calculating inverse is possible 
    return(h)           ## if not: print initial matrix
  }
  mat <- x$get()     
  h <- solve(mat,...)    ## solve for inverse
  x$setInverse(h)
  h
}

### test with small example
mat <- matrix(c(2, 4, 3, 1), nrow = 2, ncol = 2)
cacheSolve(makeCacheMatrix(mat))  ## viola ;)