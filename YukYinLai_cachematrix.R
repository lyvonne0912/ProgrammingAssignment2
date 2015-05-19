## Yuk Yin Lai R Programming Assignment 2


## the makeCacheMatrix function creates a special "matrix" object that can cache the inverse of the input matrix
## the cacheSolve function figures the inverse of the special "matrix" returned by makeCacheMatrix.  If the inverse of the special matrix has already been cached, it would skip the computing part and return the inverse of the special "matrix" from cache

makeCacheMatrix <- function(z = matrix()) {
  n <- NULL
  set <- function(y) { ##set the value of the matrix
    z <<- y 
    n <<- NULL
  }
  get <- function() z ##get the value of the matrix
  setinv <- function(solve) n <<- solve ##set the value of the inverse
  getinv <- function() n  ##get the value of the inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function

cacheSolve <- function(z, ...) {
  ## Return a matrix that is the inverse of the cached matrix z
  n <- z$getinv()
  if(!is.null(n)) { ## if inverse is already cached
    message("getting cached data")
    return(n)  ## will returned the cached data
  }
  data <- z$get() ## will retrieve the input matrix
  n <- solve(data, ...) ##inverse the matrix #data #solve(data) %*% data #data[,1] #solve(data) %*% data #mean(data, ...)
  z$setinv(n)
  n
}
