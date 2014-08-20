## The following functions create a special matrix that can cache its inverse
## and compute the inverse of the special matrix

##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  if(!is.matrix(x))                  ## tests if the given input is a matrix value.
  {len=trunc(sqrt(length(x)))      ##truncates the value to get a square matrix
   x <- matrix(x,nrow=len,ncol=len)     ## if not converts to matrix
   if(!is.matrix(x))              
     stop("Give a matrix value")     ## if cannot convert, stop the function.
  }
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


##This function computes the inverse of the special
##"matrix" returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then 
##cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  if (det(data)!=0)       ## if determinant of matrix is zero, inverse cannot be computed.
    i <- solve(data)
  x$setinv(i)
  i
}

