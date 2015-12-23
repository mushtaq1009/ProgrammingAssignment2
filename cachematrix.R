## These functions will cache the inverse of matrix instead of computing repeatedly
## This function will do the inverse of the matrix by utilizing lexical scoping 
## This function will create the matrix object

makeCacheMatrix <- function(x = matrix()) { 
    invmat <- NULL
    set <- function(y) {
    x <<- y
    invmat <<- NULL
}
get <- function() x
setInverse <- function(inverse) invmat <<- inverse
getInverse <- function() invmat
list(set = set,
     get = get,
     setInverse = setInverse,
     getInverse = getInverse)

}


##This function will calculate the inverse of the matrix created by makeCacheMatrix function
##and if it has been already calcuated then it uses the cache inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmat <- x$getInverse()
  if (!is.null(invmat)) {
    
              message("getting cached data")
              return(invmat)
    
  }
  mtx <- x$get()
  invmat <- solve(mtx, ...)
  x$setInverse(invmat)
  invmat
}
