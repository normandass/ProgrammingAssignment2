## Put comments here that give an overall description of what your
    ## functions do
    ## I simply set the input x as a matrix
    ## and then set the solved value "x" as a null
## Write a short comment describing this function
   ##"makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
  ##I changed "mean" to "solve" and "m" to "x"
  ##This function creates a special "matrix" object that can cache its inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                 message("getting cached data")
                 return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
