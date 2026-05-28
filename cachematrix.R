## Put comments here that give an overall description of what your
## first, it stores a matrix, then it stores the inverse of that matrix, then it retrieves the matrix, before finally retrieves the cached inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  getinverse <- function() inv
  
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )

}


## Write a short comment describing this function: 
##first, the function checks if the function isn't already cached. If not, then it computes the inverse using solve(), stores it in the cache, and thenreturns the inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("obtaining cahed inverse matrix")
    return(inv)
  }
  
  data <- x$get()
  
  inv <- solve(data, ...)
  
  x$setinverse(inv)
  
  inv  ## Return a matrix that is the inverse of 'x'
}
