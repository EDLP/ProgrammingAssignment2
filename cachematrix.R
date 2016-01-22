## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.





makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  setinv_matrix <- function(inverse) inv_matrix <<- inverse
  getinv_matrix <- function() inv_matrix
  list(set = set, get = get,
       setinv_matrix = setinv_matrix,
       getinv_matrix = getinv_matrix)
}




cacheSolve <- function(x, ...) {
  inv_matrix <- x$getinv_matrix()
  
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  
  mat.data <- x$get()
  inv_matrix <- solve(mat.data, ...)
  
  x$setinv_matrix(inv_matrix)
  
  return(inv_matrix)
}


