# Generates a matrix used to cache results
makeCacheMatrix <- function(x = matrix()) {
  # Cached inverse
  inv <- NULL
  
  # Set the cached data
  setCache <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  
  # Get the cached data
  getCache <- function() x
  
  # Set the cached inverse
  setInverse <- function (solve) inv <<- solve
  
  # Return the cached inverse
  getInverse <- function () {
      inv
  }
  
  # Return the list
  list (setCache = setCache, getCache = getCache,
        setInverse = setInverse, getInverse = getInverse)
}

# Compute the inverse of the matrix
cacheSolve <- function(x, ...) {
  # Is the inverse cached?  
  inv <- x$getInverse()
  
  # If we found a cached inverse, return it
  if (!is.null(inv))
  {
    message("Returning cached data")
    return (inv)
  }
  
  # Get the data cache
  data <- x$getCache()
  
  # Solve the matrix
  inv <- solve(data)
  
  # Store the cached inverse
  x$setInverse(inv)
  
  # Return the inverse
  inv
}
