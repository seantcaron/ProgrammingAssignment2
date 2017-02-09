# Programming Project 2 -- Matrix with Caching
#  Sean Caron (scaron@umich.edu)
# 
# Function makeCacheMatrix constructs a special "cacheMatrix" object
#  given a common R matrix as an argument. This cacheMatrix has various
#  member functions to set and retrieve its value and the value of its
#  inverse. The inverse is cached.
#
# Function cacheSolve queries the cache to see if the inverse has already
#  been found for the argument matrix. If so, the cached inverse is returned
#  to the caller. Otherwise, the inverse is calculated, the cache is updated
#  and the result is returned to the caller.

# Given a common matrix as an argument, construct a cacheMatrix object
makeCacheMatrix <- function(x = matrix()) {
  # Initialize cache to NULL
  i <- NULL
  # Method to set the cacheMatrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # Method to get the cacheMatrix
  get <- function() x
  # Method to set the inverse of cacheMatrix
  setinvers <- function(invers) i <<- invers
  # Method to get the inverse of cacheMatrix
  getinvers <- function() i
  # Return the constructed object
  list(set = set, get = get,
       setinvers = setinvers,
       getinvers = getinvers)
}

# Return a matrix that is the inverse of cacheMatrix 'x'
#  It is assumed that the argument will be invertible

cacheSolve <- function(x, ...) {
  # Query the cache
  i <- x$getinvers()
  # If the cache is populated, return that
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  # Otherwise, retrieve the current value in cacheMatrix
  data <- x$get()
  # Calculate the inverse, set the inverse in cacheMatrix
  i <- solve(data, ...)
  x$setinvers(i)
  # And return the inverse
  i
}
