## Put comments here that give an overall description of what your
## functions do


makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

## Write a short comment describing this function

# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the cache
  inverse <- NULL
  
  # Function to set the matrix value and invalidate the cache
  set <- function(newValue) {
    x <<- newValue
    inverse <<- NULL
  }
  
  # Function to get the matrix value
  get <- function() {
    x
  }
  
  # Function to compute and cache the inverse of the matrix
  cacheSolve <- function(x, ...) {
    # If the inverse is already computed, retrieve it from the cache
    if (!is.null(inverse)) {
      message("Getting cached inverse")
      return(inverse)
    }
    
    # If the inverse is not cached, compute it using solve() function
    inverse <- solve(x)
    
    # Cache the inverse
    setinverse(inverse)
    
    inverse
  }
  
  # Return a list of functions
  list(set = set, get = get, cacheSolve = cacheSolve)
}



## Write a short comment describing this function

#cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
#}


