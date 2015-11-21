## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # Define mInv to be NULL
  mInv <- NULL
  
  # Define set function to accept any data user provides
  set <- function(y)
  {
    mDim <- dim(y)
    if (mDim[1] != mDim[2]) {
      stop('Square matrices only!')
    }
    
    # Assign user provide matrix to variable 'm'
    x <<- y
    # reset mInv to NULL since the matrix changed
    mInv <<- NULL
  }
  
  # Define get function to get the matrix
  get <- function(){
    x
  }
  
  # Define setInv function to cache the inverse to variable mInv
  setInv <- function(inv) {
    mInv <<- inv
  }
  
  # Define getInv function to get the inverse stored in variable mInv
  getInv <- function () {
    mInv
  }
  
  # Define list to return these function handles
  list(set=set, get=get, setInv = setInv, getInv = getInv)
  
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # Get the cached inverse
  mInv <- x$getInv()
  # Check if it is not a null - it has been calculated already
  if (!is.null(mInv)) {
    message("Getting Cached Inverse")
    # Return the inverse and stop execution of this function
    return(mInv)
  }
  # if inverse hasn't been calculated yet, then get the matrix
  data <- x$get()
  # calculate the matrix inverse using the solve function and any additional solve parameters
  mInv <- solve(data, ...)
  # Cache the inverse
  x$setInv(mInv)
  # Return the special vector
  mInv
}
