## Put comments here that give an overall description of what your
## functions do

## This creates the matrix and calculates the solve and stores both in the global variables

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y ## Sets the value in the parent environment
    m <<- NULL
  }
  get<- function()x
  setsolve <- function (solve) m <<- solve ## assigns the value of the inverted matrix
  getsolve <- function () m
  ## returns all the functions inside the main function, list is used, since this is supposed to return muliple functions
  
  list (set = set, 
        get = get, 
        setsolve = setsolve, 
        getsolve = getsolve)

}


## This reads the cache, and if it is empty it calls the makecachematrix to get the solve value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if (!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  
  ## gets the data which was previously assigned
  data <- x$get()
  ## creates the inverse of the matrix with the solve function
  m <- solve(data)
  x$setsolve(m)
  m
}

##Passing a matrix to the vectors and getting the inverse

convmat <- matrix(c(1,2,3,4),nrow=2,ncol=2)
invertedmatrix <- makeCacheMatrix(convmat)
cacheSolve(invertedmatrix)