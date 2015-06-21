## Put comments here that give an overall description of what your
## functions do

## This creates the matrix and calculates the solve and stores both in the global variables

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get<- function()x
  setsolve <- function (solve) m <<- solve
  getsolve <- function () m
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
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
}
