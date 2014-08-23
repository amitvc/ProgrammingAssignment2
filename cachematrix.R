## makeCacheMatrix initializes the matrix variable 
## and creates a list of objects that attaches setsolve, getsolve, get,set methods
## to this matrix variable.
## Sample Usage : matObj <- makeCacheMatrix(matrix(1:4,2,2))
## use matObj$get() -- returns the 2,2 matrix
## use matObj$getsolve() -- return NULL because there is nothing set yet.
## user matObj$setsolve(value) -- will be called to set the inverted value of matrix.
 makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ##Define get function which returns the matrix.
  get <- function(){ x }
  
  ##Define function which sets the calculated value of solve function.
  setsolve <- function(solve) { m <<- solve }
  ## Gets the calculated solve value.
  getsolve <- function() { m }
  ## Returns a list of function pointers.
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve function uses a caching mechanism enabled by the makeCacheMatrix function.
## For cacheSolve to enable the caching mechanism a prerequsite is call to makeCacheMatrix and
## pass the value returned by makeCacheMatrix to cacheSolve function.
## sample usage -
## matObj <- makeCacheMatrix(matrix(1:4,2,2))
## cacheSolve(matObj)
## First time cacheSolve is called for matObj it will look into the cache to see if the value is already
## calculated. Since it has not been calculated it will calculate it using R's solve() function and then 
## cache the value. 
## Every call to cacheSolve function after that will use the cached solve value. 
## Remember it is critical to note that the cached value is only for the matObj created above using 
## makeCacheMatrix call. If you create a new matrix using makeCacheMatrix call the above process will repeat.
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
