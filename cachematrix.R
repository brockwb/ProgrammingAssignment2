## Complementary functions to create a special "matrix" object able to cache its inverse and
## determine whether the inverse has been cached.  If the inverse has been cached, the cached value
## is returned. Otherwise, the inverse is calculated and returned.

## Function to create a special "matrix" able to cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set<-function(y){
      x <<- y
      m <<- NULL
  }
    get<-function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}

## Function to retrieve and return cached inverse if present or
## to calculate and return inverse if cache is NULL

cacheSolve <- function(x=matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  
    m <- x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
  }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}
