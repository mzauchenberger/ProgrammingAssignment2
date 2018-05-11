## This function, written by mzauchenberger, will return the inverse
## of a matrix, using the cached result if it exists.

## The first function will both create/inverse the matrix and instill the possibility
## of a cached return through the use of the <<- operator.

makeCacheMatrix <- function(x = matrix()) {
  z<- NULL
  set<- function(y) {
    x<<-y
    z<<-NULL
  }
  get<- function() x
  setinverse<- function(inverse) z<<- inverse
  getinverse<- function() z
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## The second function will attempt to compute the inverse matrix data and pull the cached data
## if it exists in the global environment via the <<- operator.

cacheSolve <- function(x, ...) {
  z<- x$getinverse()
  if(!is.null(z)) {
    message("getting cached data")
    return(z)
  }
  data<- x$get()
  z<- solve(data)
  x$setinverse(z)
  z
}
