## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	i<-NULL
	set<-function (y) {
		x <<- y
      inver <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function
## his function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getInverse()
        if (!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        mat <- x$get()
        inver <- solve(mat, ...)
        x$setInverse(inver)
        inver
}

}
