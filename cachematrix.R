## Overal description: finds inverse of matrices and cache them in case they  are needed later
## 

## Write a short comment describing this function
## first you have to feed the matrices to the "makeCacheMatrix"
## it makes a cache copy of its inverse when you call "casheSolve"




makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) m <<- inverse
      getInverse <- function() m
      list( get = get,
            setInverse = setInverse,
            getInverse = getInverse)      
}


## Write a short comment describing this function
## this function first checks if the inverse have been already caculated if so it uses the cashed copy to call it otherwise solve for inverse and chace it.
cacheSolve <- function(x) {
      ## Return a matrix that is the inverse of 'x'
      
      m <- x$getInverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data)
      x$setInverse(m)
      m
}
