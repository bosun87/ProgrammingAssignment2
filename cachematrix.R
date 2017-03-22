## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix returns a list containing four functions: set(),get(),
## setInverse() and getInverse()

makeCacheMatrix <- function(x = matrix()) {
  
       inv <- NULL
       
       set <- function(y)  {
         
           x <<- y
           inv <<- NULL
       }
       
       get <- function() x
       
       setInverse <- function(i) inv <<- i
       getInverse <- function() inv
       
       list( set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Write a short comment describing this function

## cacheSolve return the inverse matrix of the special "matrix" object returned by
## makeCacheMatrix function defined above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## here 'x' is the special "matrix" object
  
       i <- x$getInverse()
       
       if(!is.null(i)) {
         
          message("getting cached data")
          return(i)
       }
       
       data <- x$get()
       i <- solve(data)
       x$setInverse(i)
       i
}
