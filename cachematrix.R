## Given an invertible matrix, (witch this methods won't check) makeCacheMatrix will cache
## the matrix and will create a list of methods for further use.
## cacheSolve first will check if the inverse is already chached, if it is, it will be retrieve
## if is not, it will calculate the inverse and return it.  
##
##  Usage sample:
##       ma <- matrix(runif(1:5*5), 5,5) #retrieve a random 5x5 matrix
##       macache <- makeCacheMatrix(ma)  #cache the matrix and create the list of methods
##       inverse <- cacheSolve(macache)  #calculate the inverse and retrieves it
##


#Create a list of methods used to store a matrix cached
makeCacheMatrix <- function(x = matrix()) {

      #initialize the varible to store the inverse
      inv <- NULL 
      #sets the matrix and clear the inverse
      set <- function(y) {
      x <<- y
      inv <<- NULL
      }
      
      #Retrieve the cached matrix
      get <- function() x
      #Sets the inverse matrix
      setinv <- function(solve) inv <<- solve
      #Retrive the cached inverse
      getinv <- function() inv
      #create a list of methods
      list(set = set, 
           get = get,
           setinv = setinv,
           getinv = getinv)
    
}


## Write a short comment describing this function
## Solve the inverse matrix with the list of metohsd from makeCacheMatrix
cacheSolve <- function(x, ...) {
# Return a matrix that is the inverse of 'x', the '...' corresponds to argumentes for solve(x, ...)
    #Retrieves the inversa
    inv <- x$getinv()
    #If it's in the cache, return it
    if(!is.null(inv)) {
      return(inv)
    }
    #If not in cache, calculate it.
    data <- x$get()
    inv <- solve(data, ...)
    #And store the inverse
    x$setinv(inv)
    
    inv
}
