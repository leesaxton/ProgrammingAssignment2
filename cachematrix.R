## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function first sets inv variable to null object
## It then calls another function that sets the x variable (matrix) , use of double
## arrows <<- enables code to modify variable in parent environment
## This is needed as we have a function within a function
## Outside of the set function, but still within the makeCacheMatrix function we
## get the value of the matrix



makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y 
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set= set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve function first gets the inverse matrix cahced int he prvious function.
## If this has been calculated, i.e. is not null (the ! in is.null means not null)
## it exits the calculation
## If the cached inverse matrix is null it calculates the inverse using solve function
## It then updates the cached inverse matrix and returns the inverse matrix

cacheSolve <- function (x, ...) {
  inv <- x$getInverse ()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

# job done

  
  

