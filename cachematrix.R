## Put comments here that give an overall description of what your
## functions do

## The following functions make use of the Lexical scoping of R and
## create a list of functions that get and set variables
## defined within its environment and maintain the values of these
## variables through scoping and the <<- operator

## Write a short comment describing this function

## makeCaheMatrix function creates and defines a local variable for
## the inverse calculation of the provided matrix. The list of
## functions returned are able to access variables in the parent
## environment to maintain the values of both the matrix and its inverse.

makeCacheMatrix <- function(x = matrix())
{
     # inv to hold the inverse of the provided matrix
     inv <- NULL
     set <- function (y)
     { # Function to set a new matrix to x and reset the inverse
          x<<-y
          inv<<-NULL
     }
     get <- function() x
     setinverse <- function(result) inv<<-result
     getinverse <- function() inv
     
     list(set = set
          , get = get
          , setinverse = setinverse
          , getinverse = getinverse)
}


## Write a short comment describing this function

## cacheSolve starts by calling getinverse from the list we created
## to get the inverse. If the result is NULL means the inverse was
## not calculated before and it will use the matrix to call solve.
## The result will be passed to the list's setinverse function
## to update the inverse.

cacheSolve <- function(x, ...)
{
     ## Return a matrix that is the inverse of 'x'
     inv <- x$getinverse()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinverse(inv)
     inv
}


