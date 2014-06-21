## These functions together can take a matrix, store the matrix and its inverse
## and then return the matrix or its inverse from the 'cached' data
##  if a new matrix is stored, the old mean will be removed but has to
## be manually re-set to be stored in cache

## In this makecachematrix function, 2 variables and 4 functions are created
## which take care of storing and retrieving the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL  ## i stores the inverse.  it is initialized to null
  
     set <- function(y) { 
          x <<- y      ## x stores the matrix passed to the set function
          i <<- NULL   ## inverse is reset to null after storing a new matrix
          ## note both x and i exist inside the makecachematrix top level
     }
     
     get <- function() x ## returns the stored matrix
     setinv <- function(inv) i <<- inv  # inversed passed to setinv is stored in i
     getinv <- function() i       # inverse stored in i is returned to getinv
     list(set = set, get = get, setinv = setinv,
          getinv = getinv) # all four functions are stored in list returend by makeCacheMatrix

}


## This function will return the cached inverse of the matrix which 
## was set using the above set function if there is a cached version,
## and otherwise it will calculate, store, and return the inverse

cacheSolve <- function(x, ...) { # x is the returned list from makeCacheMatrix
  
     i <- x$getinv()   # i is set to equal the value of i in the makeCacheMatrix function
     if(!is.null(i)) { #if the i value which is passed is not null
       message("getting cached data") # print that message to screen
       return(i)     # and return that non-null value, and exit the function
     }
     
     data <- x$get() #load the stored matrix from machCacheMatrix into local data variable
     i <- solve(data, ...) # store the inverse of the matrix locally in m
     x$setinv(i)       # now store that local inverse in to the makeCacheMatrix space
     i                #return that newly calculated inverse
}
