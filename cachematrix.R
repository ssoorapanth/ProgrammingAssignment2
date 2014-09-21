## Put comments here that give an overall description of what your
## functions do

### The goals of makeCacheMatrix and cacheSolve are to compute the inverse of a matrix 'x'. 
### If the inverse of matrix 'x' hasn't been computed, it will be computed and saved in the global environmnet. 
### If the inverse of matrix 'x' has already been computed, then it will be rertrieved from the global environmnent.  

### Here are the command lines showing how to use the functions: 

### > A <- matrix(c(1,1:8),3,3) # create a 3-by-3 numeric matrix named 'A' with values 1,1,2,3,4,5,6,7,8.
### > cache_A <- makeCacheMatrix(A) # create a special matrix using makeCacheMatrix 
### > cacheSolve(cache_A) # call cacheSolve function to compute or retrieve the inverse of matrix A. The function should return the inverse of A. 


## Write a short comment describing this function
### makeCacheMatrix creates a special matrix which is actually a list of functions that can be use to set and retreive the inverse of matrix 'x' in the global environment
 
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL ## assign NULL to i (i will contain the inverse of matrix 'x') 
  set <- function(y) {
    x <<- y ## set the value of matrix 'x' in the global environment. 
    i <<- NULL ## assign NULL to i in the global environment
  }
  get <- function() x ## retreive matrix 'x'  
  setInverse <- function(inverse) i <<- inverse ##set the inverse of matrix 'x' to i in the global environment
  getInverse <- function() i ## retreive the inverse from the global environment
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) ## create a list with 4 functions using to set and retrieve the inverse of matrix
}

## Write a short comment describing this function
### cacheSolve computes the inverse of the matrix. If the inverse has already been computed and saved in the global environment, it will then retrieve the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getInverse() ## retreive the inverse of matrix 'x' that are saved in the global environment
  if(!is.null(i)){ ## if the value of i (i.e. the inverse of matrix 'x') is NOT null, then print message "getting cached data" and ruturn the value of i.
    message("getting cached data")
    return(i) 
  }
  data <- x$get() ## if the value of i is NULL, then get matrix 'x' 
  i <- solve(data) ## compute the inverse of matrix 'x'
  x$setInverse(i) ## call the function 'setInverse' to set the value of the inverse in the global environment
  i ## return i
}
