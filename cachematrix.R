## In the following functions, what we are trying to do is optimizing the process of finding inverse of a matrix. If we want to run
## a tool/ code in which matrix inverse needs to be calculated repeatedly, these functions help in optimizing the process. When these
## functions are called to find inverse of a matrix, it first checks if the inverse for that particular matrix has already been 
## calculated. If it has already been calculated and there is no change in the matrix, then it returns the result from the previous
## computation instead of calculating the inverse all over again. This saves some time and optimizes the process.


## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
+     set <- function(y) {
+         x <<- y
+         m <<- NULL
+     }
+     get <- function() x
+     setinv <- function(inv) m <<- inv
+     getinv <- function() m
+     list(set = set, get = get,
+          setinv = setinv,
+          getinv = getinv)
}


## The following function calculates the inverse of the special "matrix" created with the above function. However, it first checks to 
## see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it 
## calculates the inverse of the data and sets the value of the inverse in the cache via the setinv function. This function uses the
## pre-defined function solve() to calculate the inverse of a matrix.

cacheSolve <- function(x, ...) {

m <- x$getinv()
+     if(!is.null(m)) {
+         message("getting cached data")
+         return(m)
+     }
+     data <- x$get()
+     m <- solve(data, ...)
+     x$setinv(m)
+     m
}
