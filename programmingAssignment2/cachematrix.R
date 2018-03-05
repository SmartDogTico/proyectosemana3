## PAB: This function creates a special "matrix" object that can cache its inverse.

## PAB: This function called makeCacheMatrix creates a list containing a function and
## sets and calculates:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## PAB: This function returns the inverse of the matrix. First of all, it 
##checks if the inverse has already been computed and calculated. 
##If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

x = rbind(c(20, -10/85), c(-9/2, 7))
m = makeCacheMatrix(x)
m$get()
cacheSolve(m)
?inver
