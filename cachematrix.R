## Create a special matrix which stores the matirx and its inverse. 
## When calculating the inverse, check cache first. If the inverse 
## is cached, return the cached inverse. If not, calculate and 
## cache the inverse. 


## The function creates a special matrix which stores data and inverse.
## It takes a matrix and returns a list of functions to get, set matrix
## and get, set the inverse. 
makeCacheMatrix <- function(x = matrix()) { #default empty matrix
        inverse <- NULL
        set <- function(y) {
                # <<- used to assign to different environment
                x <<- y
                inverse <<- matrix()
        }
        get <- function() x  # no {}
        setInverse <- function(inv) inverse <<- inv
        getInverse <- function() inverse
        # return a list of functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The function calculates the inverse of a special matrix
## returned by makeCacheMatrix. It takes an object of
## makeCacheMatrix, checks if cached inverse exists, and 
## then returns the cached inverse if exists, calcuates 
# and cache the inverse if not.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)  # return cached inverse
        }
        data <- x$get()
        inverse <- solve(data)  # calculate inverse
        x$setInverse(inverse)  # cache inverse
        inverse
}
