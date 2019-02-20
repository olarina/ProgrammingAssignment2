## Below are two functions that are used to create a special object that stores
## a numeric matrix and caches its inversion

## The first function, makeCacheMatrix creates a special "vector"
## which is really a list containing a function to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inversion
## 4. get the value of the inversion

makeCacheMatrix <- function(x = matrix())
{
    inv <- NULL
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The following function calculates the inversion of the special "vector"
## created with the makeCacheMatrix function. It first checks if the inversion
## has already been calculated. If so, it gets the inversion from the cache and
## skips the computation. Otherwise, it calculates the inversion of the data
## and sets the value of the inversion in the cache via the setinv function.

cacheSolve <- function(x, ...)
{
    inv <- x$getinv()
    if (!is.null(inv))
    {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
