
# MakeCacheMatrix is a function used to create a object which consists of 
# four functions

# 1. set function to set matrix
# 2. get function to get matrix
# 3. setInverse function to set inverse of a matrix
# 4. getInverse function to get inverse of a matrix

# makeCacheMatrix: This function creates a special "matrix" object that can 
# cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

# For example

a <- makeCacheMatrix(x = matrix(1:4,2,2))

# let's see the summary of object a

summary(a)

# The output shows it contains four functions set, get, setInverse, getInverse
# as stated previously with their functions


# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then the cachesolve should retrieve the inverse from 
# the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setInverse(inv)
        inv
}

# cacheSolve function initially checks if getInverse () function produces a inv 
# value and not NULL. If it is NULL then it calculates the inverse of matrix.
# Then the x$setInverse get the value of inv. By lexical scoping the value is 
# obtained by the setInverse and getInverse of makeCacheMatrix function. 

# When we run the same function second time, the getInverse () function produces
# a inv value which is not null and it produces an cached output rather than 
# performing inverse of a matrix.



