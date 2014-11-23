#####################################################
## Didier Bouba Ndengue
## 11/22/2014
## ASSIGNMENT II
##
## Program to compute the inverse of a matrix
##
## This program is designed to get as an input a 2x2 matrix and compute its inverse
##and cache it for future retrieval. If the same matrix is input again, the program
## calls the cache and displays the saved inverse matrix.
## Write a short comment describing this function

#makeCache matrice declare a function that takes as input a 2x2 matrix.
#Declare Getters and Setters that will save the inverse matrice
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {#function to set the inverse if it was previously ran
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse##Set the inverse matrix for future reference
    getinverse <- function()m #declare the function to retrieve the inverse if already calculated
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)##display the different function on the console

}


## This function takes an input the inverse matrix and tries to check if the inverse was previously
## calculated. In case it was calculated, it displays "getting cahed inverse data"
## in case it was not calculated, it calculates the inverse, caches it in "setinverse" function and
## displays the inverse back to the user.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()##get the inverse from the previous function
        if(!is.null(m)) {##if inverse was previously calculated display it again
            message("getting cached inverse matrice")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)##Solve the inverse matrix
        x$setinverse(m)##Set the inverse in the setinverse function for future reference
        m ##display the inverse matrix
}
