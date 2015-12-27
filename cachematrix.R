## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly. 
## The functions will cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix: sets the assined object for example my_matrix with the value of Matrix x and 
## empty the inverse matrix 
## makeCacheMatrix: also creates a list containing containing 4 functions: 
## 1- set
## 		sets the object again if needed for example my_matrix with the value of Matrix x and empty the inverse matrix
## 2- get
##      returns the Matrix stored in the object my_matrix
## 3- setinv 
##      sets the inverse matrix
## 4- getinv
##		gets the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
		x_inv <- NULL
        set <- function(y) {
                x <<- y
                x_inv <<- NULL
        }
        get <- function() x
        setinv <- function(my_m) x_inv <<- my_m
        getinv <- function() x_inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. 

## 1- If the inverse has already been calculated the a message "getting cached data" is printed and
## the stored value of matrix inverse is returned 
## 2- If the inverse value is empty then the function will calclute the inverse using the function solve

cacheSolve <- function(x, ...) {
		x_inv <- x$getinv()
        if(!is.null(x_inv)) {
                message("getting cached data")
                return(x_inv)
        }
        data <- x$get()
        x_inv <- solve(data, ...)
        x$setinv(x_inv)
        x_inv
}
## Example:
## > my_matrix <- makeCacheMatrix(matrix (c( 4, 3, 3, 2), 2,2))
## > my_matrix$get()
##      [,1] [,2]
## [1,]    4    3
## [2,]    3    2

## In the first time to run cacheSolve the inverse is to be calculated 
## > cacheSolve(my_matrix)
##      [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4

## In the second time to run cacheSolve the inverse is to be retrived from the stored inverse
## > cacheSolve(my_matrix)
## getting cached data
##      [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4

