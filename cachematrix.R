#########################################################################
## Functions to cache a matrix and it's inversion                      ##
#########################################################################
##
##     function makeCacheMatrix() :
##
##              creates a new list, which contains 4 function elements:
##              $get() - will return the original matrix
##              $set() - can be use to set a new matrix
##              $setinversed() - function to set the inverted matrix
##              $getinversed() - will return the inverted matrix
##
##     function makeCacheMatrix() :
##
##              a function which can be used to calculate inverse of 
##              the above kind of list matrix
##
#########################################################################
##
## How to use these functions
## Creating a cached matrix
##     source("cachematrix.R")
##     a<-matrix(1:4,nrow=2,ncol=2)
##     mat<-makeCacheMatrix(a)

## now following line should return null
##     mat$getinversed()

## calculate the inversion
##     cacheSolve(mat)
## now
##     mat$getinversed wil return 
##        [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5
##
#########################################################################



#########################################################################
## This function encupsulates a matrix and it's inversion,
## function returns a list of 4 member functions
##
## Note! In set function the cached inversion is cleared only 
## if the new matrix y isn't identical with existing matrix x
#########################################################################
makeCacheMatrix <- function(x = matrix()) {
        
        cachedInversion <- NULL
        
        set <- function(y) {
                if (!identical(x,y)) {
                        x <<- y
                        cachedInversion <<- NULL
                }
                
        }
        
        get <- function() x
        
        setinversed <- function(solvedData) cachedInversion <<- solvedData
        
        getinversed<- function() cachedInversion
        
        list(set = set, get = get,
             setinversed = setinversed,
             getinversed = getinversed)
        
                        
}


#########################################################################
## Return a matrix that is the inverse of 'x' and cache it into 'x'
##
## Note! This function assumes that x is invertible
## and matrix is square (count of columns is same as count of rows)
#########################################################################
cacheSolve <- function(x, ...) {        

        ## check that x is actual correct data type ie. list      
        ## and the list contains correct functions
        if (is.list(x)) {
                if (identical(names(x),c("set","get","setinversed","getinversed"))){
                        ## first we get the data out of x
                        ## if it contains the inversed matrix, return it        
                        solved <- x$getinversed()
                        if(!is.null(solved)) {
                                message("getting the already inversed matrix")
                                return(solved)
                        }
                        
                        ## if the inverted matrix isn't calculated, we calculate it
                        ## and store it into the x
                        data <- x$get()
                        solved <- solve(data, ...)
                        x$setinversed(solved)
                        solved
                }
                else stop("x doesn't contain needed functions")
        } 
        else stop("x is not a list")             
}
