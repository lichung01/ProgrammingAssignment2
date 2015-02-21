##  2 functions are created to perform storing of matrix values, compute and store the inverse matrix
##  and also return result of previously cached matrix without computation if input matrix is the same matrix set
##  (1)  makeCacheMatrix
##  (2)  cacheSolve


##  Function makeCacheMatrix will use a list of functions below to create special matrix objects.
##  (1) functions to get and set the value of the matrix
##  (2) functions to getInv and setInv the value of the inverse matrix
##  (3) functions to getPrevx and setPrevx the value of the previous inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    ## Assign NULL to matrixInv in local env
    matrixInv <- NULL
    matrixPrevx <- NULL
    
    ##set function  for x, matrixInv and matrixPrevx in global env
    set <- function(y) {
        x <<- y
        matrixInv <<- NULL
        matrixPrevx <<- NULL
    }
    ## get function to return x
    get <- function() x
    
    ## setInv function for inverse matrix in global env
    setInv <- function(invPara) matrixInv <<- invPara
    ##  getInv function to return maxtrixInv
    getInv <- function() matrixInv
    
    ## setPrevx function for matrix in global env
    setPrevx <- function(Prevx) matrixPrevx <<- Prevx
    ##  getPrevx function to return maxtrixPrevx
    getPrevx <- function() matrixPrevx
    
    
    ## returning list of functions
    list(set = set,
    get = get,
    setInv = setInv,
    getInv = getInv,
    setPrevx = setPrevx,
    getPrevx = getPrevx)
}


##  This function calculates the inverse of the special matrix created from 'makeCacheMatrix' function.
##  The routine will store variable for checking previous cache matrix.
##  If same matrix result is cached, then previous cache matrix values are returned without calculation.
##

cacheSolve <- function(x, ...) {
    ## Assign working values to matrixX, matrixPrevx, matrixInv
    matrixPrevx <- x$getPrevx()
    matrixX <- x$get()
    matrixInv <- x$getInv()
    
    ## Check if previous cache matrix is the same, then return inverse cache matrix result
    if(!is.null(matrixX)) {
        if (!is.null(matrixPrevx)) {
            if (identical(matrixX, matrixPrevx)) {
                message("getting cached data")
                return(matrixInv)
            }
        }
    }
    ## Store new matrix to previous matrix for comparison
    x$setPrevx(x$get())
    ## Assign new matrix to data
    data <- x$get()
    ## Calculate inverse value for data
    matrixInv <- solve(data, ...)
    ## Set local matrixInv to global
    x$setInv(matrixInv)
    matrixInv
}
