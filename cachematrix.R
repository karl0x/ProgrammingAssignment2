

makeCacheMatrix <- function(x = matrix()) {
        
        xminv <- NULL # this is where the result of inversion is stored
        set <- function(y) {
                x <<- y
                xminv <<- NULL # Here is inisianilised the inverse matrix 
        }
        
        get <- function() x # it return the matrix
        setInv <- function(inv) xminv <<-minv # Set the inverse Matrix
        getInv <- function() xminv              # return  the inverse Matrix
        list(set = set, get = get, setInv = setInv, getInv = getInv) # these are the functions to work the inverse matrxi
}

cacheSolve <- function(x, ...) { #calculate the inverse of the matrix
        m <- x$getInv()                 #get the inverse maytrix on cache
        if(!is.null(m)) {               # if there is an inverse matrix on cache
                message("getting cached data")
                return(m)               # return the calculated inversion 
        }
        message("Calculting the inverse Matrix")
        
        data <- x$get()         # get the matrix to calculate its inverse
        m <- solve(data)        # calculate the inverse matrix
        x$setInv(m)             # set the inverse matrix 
        m 
}
