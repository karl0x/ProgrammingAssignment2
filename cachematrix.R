

makeCacheMatrix <- function(x = matrix()) {
        
        xminv <- NULL
        set <- function(y) {
                x <<- y
                xminv <<- NULL
        }
        
        get <- function() x 
        setInv <- function(inv) xminv <<-minv 
        getInv <- function() xminv 
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}

cacheSolve <- function(x, ...) {
        m <- x$getInv() 
        if(!is.null(m)) { 
                message("getting cached data")
                return(m) # return the calculated inversion
        }
        message("Calculting the inverse Matrix")
        
        data <- x$get()
        m <- solve(data) 
        x$setInv(m) 
        m 
}
