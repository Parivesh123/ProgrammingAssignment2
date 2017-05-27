## make matrix assign to variable x and initialize inv to Null

makeCacheMatrix<- function(x= matrix()){
    inv<- NULL
    set <- function(y) {  ## if user want to reset matrix
           x <<- y        ## reassign new matrix to x
          inv <<- NULL    ## reinitialize inv to NULL
      }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,get = get,
         setInverse = setInverse,
         getInverse = getInverse)
     }

## Return a matrix that is inverse of 'x'

cacheSolve <- function(x, ...) {
             inv <- x$getInverse()
              if (!is.null(inv)) {  ## if user had calculated the same matrix before 
                    message("getting cached data")
                    return(inv)     ## return old result(inv) directly
                }
             mat <- x$get()         ## otherwise, get the uncalculated matrix
             inv <- solve(mat, ...) ## calculate the inverse matrix
             x$setInverse(inv)      ## reassign inverse matrix
            inv                     ## print the inverse matrix   
         }
