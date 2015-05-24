## Function to cache the inverse of a matrix 

## Function to create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        #Set inverse to NULL to return default value unless cacheSolve is called
        inv <-NULL
        
        #Sets the value of the matrix as defined by the user
        setMatrix<-function(y){
                x <<- y
                inv <<- NULL #Initialize inv to default everytime matrix is initialized
        }
        
        #Returns matrix set as per setMatrix
        getMatrix<-function() x
        
        #Sets inverse of matrix using solve
        setInvMatrix<-function(solve) inv <<- solve
        
        #Returns inverse of matrix
        getInvMatrix<-function() inv
        
        list(setMatrix=setMatrix, 
             getMatrix=getMatrix,
             setInvMatrix=setInvMatrix,
             getInvMatrix=getInvMatrix)
}


## Function to compute the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        #Returns matrix that is the inverse of 'x'
        inv<-x$getInvMatrix()
        
        #Check if inv has a value set and return the same if found to save re-computation time
        if(!is.null(inv)){
                message("Retrieving cached data for matrix inverse...")
                return(inv)
        }
        else { #Compute value of inverse of matrix if no cached value for inv found
                matrix <- x$getMatrix()
                
                #Check if the matrix is a square matrix
                if(dim(matrix)[1] == dim(matrix)[2]) {
                        message("Computing matrix inverse for given matrix and values will be cached..")
                        #Calls solve function to compute matrix inverse for the first time
                        inv <- solve(matrix, ...)
                        
                        #Call function to cache recently computed value of matrix inverse
                        x$setInvMatrix(inv)
                        
                        #Display the inverse of matrix
                        inv
                }
                else {
                        message ("'cacheSolve' is currently limited to computing inverse only for square matrix")
                }
        }
}
