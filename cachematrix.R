## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(matrix = matrix()) {
  
  matrix_inverse <- NULL;
  
  getMatrix <- function() { matrix; }
  setMatrix <- function(new_matrix) { 
    matrix <<- new_matrix; 
    matrix_inverse <<- NULL;
  }
  
  getInverseMatrix <- function() { matrix_inverse; }
  setInverseMatrix <- function(matrix_inverse_new) { matrix_inverse <<- matrix_inverse_new; }
  
  list(getMatrix = getMatrix, setMatrix = setMatrix, 
       getInverseMatrix = getInverseMatrix, setInverseMatrix = setInverseMatrix);
}


# Description : cacheSolve is the interface to the closure environment passed as argument 
# and has the function to compute and cache the inverse of the matrix cached in the closure environment
# if it is not already cached in it.

# Return value : The inverse of the matrix cached in the closure environment.

cacheSolve <- function(cachematrix_closures, ...) {
  
  matrix_inverse <- cachematrix_closures$getInverseMatrix();
  
  if(is.null(matrix_inverse) == TRUE) {
    
    matrix_inverse_new <- solve(cachematrix_closures$getMatrix(), ...);
    cachematrix_closures$setInverseMatrix(matrix_inverse_new);
    
    return(matrix_inverse_new);
  }
  
  message("The inverse of the matrix has already been calculated and cached.");
  matrix_inverse;
  
}