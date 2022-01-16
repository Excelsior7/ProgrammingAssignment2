## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(matrix = matrix()) {
  
  matrix_inverse <- NULL;
  
  getMatrix <- function() { matrix }
  setMatrix <- function(new_matrix) { matrix <<- new_matrix }
  
  getInverseMatrix <- function() { matrix_inverse }
  setInverseMatrix <- function(matrix_inverse_new) { matrix_inverse <<- matrix_inverse_new }
  
  list(getMatrix = getMatrix, setMatrix = setMatrix, 
       getInverseMatrix = getInverseMatrix, setInverseMatrix = setInverseMatrix);
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
