## These two functions provide functionality for the cached computation of the inverse matrix
## how to use:
## You must create special object using special_matrix<-makeCacheMatrix(original_matrix)
## You can get inverse matrix using cacheSolve(special_matrix)
##
## Эти две функции предоставляют функционал для кэшированного вычисления обратной матрицы
## Как использовать:
## Сначала создаете специальный объект используя что-нибудь аля special_matrix<-makeCacheMatrix(original_matrix)
## Потом получаете обратную матрицу используя cacheSolve(special_matrix)


## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL # variable containing the inverse matrix
  set <- function(y) { # function used to set the matrix
    x <<- y 
    inverse <<- NULL # we dont know the inverse of new matrix
  }
  get <- function() x # function used to get the matrix
  set_inv <- function(inverse_matrix) inverse <<- inverse_matrix #function to set the 'inverse' variable
  get_inv <- function() inverse #function to retrive the 'inverse' variable
  list(set = set, get = get,
       set_inverse = set_inv,
       get_inverse = get_inv)
}


## This function computes the inverse of the special
## "matrix"(actually list) returned by "makeCacheMatrix" function. If the inverse has
## already been calculated (and the matrix has not changed), then
## this function retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inverse<-x$get_inverse() # get the stored inverse
  if (!is.null(inverse)) {inverse} #if the stored matrix is not null (it means that we have calculated the inverse matrix) then we return calculated matrix
  else {
    data<-x$get() # get new data
    inverse<-solve(data) # calculate inverse matrix
    x$set_inverse(inverse) # store the calculated value
    inverse  
    }
}
