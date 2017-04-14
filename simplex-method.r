# Author: Luis, Brent Ian
# Lab Section: CMSC 150 B-5L
# December 1, 2016

# implements Simplex method
Simplex <- function(matrix, numberOfRows, numberOfColumns){
  iterations = 0
  hasNegative = TRUE

  while(hasNegative){
    # selects the pivot column
    pivotColumn = 0 
    for(i in seq(1, numberOfColumns-1)) {
      if(matrix[numberOfRows, i] < 0){  # checks negative elements in the last row
        if(pivotColumn == 0){ 
          pivotColumn = i
        } else if(matrix[numberOfRows, i] < matrix[numberOfRows, pivotColumn]){
          pivotColumn = i
        }# else if
      }# if
    }# for

    # selects the pivot row
    pivotRow = 0
    for(i in seq(1, numberOfRows-1)) {
      if(matrix[i, pivotColumn] > 0) {
        if(pivotRow == 0){
          pivotRow = i
        } else if(matrix[i, numberOfColumns] / matrix[i, pivotColumn] >= 0 && matrix[i, numberOfColumns] / matrix[i, pivotColumn] < matrix[pivotRow, numberOfColumns] / matrix[pivotRow, pivotColumn]){ # computes test ratio: solution column divided by the pivot column
          pivotRow = i
        }# else if
      }# if
    }# if
    
    # normalizes pivot row
    pivotElement = matrix[pivotRow, pivotColumn]
    matrix[pivotRow, ] = matrix[pivotRow, ] / pivotElement
    
    # implements Gauss-Jordan Elimination
    for(i in seq(1, numberOfRows)) {
      # retains only pivot row
      if(i != pivotRow){
        matrix[i, ] = matrix[i, ] - (matrix[pivotRow, ] * matrix[i, pivotColumn])
      }# if
    }# for
    
    # checks last row if it has negative
    hasNegative = FALSE
    for(i in matrix[numberOfRows, ]) {
      if(i < 0){
        hasNegative = TRUE
      } # if
    }# for

    # prints each iteration
    iterations = iterations + 1
    cat("Iteration: ", iterations, "\n")
    print(matrix)
  }# while
}# Simplex

# initializes the main function
Main <- function () {
  # reads input file as a matrix
  input <- read.csv(file="C:\\Users\\Brent Ian Luis\\Desktop\\luis_project\\test cases\\test case 1.csv", header=TRUE, sep=",")
  matrix <- as.matrix(input)

  # gets matrix dimensions
  numberOfRows = nrow(matrix)
  numberOfColumns = ncol(matrix)
  
  # prints initial tableau
  cat("Initial Tableau:\n")
  print((matrix))

  Simplex(matrix, numberOfRows, numberOfColumns)
}# Main