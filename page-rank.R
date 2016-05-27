
load.graph <- function(graph.file) {
  # Loads graph for use in other functions in this demo. Assumes named file is in working directory.
  #
  # Arguments:
  #   graph.file: Name of xlsx file containing matrix of a graph of interlinked web pages,
  #               forming the basis of the transition matrix representing the probability
  #               of state change from j to i, i.e. probability of a hypothetical web surfer 
  #               following a link from the jth page to the ith page.
  
  file.data <- read_excel(graph.file) # Get data from excel.
  nx <- length(file.data) # number of nodes/pages
  data.matrix(file.data) # Convert data to matrix, transpose, and return
}

markov.demo <- function(graph, random.factor) {
  # Demonstrates iterations of Markov Chain using PageRank algorithm
  #
  # Arguments: 
  #   graph: Matrix of a graph of interlinked web pages, forming the basis of the transition
  #          matrix representing the probability of state change from j to i, i.e. the probability
  #          of a hypothetical web surfer following a link from the jth page to the ith page.
  #   random.factor: Damping constant to simulate random walk accounting for issue of isolated pages.
  #                  As written, this factor is the probability that a random surfer will *not*
  #                  make a jump to a random page but will follow links.
  
  # initial probability vector
  initial <- rep(1 / nx, nx)
  # initial <- c(1, rep(0, nx - 1))
  probability <- initial
  
  # Minimum difference between iteration probability values
  delta_threshold <- 1e-10
  i <- 1
  
  # Loop through iterations until resultant PageRank probability vector is stable to threshold delta
  repeat{
    previous <- probability
    
    # PageRank formula
    probability <- (1 - random.factor) / nx + random.factor * (graph %*% probability)
    
    # Print alternate iterations.
    if (i == 1 | (i %% 3 == 0)) {
      cat("Iteration", i, ": ")
      print(probability)
    }
    
    # Check difference between probability and previous probability iteration.
    check_vector <- abs(previous - probability)
    
    # If all values in check_vector are less than delta_threshold, print result and end.
    if (all(check_vector < delta_threshold)) {
      cat("Probabilities converge to steady state vector at iteration number", i, ": ")
      print(probability)
      break      
    } else if (i == 1000) {
      cat("Did not reach steady state within 1000 iterations")
      print(probability)
      break
    }
    
    i <- i + 1
  }
}

library(readxl) # package to read excel files
graph <- load.graph("graph-simple.xlsx")
random.factor <- 0.85
markov.demo(graph, random.factor)


# Gian's Work in Progress on Eigenvector calculation:
###################################################### 
###################################################### 
###################################################### 
###################################################### 
###################################################### 
# Proposed eigen value code 
# install.packages("pracma") 
library(pracma) 

## Check if each column sums to 1, if not then over write it so that each value in the column = 1/nx 
for (i in 1:nx)  
{ #loops every new probability until it normalized. 
  if (sum(graph[,i]) != 1) { 
    graph[,i] <- 1/nx 
  } 
} 


# Create Random Walk Matrix (B) 
B <- matrix(1/nx,nrow=nx,ncol=nx) 


# Create PageRank Matrix based off Transition Matrix (graph) and Random Walk Matrix (B) 
M <- (random.factor * graph) + ( (1 - random.factor) * B) 


# Create Eigen Vector from the first vector output and change typeof to double (by default, it is complex type) 
eigen_vector <- as.double(eigen(M)$vectors[,1]) 


# Normalize vector such that entire column sum = 1 
steady_state_vector <- eigen_vector / sum(eigen_vector) 


print(steady_state_vector) 




# Run check to see if Steady State Vector actually sums to 1 
check <- sum(steady_state_vector) 


if (check == c(1)) { 
  print("Steady State Vector sums to 1") 
} else  
{ 
  print("Steady State Vector DOES NOT sum to 1") 
} 






######## Scratch work / notes 

######################### EIGEN VECTOR CALCULATION 
### NOTE: THIS WILL NOT WORK UNLESS ENTIRE COLUMNS ADD UP TO 1 SO EVERY NODE MUST HAVE AT LEAST 1 LINK 
# install.packages("pracma") 
library(pracma) 


# Since this is a positive column Stochastic Model (the sum of every column = 1), then 1 is ALWAYS an eigen value and ALWAYS the largest eigen value. This should theoretically correspond with the first eigenvector (We may need to check this in other calculations) 


# Test with THIS matrix first (exact same from Cornell) 
(graph <- matrix(c(0, 1/3, 1/3, 1/3, 0, 0, 1/2, 1/2, 1, 0 ,0 ,0 ,1/2 ,0 ,1/2 , 0), ncol=4)) 


(eigen_vector <- eigen(graph)$vectors[,1]) 


# Normalize vector such that entire column sum = 1 
(steady_state_vector <- eigen_vector / sum(eigen_vector)) 






# Try this with Haroon's Example 1 WITH DANGLING SITE. 
# THIS EQUATION INCLUDES the Random Walk into the matrix. 
# HOWEVER, it won't fix any dangling nodes (columns that don't add up to 1). We need to include that somewhere in the code as a check.  
# graph will be our Transition Matrix now. We will combine it with the Random Walk Matrix and Random Walk probability (random.factor) to create our PageRank Matrix (M) 
#  
(graph <- matrix(c(0, 0, 0, 0, 1/2, 0, 1/2, 0, 1, 0 ,0 ,0 ,1/3 ,1/3 ,1/3 , 0), ncol=4)) 


# Random Walk Matrix (same dimensions as graph, but divided evenly in each column since equal probability of jumping from one site to the other) 
(B <- matrix(1/nx,nrow=nx,ncol=nx)) 


# Create new matrix M (PageRank Matrix) that keeps 85% of weight to original matrix and 15% to the random walk. Random walk is also known as a "damping factor" 
(M <- (random.factor * graph) + ( (1 - random.factor) * B)) 


# This gives us the wrong Eigenvalue...since column 1 does not add up to 1.  
eigen(M) 


## RECOMMENDATION:  
# Maybe we can run a check to see if each column sums to 1? and if not, rewrite the column to be 1/nx before proceeding. This is an idea below (not actual code that will work) 
if (sum(graph[,i]) != 1) { 
  graph[,i] <- 1/nx 
} 



