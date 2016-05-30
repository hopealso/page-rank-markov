load.graph <- function(graph.file) {
  # Loads graph used by other functions in demo. Assumes file is in working directory.
  #
  # Arguments:
  #   graph.file: Name of xlsx file containing matrix of a graph of interlinked web pages.
  
  file.data <- read.csv(graph.file) 
  data.matrix(file.data) # Convert data to matrix, transpose, and return
}


check.markov <- function(graph) {
  # Checks that the sum of every column = 1. 
  #   If it adds to 1, return the graph unchanged.
  #   If it adds to 0 (dangling node), return an adjusted graph, create each value 
  #     as 1/nx where nx is total number of nodes. 
  #   If it does not add to 1 and is not a dangling node, return FALSE. This represents
  #     an error in the matrix itself.
  #
  # Arguments:
  #   graph: Matrix of a graph of interlinked web pages
  
  # Loop through columns and normalize.
  nx <- nrow(graph)
  
  adjusted <- FALSE
  for (i in 1:nx) { 
    colsum <- sum(graph[,i])
    if (!isTRUE(all.equal(colsum,1, tolerance=0.0001))) { 
      if (colsum == 0) {
        graph[,i] <- 1/nx 
        adjusted <- TRUE
      } else {
        return(FALSE)
      }
    }
  }
  
  if (adjusted) {
    cat("Adjusted graph:")
    print(graph)
  }

  return(graph)
}


markov.demo <- function(graph, initial, random.factor=0.85, print.skip=3) {
  # Demonstrates iterations of Markov Chain using PageRank algorithm
  #
  # Arguments: 
  #   graph: Matrix of a graph of interlinked web pages, forming the transition matrix
  #     representing the probability of state change from j to i, i.e. the probability
  #     of a hypothetical web surfer following a link from the jth page to the ith page.
  #   initial: Initial probability vector.
  #   random.factor: Damping constant simulates random walk accounting for isolated pages.
  #     As written, this factor is the probability that a random surfer will *not*
  #     make a jump to a random page but will follow links.
  #     Set random.factor to 1 to simulate basic Markov Chain without damping.
  #   print.skip: Skip count when printing graphs to demonstrate iterations.
  
  nx <- nrow(graph) # number of nodes/pages
  probability <- initial
  
  # Minimum difference between iteration probability values
  delta_threshold <- 1e-7
  
  # Check if truly Markov, if not, change problem columns to sum to 1
  #graph <- check.markov(graph)
  
  i <- 1
  print(graph)
  
  # Iterate until PageRank probability vector is stable to threshold delta
  repeat{
    previous <- probability
    
    # PageRank formula
    probability <- (1 - random.factor) / nx + random.factor * (graph %*% probability)
    
    # Print alternate iterations.
    if (i %in% 1:5 | (i %% print.skip == 0)) {
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
  return(probability)
}


eigen.demo <- function(graph, random.factor=0.85) {
  
  nx <- length(graph)
  
  # Check if truly Markov, if not, change problem columns to sum to 1
  #graph <- check.markov(graph)
  
  # Create Random Walk Matrix (B) 
  B <- matrix(1/nx,nrow=nx,ncol=nx) 
  
  # Create PageRank Matrix based off Transition Matrix (graph) and Random Walk Matrix (B) 
  M <- (random.factor * graph) + ((1 - random.factor) * B) 
  
  # Create Eigen Vector from the first vector output and change typeof to double (by default, it is complex type) 
  eigen_vector <- as.double(eigen(M)$vectors[,1]) 
  
  # Normalize vector such that entire column sum = 1 
  steady_state_vector <- eigen_vector / sum(eigen_vector) 
  
  # Run check to see if Steady State Vector actually sums to 1 
  check <- sum(steady_state_vector) 
  
  if (check == c(1)) { 
    print(steady_state_vector) 
  } else { 
    print("WARNING: Steady State Vector DOES NOT sum to 1") 
    print(steady_state_vector)
  } 
}