load.graph <- function(graph.file) {
  # Loads graph used by other functions in demo. Assumes file is in working directory.
  #
  # Arguments:
  #   graph.file: Name of xlsx file containing matrix of a graph of interlinked web pages.
  
  file.data <- read.csv(graph.file) 
  data.matrix(file.data) # Convert data to matrix, transpose, and return
}


check.markov <- function(graph, fix.dangling) {
  # Checks that the sum of every column = 1. 
  #   If it adds to 1, return the graph unchanged.
  #   If it adds to 0 (dangling node), return an adjusted graph, create each value 
  #     as 1/nx where nx is total number of nodes. 
  #   If it does not add to 1 and is not a dangling node, return FALSE. This represents
  #     an error in the matrix itself.
  #
  # Arguments:
  #   graph: Matrix of a graph of interlinked web pages
  
  nx <- nrow(graph)
  
  # Loop through columns and normalize.    
  adjusted <- FALSE
  for (i in 1:nx) { 
    colsum <- sum(graph[,i])
    if (!isTRUE(all.equal(colsum,1, tolerance=0.0001))) { 
      if (colsum == 0) {
        if (fix.dangling) {
          graph[,i] <- 1/nx 
          adjusted <- TRUE
        }
      } else {
        return(FALSE)
      }
    }
  }
  
  if (adjusted) {
    message("Graph adjusted to correct dangling nodes:")
    print(graph)
  }
  
  return(graph)
}


markov.demo <- function(graph, initial, damping.factor=0.85, 
                        print.skip=3, fix.dangling=TRUE) {
  # Demonstrates iterations of Markov Chain using PageRank algorithm
  #
  # Arguments: 
  #   graph: Matrix of a graph of interlinked web pages, forming the transition matrix
  #     representing the probability of state change from j to i, i.e. the probability
  #     of a hypothetical web surfer following a link from the jth page to the ith page.
  #   initial: Initial probability vector.
  #   damping.factor: Damping constant simulates random walk accounting for isolated pages.
  #     As written, this factor is the probability that a random surfer will *not*
  #     make a jump to a random page but will follow links.
  #     Set damping.factor to 1 to simulate basic Markov Chain without damping.
  #   print.skip: Skip count when printing graphs to demonstrate iterations.
  
  nx <- nrow(graph) # number of nodes/pages
  probability <- initial
  
  message("Graph input, representing original transition matrix:")
  print(graph)
  
  # Check if truly Markov, if not, change problem columns to sum to 1
  graph <- check.markov(graph, fix.dangling)
  if (graph[1] == FALSE & length(graph) == 1) {
    stop("ERROR: Data is not properly formatted.")
  }
  
  # Minimum difference between iteration probability values
  delta_threshold <- 1e-7
  
  # Iterate until PageRank probability vector is stable to threshold delta, 
  # or max 1000 iterations
  for (i in 1:1000) {
    previous <- probability
    
    # PageRank formula
    probability <- (1 - damping.factor) / nx + damping.factor * (graph %*% probability)
    
    # Print alternate iterations.
    if (i %in% 1:3 | (i %% print.skip == 0)) {
      message("Iteration ", i, " PageRank (probability) vector: ")
      print(probability)
    }
    
    # Check difference between probability and previous probability iteration.
    check_vector <- abs(previous - probability)
    
    # If all values in check_vector are less than delta_threshold, print result and end.
    if (all(check_vector < delta_threshold)) {
      message("Probabilities converge to steady state vector at iteration number ", i, ": ")
      return(probability)
      break      
    } 
  }
  
  message("Did not reach steady state within 1000 iterations.")
  print(probability)
}


eigen.demo <- function(graph, damping.factor=0.85, fix.dangling=TRUE) {
  
  nx <- nrow(graph)
  
  # Check if truly Markov, if not, change problem columns to sum to 1
  graph <- check.markov(graph, fix.dangling)
  if (graph[1] == FALSE & length(graph) == 1) {
    stop("ERROR: Data is not properly formatted.")
  }
  
  # Create Random Walk Matrix (B) 
  B <- matrix(1/nx,nrow=nx,ncol=nx) 
  
  # Create PageRank Matrix based off Transition Matrix (graph) and Random Walk Matrix (B) 
  M <- (damping.factor * graph) + ((1 - damping.factor) * B) 
  
  eigen_output <- eigen(M)
  
  # Create eigenvector with an eigenvalue of 1 and change type to double (by default, 
  # it is complex type) 
  where.eigenvalue.1 <- abs(as.double(eigen_output$values) - 1) < 1e-6
  if (!any(where.eigenvalue.1)) {
    stop("There is no eigenvalue of 1.")
  }
  eigen_vector <- as.double(eigen_output$vectors[, where.eigenvalue.1])

  # Normalize vector such that entire column sum = 1 
  steady_state_vector <- eigen_vector / sum(eigen_vector) 
  
  # Run check to see if Steady State Vector actually sums to 1 
  check <- sum(steady_state_vector) 
  
  if (isTRUE(all.equal(check,1))) { 
    message("Steady state vector is:")
    return(steady_state_vector) 
  } else { 
    warning("Normalized eigenvector DOES NOT sum to 1.") 
    print(steady_state_vector)
  } 
}