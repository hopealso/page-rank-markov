library(readxl) #package to read excel files

website <- read_excel("websites.xlsx") # get data from excel
nx <- length(website) #numbers of nodes/pages
website <- data.matrix(website) # convert data to matrix and transpose

gp <- .85 # google prob of going from a link

#probability of being on a page. Here I set the 1st interation to A as a homepage

pb <- scan(text = readline(prompt = "Give the initial probability of being on a page \n"))

# Minimum difference between iteration probability values
delta_threshold <- 1e-10





for (i in 1:50) 
{ #loops every new probability until it normalized.
  
  
  #PageRank formula. Dot product of website matrix with probability vector.
  pb_temp <- pb
  pb <-  (1-gp)/nx + gp*(website%*%pb)
  
  
  # Created checks and differences between pb and previous pb iteration (aka check_vector)
  check_vector <- abs(pb_temp - pb)
  check_bool <- FALSE
  break_key <- FALSE
  
  
  # Run this over every element in check_vector
  for (n in 1:nx) 
  {
    
    
    # If every value in check_vector are less than 1e-10, change check_bool to TRUE. Otherwise change it back to FALSE.
    if ( check_vector[n] < delta_threshold) 
    {
      check_bool <- TRUE
    }
    
    else 
    {
      check_bool <- FALSE
    }
    
    # If check_bool stays TRUE, then print pb table, iteration number and change break_key to TRUE...then break out of FOR (n in 1:nx) loop
    if (check_bool == TRUE) 
    {
      print("Probabilities converge to steady state vector")
      print(pb)
      print("At iteration number")
      print(i)
      break_key <- TRUE
      break
    }
    
  }
  
  # If break_key is TRUE, then break out of FOR (i in 1:50) loop too. Without this, code will keep running and printing every iteration afterwards
  if (break_key == TRUE) 
  {
    break
  }
  
}


pb #the results
