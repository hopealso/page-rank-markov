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





###################################################### 
###################################################### 
###################################################### 
###################################################### 
###################################################### 
# Proposed eigen value code 
install.packages("pracma") 
library(pracma) 

## Check if each column sums to 1, if not then over write it so that each value in the column = 1/nx 
for (i in 1:nx)  
{ #loops every new probability until it normalized. 
  if (sum(website[,i]) != 1) { 
    website[,i] <- 1/nx 
  } 
} 


# Create Random Walk Matrix (B) 
B <- matrix(1/nx,nrow=nx,ncol=nx) 


# Create PageRank Matrix based off Transition Matrix (website) and Random Walk Matrix (B) 
M <- (gp * website) + ( (1 - gp) * B) 


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
install.packages("pracma") 
library(pracma) 


# Since this is a positive column Stochastic Model (the sum of every column = 1), then 1 is ALWAYS an eigen value and ALWAYS the largest eigen value. This should theoretically correspond with the first eigenvector (We may need to check this in other calculations) 


# Test with THIS matrix first (exact same from Cornell) 
(website <- matrix(c(0, 1/3, 1/3, 1/3, 0, 0, 1/2, 1/2, 1, 0 ,0 ,0 ,1/2 ,0 ,1/2 , 0), ncol=4)) 


(eigen_vector <- eigen(website)$vectors[,1]) 


# Normalize vector such that entire column sum = 1 
(steady_state_vector <- eigen_vector / sum(eigen_vector)) 






# Try this with Haroon's Example 1 WITH DANGLING SITE. 
# THIS EQUATION INCLUDES the Random Walk into the matrix. 
# HOWEVER, it won't fix any dangling nodes (columns that don't add up to 1). We need to include that somewhere in the code as a check.  
# website will be our Transition Matrix now. We will combine it with the Random Walk Matrix and Random Walk probability (gp) to create our PageRank Matrix (M) 
#  
(website <- matrix(c(0, 0, 0, 0, 1/2, 0, 1/2, 0, 1, 0 ,0 ,0 ,1/3 ,1/3 ,1/3 , 0), ncol=4)) 


# Random Walk Matrix (same dimensions as website, but divided evenly in each column since equal probability of jumping from one site to the other) 
(B <- matrix(1/nx,nrow=nx,ncol=nx)) 


# Create new matrix M (PageRank Matrix) that keeps 85% of weight to original matrix and 15% to the random walk. Random walk is also known as a "damping factor" 
(M <- (gp * website) + ( (1 - gp) * B)) 


# This gives us the wrong Eigenvalue...since column 1 does not add up to 1.  
eigen(M) 


## RECOMMENDATION:  
# Maybe we can run a check to see if each column sums to 1? and if not, rewrite the column to be 1/nx before proceeding. This is an idea below (not actual code that will work) 
if (sum(website[,i]) != 1) { 
  website[,i] <- 1/nx 
} 


