library(readxl) #package to read excel files

website <- read_excel("websites.xlsx") # get data from excel
nx <- length(website) #numbers of nodes/pages
website <- data.matrix(website) # convert data to matrix and transpose

gp <- .85 # google prob of going from a link

#probability of being on a page. Here I set the 1st interation to A as a homepage
pb <- scan(text = readline(prompt = "Give the initial probability of being on a page \n")) 

for (i in 1:50){ #loops every new probability until it normalized.
  #PageRank formula. Dot product of website matrix with probability vector.
  pb <-  (1-gp)/nx + gp*(website%*%pb)
  print(pb)
}

pb #the results






