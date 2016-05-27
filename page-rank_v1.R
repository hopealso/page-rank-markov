###### INPUTS SET UP
n <- 3 #numbers of nodes/pages
a <- matrix(c(0,2,0)) #node A with B point to it. B has 2 outgoing links
b <- matrix(c(2,0,1)) #node B with A&C nodes pointing to it. A has 2 out links, C has 1
c <- matrix(c(2,2,0)) #node C with A&B pointing to it.
p <- matrix(c(1,0,0)) #probability of being on a page. Here I set the 1st interation to A as a homepage. Let's from now on refer to it as "0 Iteration or KNOT Iteration" because no iterations have actually been ran yet
colnames(p) <- "Probability" # Name column of P vector
rownames(p) <- c("Node A","Node B","Node C") # Name each row so can easily identify to which node
d <- .85 # google prob of going from a link...(1-d) = random walk probability




####### Code ran for 10 Iterations
# loops every new probability until it normalized and approached convergence.
  # Pr Node = (Random Walk / Total Nodes) + 
  # (Prob of going from a Link) * 
  # [ (Pr(Input Node_Alpha)/#Outputs Node_Alpha has) + 
  # (Pr(Input Node_Beta)/#Outputs Node_Beta has)  +
  # etc until all Input nodes are accounted for
for (i in 1:10) { 
pr.a <- (1-d)/n + d*(p[2]/2 )
pr.b <- (1-d)/n + d*(p[1]/2 + p[3]/1)
pr.c <- (1-d)/n + d*(p[1]/2 + p[2]/2)
# Once all new probabilities of iteration are complete, set p to be new probabilities.
p <- matrix(cbind(pr.a,pr.b,pr.c),n)
  # This IF loop will output out the very first iteration as a point of reference, then every 10th iteration. This way we can keep track of convergence
  if ( i == 1 | (i %% 10) == 0) {
    print(p)
  }
}



# Final Output of Page Ranks
p
