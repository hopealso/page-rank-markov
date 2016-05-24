n <- 3 #numbers of nodes/pages
a <- matrix(c(0,2,0)) #node A with B point to it. B has 2 outgoing links
b <- matrix(c(2,0,1)) #node B with A&C nodes pointing to it. A has 2 out links, C has 1
c <- matrix(c(2,2,0)) #node C with A&B pointing to it.
p <- matrix(c(1,0,0)) #probability of being on a page. Here I set the 1st interation to A as a homepage
d <- .85 # google prob of going from a link

for (i in 1:100){ #loops every new probability until it normalized.
pr.a <- (1-d)/n + d*(p[2]/2 )
pr.b <- (1-d)/n + d*(p[1]/2 + p[3]/1)
pr.c <- (1-d)/n + d*(p[1]/2 + p[2]/2)
p <- matrix(cbind(pr.a,pr.b,pr.c),n)
}

p #the results




