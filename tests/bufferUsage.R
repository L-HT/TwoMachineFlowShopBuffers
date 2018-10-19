B = 5
N = 10

jobData <- generateProblemInstance(N, c(1,2), c(5,5), c(1,B))
perm <- paste("j",getNEHSolution(jobData, "intermediateBuffer", 30, 0)[1:3],sep="")

perm2 <- perm#c("j10","j8","j4")

print(perm)
simRes <- simulateFlowShop(jobData, perm, perm2, B)

plotSchedule(simRes, jobData, B, 1)
getCurrentBufferUsageR(jobData,perm,perm2,B)
simulateBufferUsage(jobData,perm,perm2,B)
#simulateSpecial(jobData,perm,50,T,getMakespan = F)
simulateSpecial(jobData,perm,B,T,getMakespan = F)
#getCurrentBufferUsageR(jobData,perm, perm,5)
#getCurrentBufferUsage(jobData,perm, perm,5)

