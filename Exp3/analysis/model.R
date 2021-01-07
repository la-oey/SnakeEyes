# Simulate Liar + Detector #
setwd("/Users/loey/Desktop/Research/FakeNews/SnakeEyes/Exp1/analysis/")
source("lying_modelFunctions.R")

set.seed(100)
numSims = 100

sims <- data.frame(role=factor(),
                      decay=numeric(),
                      ks=numeric(), # ksay in Detector, k in Liar
                      n=numeric(),
                      val=numeric(),
                      se=numeric()) # prop in Detector, expLie in Liar

for(j in c(0.1, 0.25, 0.5, 1)){
  sim.D <- t(replicate(numSims, recurse.D(j, 0.5, rep(1/length(0:10),11))))
  prop <- colMeans(sim.D)
  se <- prop * (1-prop) / sqrt(numSims)
  sims <- bind_rows(sims, data.frame(role="Detector", decay=j, ks=0:10, n=numSims, val=prop, se=se))
  
  sim.L <- t(replicate(numSims, exp.ksay(0.5, recurse.D(j, 0.5, rep(1/length(0:10),11)))))
  expLie <- colMeans(sim.L)
  se <- apply(sim.L, 2, sd) / sqrt(numSims)
  sims <- bind_rows(sims, data.frame(role="Liar", decay=j, ks=0:10, n=numSims, val=expLie, se=se))
  
  sim.D2 <- t(replicate(numSims, recurse.L(j, 0.5, rep(1/length(0:10),11))))
  prop <- colMeans(sim.D2)
  se <- prop * (1-prop) / sqrt(numSims)
  sims <- bind_rows(sims, data.frame(role="Detector2", decay=j, ks=0:10, n=numSims, val=prop, se=se))
}

sims
