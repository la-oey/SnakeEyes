setwd("/Users/loey/Desktop/Research/FakeNews/SnakeEyes/Exp3/analysis/")

source("lying_modelFunctions.R")
models.sources = paste0("models/",list.files("models/"))
sapply(models.sources, source)

library(tidyverse)
library(stats4)
humanLie <- sender
humanDetect <- receiver

#### General Functions ####
logitToProb <- function(logit){
  exp(logit) / (1+exp(logit))
}

probToLogit <- function(prob){
  log(prob / (1 - prob))
}




# 121 x 6 matrix
humanLieCounts <- humanLie %>%
  count(k, ksay) %>%
  complete(k=0:10, ksay=0:10, fill = list(n = 0)) %>%
  pull(n) %>%
  matrix(nrow=121)

# 22 x 6 matrix
humanDetectCounts <- humanDetect %>%
  count(ksay, callBS) %>%
  complete(ksay=0:10, callBS=c(TRUE,FALSE), fill = list(n = 0))
  
humanDetectCounts.T <- humanDetectCounts %>%
  filter(callBS) %>%
  pull(n) %>%
  matrix(nrow=11)
humanDetectCounts.F <- humanDetectCounts %>%
  filter(!callBS) %>%
  pull(n) %>%
  matrix(nrow=11)


getDiag <- function(arr){
  d = length(dim(arr))
  if(d == 3){
    apply(arr, MARGIN=d, FUN=diag)
  } else{
    diag(arr)
  }
}

select_all_but_diag <- function(x) {
  matrix(x[lower.tri(x, diag = F) | upper.tri(x, diag = F)], 
         nrow = nrow(x) - 1, 
         ncol = ncol(x))
}
getLies <- function(arr){
  d = length(dim(arr))
  if(d == 3){
    apply(arr, MARGIN=d, FUN=select_all_but_diag)
  } else{
    select_all_but_diag(arr)
  }
}

eval.s <- function(matr, ns){ #ns = 121 x 6 matrix of counts for all conditions
  sum(log(matr)*ns)
}

eval.r <- function(matr, ns.T, ns.F){ #ns = 11 x 6 matrix of counts for all conditions
  sum(log(matr)*ns.T + log(1-matr)*ns.F)
}


st = 1
end = 1
modelsEval = list(
  # # # # # # # # # # #
  # # recursive ToM # #
  # # # # # # # # # # #
  recurseToM = function(){
    print("recursive ToM")
    recurseToM.LL <- function(alph, eta.S, eta.R, lambda, weight){
      ns.l = array(humanLieCounts, dim=c(11,11,1))
      ns.T = humanDetectCounts.T
      ns.F = humanDetectCounts.F

      recurseToM.mat <- recurseToM.pred(alph, eta.S, eta.R, lambda, weight)
      r.eval = -eval.r(recurseToM.mat[[1]][,st:end], ns.T[,st:end], ns.F[,st:end])
      s.eval = -eval.s(recurseToM.mat[[2]][,,st:end], ns.l[,,st:end])
      print(paste("alph =", alph, "; weight =", logitToProb(weight), "; lambda =", lambda, "; r =", r.eval, "; s =", s.eval))
      neg.log.lik = r.eval + s.eval
      neg.log.lik + weight^2 #+ weight^4 + lambda^4
    }
    recurseToM.fit <- summary(mle(recurseToM.LL,
                                  start=list(alph=rnorm(1, 1, 0.2),
                                             eta.S=rnorm(1, 0, 1),
                                             eta.R=rnorm(1, 0, 1),
                                             lambda=rnorm(1, 0, 1),
                                             weight=rnorm(1, 0, 1)),
                                  method = "BFGS"))
    recurseToM.fit
  }
)










#   ///////////////////////////////
#  /////// EVALUATE MODELS ///////
# ///////////////////////////////






# recursive ToM
start_time <- Sys.time()
for(i in 1:50){
  tryCatch({
    recurseToMeval = modelsEval$recurseToM()
    break
  }, error = function(e){
    message(e)
  })
}
print(Sys.time() - start_time)
# save(recurseToMeval, file="Rdata/recurseToMfit.Rdata")

recurseToMeval.s <- -2*eval.s(
  recurseToM.pred(
    recurseToMeval@coef['alph','Estimate'],
    recurseToMeval@coef['eta.S','Estimate'],
    recurseToMeval@coef['eta.R','Estimate'],
    recurseToMeval@coef['lambda','Estimate'],
    recurseToMeval@coef['weight','Estimate'])[[2]][,,st:end],
  array(humanLieCounts, dim=c(11,11,6))[,,st:end]
)
recurseToMeval.r <- -2*eval.r(
  recurseToM.pred(
    recurseToMeval@coef['alph','Estimate'],
    recurseToMeval@coef['eta.S','Estimate'],
    recurseToMeval@coef['eta.R','Estimate'],
    recurseToMeval@coef['lambda','Estimate'],
    recurseToMeval@coef['weight','Estimate'])[[1]],
  humanDetectCounts.T, 
  humanDetectCounts.F
)



# # recursive ToM broken down by condition


# sts = c(1:6,1,4)
# ends = c(1:6,3,6)
# 
# for(i in 1:length(sts)){
#   st = sts[i]
#   end = ends[i]
#   start_time <- Sys.time()
#   for(i in 1:50){
#     tryCatch({
#       recurseToMeval.subset = modelsEval$recurseToM()
#       break
#     }, error = function(e){
#       message(e)
#     })
#   }
#   print(Sys.time() - start_time)
#   fileUtil = ifelse(st <= 3, "red", "blue")
#   fileP = case_when(
#     st == end & st %% 3 == 1 ~ "0.2",
#     st == end & st %% 3 == 2 ~ "0.5",
#     st == end & st %% 3 == 0 ~ "0.8",
#     TRUE ~ "NA"
#   )
#   filename = paste0("Rdata/recurseToMfit_", fileUtil, fileP, ".Rdata")
#   save(recurseToMeval.subset, file=filename)
# }









# everybody lies
everybodyLiesEval = modelsEval$everybodyLies()
everybodyLiesEval@m2logL


# some people lie
somePeopleLieEval = modelsEval$someLies()
somePeopleLieEval@m2logL


# always truth
alwaysTruthEval = modelsEval$alwaysTruth()
alwaysTruthEval@m2logL


# signif testing detect
signifTestingEval = modelsEval$signifTesting()
signifTestingEval@m2logL


# random sender
randomSenderEval = modelsEval$randomSender()
2*randomSenderEval


# random receiver
randomReceiverEval = modelsEval$randomReceiver()
2*randomReceiverEval







# Examine Truth vs Lies



liesTruthEval = list(
  noToM = function(){
    print("truth vs lies fit - no ToM")
    noToMeval.s.diag <- -2*eval.s(
      getDiag(
        array(
          noToM.s.pred(
            noToMeval@coef['alph','Estimate'], 
            noToMeval@coef['eta.S','Estimate'],
            noToMeval@coef['weight','Estimate']),
          dim=c(11,11,6))[,,st:end]
      ), 
      getDiag(
        array(
          humanLieCounts, 
          dim=c(11,11,6))[,,st:end]
      )
    )
    noToMeval.s.lies <- -2*eval.s(
      getLies(
        array(
          noToM.s.pred(
            noToMeval@coef['alph','Estimate'], 
            noToMeval@coef['eta.S','Estimate'],
            noToMeval@coef['weight','Estimate']),
          dim=c(11,11,6))[,,st:end]
      ), 
      getLies(
        array(
          humanLieCounts, 
          dim=c(11,11,6))[,,st:end]
      )
    )
    return(list(noToMeval.s.diag, noToMeval.s.lies))
  },
  
  recurseToM = function(){
    print("truth vs lies fit - recursive ToM")
    recurseToMeval.s.diag = -2*eval.s(
      getDiag(
        recurseToM.pred(
          recurseToMeval@coef['alph','Estimate'],
          recurseToMeval@coef['eta.S','Estimate'],
          recurseToMeval@coef['eta.R','Estimate'],
          recurseToMeval@coef['lambda','Estimate'],
          recurseToMeval@coef['weight','Estimate'])[[2]][,,st:end]
      ),
      getDiag(
        array(
          humanLieCounts, 
          dim=c(11,11,6))[,,st:end]
      )
    )
    recurseToMeval.s.lies = -2*eval.s(
      getLies(
        recurseToM.pred(
          recurseToMeval@coef['alph','Estimate'],
          recurseToMeval@coef['eta.S','Estimate'],
          recurseToMeval@coef['eta.R','Estimate'],
          recurseToMeval@coef['lambda','Estimate'],
          recurseToMeval@coef['weight','Estimate'])[[2]][,,st:end]
      ),
      getLies(
        array(
          humanLieCounts, 
          dim=c(11,11,6))[,,st:end]
      )
    )
    return(list(recurseToMeval.s.diag, recurseToMeval.s.lies))
  },
  
  everybodyLies = function(){
    print("truth vs lies fit - everybody lies")
    everybodyLiesEval.s.diag = -2*eval.s(
      getDiag(
        array(
          everybodyLies.pred(
            everybodyLiesEval@coef['lambda','Estimate'],
            everybodyLiesEval@coef['weight','Estimate']
          ),
          dim=c(11,11,6)
        )[,,st:end]
      ),
      getDiag(
        array(
          humanLieCounts, 
          dim=c(11,11,6))[,,st:end]
      )
    )
    everybodyLiesEval.s.lies = -2*eval.s(
      getLies(
        array(
          everybodyLies.pred(
            everybodyLiesEval@coef['lambda','Estimate'],
            everybodyLiesEval@coef['weight','Estimate']
          ),
          dim=c(11,11,6)
        )[,,st:end]
      ),
      getLies(
        array(
          humanLieCounts, 
          dim=c(11,11,6))[,,st:end]
      )
    )
    return(list(everybodyLiesEval.s.diag, everybodyLiesEval.s.lies))
  },
  
  somePeopleLie = function(){
    print("truth vs lies fit - some people lie")
    somePeopleLieEval.s.diag = -2*eval.s(
      getDiag(
        array(
          someLies.pred(
            somePeopleLieEval@coef['pTrue','Estimate'],
            somePeopleLieEval@coef['lambda','Estimate'],
            somePeopleLieEval@coef['weight','Estimate']
          ),
          dim=c(11,11,6)
        )[,,st:end]
      ),
      getDiag(
        array(
          humanLieCounts, 
          dim=c(11,11,6))[,,st:end]
      )
    )
    somePeopleLieEval.s.lies = -2*eval.s(
      getLies(
        array(
          someLies.pred(
            somePeopleLieEval@coef['pTrue','Estimate'],
            somePeopleLieEval@coef['lambda','Estimate'],
            somePeopleLieEval@coef['weight','Estimate']
          ),
          dim=c(11,11,6)
        )[,,st:end]
      ),
      getLies(
        array(
          humanLieCounts, 
          dim=c(11,11,6))[,,st:end]
      )
    )
    return(list(somePeopleLieEval.s.diag, somePeopleLieEval.s.lies))
  }
)

liesTruthEval$noToM()
liesTruthEval$recurseToM()
liesTruthEval$everybodyLies()
liesTruthEval$somePeopleLie()



recurseToM.pred(
  recurseToMeval@coef['alph','Estimate'],
  recurseToMeval@coef['eta.S','Estimate'],
  recurseToMeval@coef['eta.R','Estimate'],
  recurseToMeval@coef['lambda','Estimate'],
  recurseToMeval@coef['weight','Estimate'])[[2]][,,1] %>%
  as_tibble() %>% 
  mutate(ksay = 0:10) %>% 
  pivot_longer(-ksay, names_to = 'k', values_to='probability') %>%
  mutate(k = as.numeric(substr(k, 2, 10))-1,
         probTxt = paste0(round(probability*100),"%")) %>%
  ggplot(aes(x=k, y=ksay, fill=probability, label=probTxt)) +
  geom_tile() +
  geom_text(size=3) +
  ggtitle("Recursive ToM (Optimized)") +
  guides(fill = FALSE) +
  scale_x_continuous("", expand=c(0,0)) +
  scale_y_continuous("", expand=c(0,0)) +
  scale_fill_gradient2(low="white", mid="darkorchid", high="blue", midpoint=0.5, limits=c(0,1))
ggsave("img/recursiveToM_prediction.png")
