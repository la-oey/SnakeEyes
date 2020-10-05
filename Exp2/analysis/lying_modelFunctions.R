#Uniform Distribution Task

dieSides = 10
KSAY = matrix(rep(0:dieSides,dieSides+1),nrow=dieSides+1)
K = matrix(rep(0:dieSides, each=dieSides+1), nrow=dieSides+1)
BET = 0.8
ALPH = 0.25 #0.25
liePenalty = 10 # 10 # -5
faPenalty = 5 # 5 # 0
moral = 0 #liar's internal penalty for lying

# Depends on Expt #
u.L <- function(ksay, lie, BS) {
  if(!BS){
    util = 2*ksay - 10 # no BS + no lie, no BS + lie
  } else{
    if(!lie){
      util = 2*ksay - (10-faPenalty) # BS + no lie
    } else{
      util = rep(-liePenalty, length(ksay)) # BS + lie
    }
  }
  if(lie){
    util = util - moral
  }
  return(util)
}


u.D <- function(ksay, lie, BS) {
  if(!BS){
    util = 10 - 2*ksay # no BS + no lie, no BS + lie
  } else{
    if(!lie){
      util = (10-faPenalty) - 2*ksay # BS + no lie
    } else{
      util = rep(liePenalty, length(ksay)) # BS + lie
    }
  }
  return(util)
}

u.L(0:10, TRUE, FALSE)
u.L(0:10, TRUE, TRUE)
u.L(0:10, FALSE, FALSE)
u.L(0:10, FALSE, TRUE)
u.D(0:10, TRUE, FALSE)
u.D(0:10, TRUE, TRUE)
u.D(0:10, FALSE, FALSE)
u.D(0:10, FALSE, TRUE)

softmax <- function(allEV) { # allEV = vector of numerics
  mapply(function(i) exp(i*ALPH)/sum(exp(allEV*ALPH)), allEV)
}

p.k <- function(k) {
  1/length(0:dieSides)
}




EV.D_bs.ksay.r <- function(ksay, p, bs, p.L) { # both are vectors
  u.D(ksay, lie=TRUE, BS=bs) * p.L + u.D(ksay, lie=FALSE, BS=bs) * (1-p.L)
}
# EV.D_bs.ksay.r(0:10, 0.5, TRUE, rep(p.k(0:10),11))
# EV.D_bs.ksay.r(0:10, 0.5, TRUE, mapply(p_t.ksay.r, 0:10, 0.5, rep(p.k(0:10),11)))


p.D_bs.ksay.r <- function(ksay, p, p.L) {
  EV.BS <- EV.D_bs.ksay.r(ksay, p, TRUE, p.L)
  EV.noBS <- EV.D_bs.ksay.r(ksay, p, FALSE, p.L)
  softmax(c(EV.BS, EV.noBS))[1]
}
# mapply(p.D_bs.ksay.r, 0:10, 0.5, 0.4)
# mapply(p.D_bs.ksay.r, 0:10, 0.5, mapply(p_t.ksay.r, 0.5, rep(0.4,11)))


EV.L_ksay.k.r <- function(k, ksay, p, p.D) {
  mapply(u.L, ksay, lie=ksay!=k, BS=TRUE) * p.D + mapply(u.L, ksay, lie=ksay!=k, BS=FALSE) * (1-p.D)
}
# EV.L_ksay.k.r(5, 0:10, 0.5, rep(0.5,11))
# mapply(function(i) EV.L_ksay.k.r(i, 0:10, 0.5, rep(0.5,11)), 0:10)


p.L_ksay.k.r <- function(p, p.D) { #look into this
  EV.all <- mapply(function(i) EV.L_ksay.k.r(i, 0:dieSides, p, p.D), 0:dieSides)
  apply(EV.all,2,softmax)
}
# round(p.L_ksay.k.r(0.5, rep(0.5,11)),4) 
# round(p.L_ksay.k.r(0.5, rep(0.5,11)) * .8 + 1 / length(0:numMarbles) *.2,4)


p_t.ksay.r <- function(p, p.D) {
  P.K <- matrix(p.k(0:dieSides), nrow=dieSides+1, ncol=dieSides+1)
  P.L_KSAY.K <- p.L_ksay.k.r(p, p.D)
  LIE = 1-diag(dieSides+1)
  rowSums(P.K*P.L_KSAY.K*LIE)/rowSums(P.K*P.L_KSAY.K)
}
# p_t.ksay.r(0.5, 1 / length(0:dieSides))

exp.ksay <- function(p, p.D) {
  colSums(KSAY * p.L_ksay.k.r(p, p.D))
}
#exp.ksay(0.5,rep(0.5,11))

recurse.L <- function(decay, p, prior=rep(1/length(0:dieSides),11)) {
  p_t.ksay.r(p, recurse.D(decay, p, prior))
}

recurse.D <- function(decay, p, prior=rep(1/length(0:dieSides),11)){
  if(runif(1, 0, 1) < decay){
    return(prior)
  } else{
    return(mapply(p.D_bs.ksay.r,0:10, p, recurse.L(decay, p, prior)))
  }
}

1-p.k(0:10)
EV.D_bs.ksay.r(3, 0.8, TRUE, p_t.ksay.r(0.8, rep(0.5,11)))


recurse.D(0.1, 0.5)
recurse.L(0.1, 0.5)
(zeroth <- rep(0.5,11))
#(zeroth <- pbinom(0:numMarbles, numMarbles, 0.5))
(first <- p_t.ksay.r(0.5, zeroth))
(second <- mapply(p.D_bs.ksay.r, 0:dieSides, 0.5, first))
(third <- p_t.ksay.r(0.5, second))
(fourth <- mapply(p.D_bs.ksay.r, 0:dieSides, 0.5, third))
(fifth <- p_t.ksay.r(0.5, fourth))











p.lie.k.fromDet <- function(p, p.D) { #look into this
  EV.all <- mapply(function(i) EV.L_ksay.k.r(i, 0:dieSides, p, p.D), 0:dieSides)
  1-diag(apply(EV.all,2,softmax))
}

round(p.lie.k.fromDet(0.5, rep(0.5,11)),4) 

