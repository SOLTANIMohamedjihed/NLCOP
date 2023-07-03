# Source NlcOptim help
library(NlcOptim)

nh <- vector("numeric", length = 5)

Nh <- c(6221,11738,4333,22809,5467)
ch <- c(120, 80, 80, 90, 150)

mh.rev <- c(85, 11, 23, 17, 126)
Sh.rev <- c(170.0, 8.8, 23.0, 25.5, 315.0)

mh.emp <- c(511, 21, 70, 32, 157)
Sh.emp <- c(255.50, 5.25, 35.00, 32.00, 471.00)

ph.rsch <- c(0.8, 0.2, 0.5, 0.3, 0.9)

ph.offsh <- c(0.06, 0.03, 0.03, 0.21, 0.77)

budget <- 300000
n.min <- 100
relvar.rev <- function(nh){
 rv <- sum(Nh * (Nh/nh - 1)*Sh.rev^2)
 tot <- sum(Nh * mh.rev)
 rv/tot^2
}

relvar.emp <- function(nh){
 rv <- sum(Nh * (Nh/nh - 1)*Sh.emp^2)
 tot <- sum(Nh * mh.emp)
 rv/tot^2
}

relvar.rsch <- function(nh){
 rv <- sum( Nh * (Nh/nh - 1)*ph.rsch*(1-ph.rsch)*Nh/(Nh-1) )
 tot <- sum(Nh * ph.rsch)
 rv/tot^2
}

relvar.offsh <- function(nh){
 rv <- sum( Nh * (Nh/nh - 1)*ph.offsh*(1-ph.offsh)*Nh/(Nh-1) )
 tot <- sum(Nh * ph.offsh)
 rv/tot^2
}

nlc.constraints <- function(nh){
 h <- rep(NA, 13)
 h[1:length(nh)] <- (Nh + 0.01) - nh
 h[(length(nh)+1) : (2*length(nh)) ] <- (nh + 0.01) - n.min
 h[2*length(nh) + 1] <- 0.05^2 - relvar.emp(nh)
 h[2*length(nh) + 2] <- 0.03^2 - relvar.rsch(nh)
 h[2*length(nh) + 3] <- 0.03^2 - relvar.offsh(nh)
 return(list(ceq=NULL, c=-h))
}

nlc <- function(nh){
 h <- rep(NA, 3)
 h[ 1] <- 0.05^2 - relvar.emp(nh)
 h[ 2] <- 0.03^2 - relvar.rsch(nh)
 h[3] <- 0.03^2 - relvar.offsh(nh)
 return(list(ceq=NULL, c=-h))
}

Aeq <- matrix(ch/budget, nrow=1)
Beq <- 1

A<-rbind(diag(-1,5,5),diag(1,5,5))
B<-c(-Nh-0.01,rep(n.min-0.01,5))

solnl(X=rep(100,5),objfun=relvar.rev,confun=nlc.constraints, Aeq=Aeq, Beq=Beq)

solnl(X=rep(100,5),objfun=relvar.rev,confun=nlc, Aeq=Aeq, Beq=Beq, A=-A, B=-B)