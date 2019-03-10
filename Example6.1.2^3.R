
factor = expand.grid(A = c(-1,1), B = c(-1,1),C = c(-1,1))

ed <- matrix(c(550,604,669,650,633,601,642,635,1037,1052,749,868,1075,1063,729,860),byrow=T,ncol=2)

dimnames(ed) <- list(c("(1)","a","b","ab","c","ac","bc","abc"),c("Rep1","Rep2"))
Total <- apply(ed,1,sum)
cbind(ed,factor,Total)


A=factor$A
B=factor$B
C=factor$C


r = 2
Aeff <- (Total %*% A)/(4*r)
Beff <- (Total %*% B)/(4*r)
Ceff <- (Total %*% C)/(4*r) 
# Interaction effects
AB <- A*B
AC <- A*C
BC <- B*C
ABC <- A*B*C
cbind(A,B,C,AB,AC,BC,ABC,Total)

ABeff <- (Total %*% AB)/(4*r)
ACeff <- (Total %*% AC)/(4*r)
BCeff <- (Total %*% BC)/(4*r)
ABCeff <- (Total %*% ABC)/(4*r)

Effects <- t(Total) %*% cbind(A,B,C,AB,AC,BC,ABC)/(4*r)
Summary <- rbind( cbind(A,B,C,AB,AC,BC,ABC),Effects )
dimnames(Summary)[[1]] <- c(dimnames(ed)[[1]],"Effect")
Summary

ed.vec <- c(t(ed))
Af <- rep(as.factor(A),rep(2,8))
Bf <- rep(as.factor(B),rep(2,8))
Cf <- rep(as.factor(C),rep(2,8))
options(contrasts=c("contr.sum","contr.poly"))
etch.lm <- lm(ed.vec ~ Af*Bf*Cf)
summary(etch.lm)
anova(etch.lm)

