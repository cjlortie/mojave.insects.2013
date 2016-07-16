
### RDA

data.rda <- read.table("C:\\Users\\Fitz\\Dropbox\\Honours thesis students\\RDA\\ruttan rda 2.csv", header=T,sep=",")
data.rda[is.na(data.rda)] <- 0

library(vegan)


# Call Variables
Resp <- data.rda[,5:56]
Expl <- data.rda[,3:4]

# Hellinger transformation --> Legendre and Gallagher 2001
Respnorm <-decostand(Resp, method="hellinger")
#Respnorm <-decostand(Resp, method="total")

rda1 <- rda(Respnorm~ Expl$Micro  * Expl$Pan, scaling=1)
par(mar=c(5.1,5.1,2.5,2.1))
plot(rda1, display="lc", ylim=c(-0.3,0.25), xlim=c(-0.6,0.5))

abu <- colSums(Respnorm)

ordilabel(rda1, col = "red", dis = "sp", priority = abu)
ordilabel(rda1, col = "blue", dis = "lc")


Expl[539,1:2]
Expl[537,1:2]
Expl[535,1:2]
Expl[536,1:2]
Expl[538,1:2]
Expl[540,1:2]



### Unbalanced 2-way Anova using RDA

Resp <- data.rda[,5:56]
Expl <- data.rda[,3:4]

n <- nrow(Resp)
p <- ncol(Resp)

# A = Micro
# B = Pan

A <- as.factor(Expl[,1])
B <- as.factor(Expl[,2])

ncol.A <- nlevels(A)-1
ncol.B <- nlevels(B)-1
ncol.int <- ncol.A * ncol.B
model.mat <- model.matrix(~ A*B, contrasts=list(A="contr.helmert", B="contr.helmert"))[,-1]

model.A <- as.matrix(model.mat[,1:ncol.A])
model.B <- as.matrix(model.mat[,(ncol.A+1):(ncol.A+ncol.B)])
model.int <- as.matrix(model.mat[,(ncol.A+ncol.B+1):ncol(model.mat)])

# Test of factor A
out.rda <- rda(Resp, model.A, cbind(model.B, model.int))
int.out <- anova(out.rda, perm.max=(999+1), step=(999+1))
out.A <- int.out[1,]

# Test of factor B
out.rda <- rda(Resp, model.B, cbind(model.A, model.int))
int.out <- anova(out.rda, perm.max=(999+1), step=(999+1))
out.B <- int.out[1,]

# Test of interaction
out.rda <- rda(Resp, model.int, cbind(model.A, model.B))
int.out <- anova(out.rda, perm.max=(999+1), step=(999+1))
out.AB <- int.out[1,]
out.res <- int.out[2,]

out <- rbind(out.A, out.B, out.AB, out.res)
out$Var <- out$Var*(n-1)/out$Df
rownames(out) <- c("Micro","Pan","Micro*Pan","Residuals")
if(p == 1) {
	out <- as.data.frame(cbind(out[,1:4], P.param, out[,5]))
	colnames(out) <- c("Df","MeanSq","F-stat","N.perm","P.param","Pr(>F)")
	} else {
	colnames(out) <- c("Df","MeanSq","F-stat","N.perm","Pr(>F)")
	}

out <- list(Anova_unbalanced=out)
class(out) <- "anova.unbalanced"
out

### Results
# $Anova_unbalanced
# Permutation test for rda under reduced model

# Model: rda(X = Resp, Y = model.A, Z = cbind(model.B, model.int))
             # Df MeanSq F-stat N.perm Pr(>F)    
# Micro         1 620.45 6.6993    999  0.001 ***
# Pan           2 242.99 2.6237    999  0.001 ***
# Interaction   2 106.79 1.1530    999  0.313    
# Residuals   534  92.61                         
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

# attr(,"class")
# [1] "anova.unbalanced"
