## Plant data
Microsite<-c("Open","Shrub","Open","Shrub","Open","Shrub","Open","Shrub","Open","Shrub","Open","Shrub","Open","Shrub","Open","Shrub","Open","Shrub","Open","Shrub","Open","Shrub","Open","Shrub","Open","Shrub","Open","Shrub","Open","Shrub")
ShrubID<-c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13,14,14,15,15)
Abundance<-c(7,17,5,18,5,6,5,12,5,5,3,10,7,2,22,35,2,35,18,27,15,42,6,10,4,15,5,15,10,25)
Richness<-c(2,4,1,28,3,3,1,3,1,1,1,1,2,1,3,2,1,3,3,2,3,3,3,1,2,1,1,1,2,1)

se <- function(x) sqrt(var(x)/length(x))

error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
stop("vectors must be same length")
arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

library(MASS)

m1 <- glm.nb(Abundance~Microsite)
m2 <- glm.nb(Richness~Microsite)


data <- read.table("C:\\Users\\Fitz\\Dropbox\\Honours thesis students\\Ally\\Permutation Rii.csv", header=T, sep=",")

means <- aggregate(data, by=list(data$Pan),mean)

m1 <- aov(Abundance~Pan, data=data)
TukeyHSD(m1)

m2 <- aov(Richness~Pan, data=data)
TukeyHSD(m2)

m3 <- aov(Simpson~Pan, data=data)
TukeyHSD(m3)


means <- aggregate(data, by=list(data$Pan),mean)
ses <- aggregate(data, by=list(data$Pan),se)

#Abundance
plot(c(1,2,3),means$Abundance, pch=19, cex=2, xlim=c(0.5,3.5), ylim=c(-0.5,0.8), ylab="Insect Abundance", xlab="Pan Colour", cex.axis=1.5, cex.lab=1.8)
abline(h=0, lty=2, lwd=2)
error.bar(c(1,2,3),means$Abundance, ses$Abundance, lwd=2)


# Permutation in R

data <- read.table("C:\\Users\\Fitz\\Dropbox\\Honours thesis students\\Ally\\test.csv", header=T, sep=",")

#Generate 999 replicates of data and randomize

data3 <- data

for(i in 1:999)
{
n = i+1
data2 <- data[sample(nrow(data), 540), ]
data3 <- rbind(data3,data2)
}

#Calculate RII
rii <- function(x, j, var)
{
s1 <- x[x$Micro == "Shrub",]
o1 <- x[x$Micro == "Open",]
return1 <- (s1 - o1) / (s1+o1)
x1 <- x[seq(1, nrow(x), by = 2),]
return2 <- cbind(x1[j], return1[var])
return2[is.na(return2)] <- 0
print(return2)
}

rii.dat <- rii(data3, 4, 5:7)

means<- aggregate(rii.dat$Abundance, by=list(rii.dat$Pan), mean)
ses<- aggregate(rii.dat$Abundance, by=list(rii.dat$Pan), se)

plot(c(1,2,3),means[,2], pch=19, cex=2, xlim=c(0.5,3.5), ylim=c(-0.5,0.8), ylab="Insect Abundance", xlab="Pan Colour", cex.axis=1.5, cex.lab=1.8)
abline(h=0, lty=2, lwd=2)
error.bar(c(1,2,3),means[,2], ses[,2], lwd=2)

blue <- subset(rii.dat, Pan=="Blue")
white <- subset(rii.dat, Pan=="White")
yellow <- subset(rii.dat, Pan=="Yellow")

t.test(yellow$Abundance)

t.test(yellow$Richness)

t.test(yellow$Simpson)