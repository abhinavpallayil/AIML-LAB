#https://cran.r-project.org/doc/contrib/Faraway-PRA.pdf

sink("./Output.txt",append=T)

library(faraway)
data(eco)

#jpeg("Proportion US born vs. Mean Annual Income 1.jpeg",quality = 100)
plot(income~usborn, data=eco, xlab="Proportion US born",ylab="Mean Annual Income")
#dev.off()

g <- lm(income~usborn, eco)
summary(g)

#("Proportion US born vs. Mean Annual Income 2.jpeg",quality = 100)
plot(income~usborn, data=eco, xlab="Proportion US born",ylab="Mean Annual Income",xlim=c(0,1),ylim=c(15000,70000),xaxs="i")
#dev.off()

abline(g$coef)
data(chicago)
chicago
ch <- data.frame(chicago[,1:4],involact=chicago[,6],income=chicago[,7]/1000)
ch
summary(ch)
par(mfrow=c(2,3))
for(i in 1:6) hist(ch[,i],main=names(ch)[i])
for(i in 1:6) boxplot(ch[,i],main=names(ch)[i])
pairs(ch)
summary(lm(involact~race,data=ch))
g <- lm(involact~race + fire + theft + age + log(income), data = ch)
plot(g$fit,g$res,xlab="Fitted",ylab="Residuals",main="Residual-Fitted plot")
abline(h=0)
qqnorm(g$res)
gi <- lm.influence(g)
for(i in 1:5) qqnorml(gi$coef[,i+1],main=names(ch)[-5][i])
qqnorml(rstudent(g),main="Jacknife Residuals")
qt(0.05/(2*47),47-6-1)
halfnorm(cooks.distance(g),main="Cook-Statistics")
ch[c(6,24),]
g <- lm(involact~race + fire + theft + age + log(income),ch,subset=(1:47)[-c(6,24)])
summary(g)
g2 <- lm(involact~race + poly(fire,2) + poly(theft,2) + poly(age,2) + poly(log(income),2), ch, subset=(1:47)[-c(6,24)])
anova(g,g2)
y <- ch$inv[cooks.distance(g) < 0.2]
x <- cbind(ch[,1:4],linc=log(ch[,6]))
x <- x[cooks.distance(g) < 0.2,]
library(leaps)
a <- leaps(x,y)
Cpplot(a)
g <- lm(involact~race + fire + theft + age, ch, subset=(1:47)[-c(6,24)])
summary(g)
galt <- lm(involact~race+fire+log(income),ch,subset=(1:47)[-c(6,24)])
summary(galt)
galt <- lm(involact~ race+fire,ch,subset=(1:47)[-c(6,24)])
summary(galt)
g <- lm(involact~race + fire + theft + age, data=ch)
summary(g)
data(chiczip)
g <- lm(involact~race + fire + theft +age, subset=(chiczip == "s"), ch)
summary(g)
g <- lm(involact~ race + fire + theft +age, subset=(chiczip == "n"), ch)
summary(g)

sink()
