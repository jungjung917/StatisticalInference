### sample means
set.seed(10302)
sim_exp_means = NULL
for (i in 1 : 1000) sim_exp_means = c(sim_exp_means, mean(rexp(40, 0.2)))

mean(sim_exp_means)
sd(sim_exp_means)

x1<-sort(sim_exp_means)
y1<- dnorm(x1, mean=mean(sim_exp_means), sd=sd(sim_exp_means))

hist(sim_exp_means,freq=FALSE,col="lightblue",breaks=40, xlab="Sample means")
abline(v=5.0,col="red",lwd=4)
lines(x1,y1,type="l",lwd=2)

### sample variances
set.seed(10302)
sim_exp_vars = NULL
for (i in 1 : 1000) sim_exp_vars = c(sim_exp_vars, var(rexp(40, 0.2)))

x2<-sort(sim_exp_vars)
y2<- dnorm(x2, mean=mean(sim_exp_vars), sd=sd(sim_exp_vars))
data_var_norm <- cbind(x2,y2)

hist(sim_exp_vars,freq=FALSE,breaks=40,col="lightblue",xlab="Sample variances")
lines(x2,y2)
lines(density(sim_exp_vars))
abline(v=25.0,col="red", lwd=4 )

#distributions


#sim_exp <- rexp(1000,.2)
set.seed(10033)
sim_exp <- rexp(1000,.2)
x3 <- sort(runif(1000,min=0,max=40))
y3<-dexp(x3, rate = .2, log = FALSE)
par(mfrow=c(1,2))
hist(sim_exp, freq=FALSE,breaks=40,col="lightblue")
lines(x3,y3,type='l')

## 
set.seed(10302)
sim_exp_means = NULL
for (i in 1 : 1000) sim_exp_means = c(sim_exp_means, mean(rexp(40, 0.2)))
par(mfrow=c(1,2))
hist(sim_exp_means,freq=FALSE,col="lightblue",breaks=40, xlab="Sample means")
abline(v=5.0,col="red",lwd=4)
abline(v=mean(sim_exp_means),col="green",lwd=4)
lines(density(sim_exp_means))
lines(x1,y1,type="l",lwd=2)

for (i in 1 : 10000) sim_exp_means = c(sim_exp_means, mean(rexp(40, 0.2)))
fit<-density(sim_exp_means)
yy <- rnorm(1000, sample(sim_exp_means, size = 1000, replace = TRUE), fit$bw)
hist(sim_exp_means,freq=FALSE,col="lightblue",breaks=40, xlab="Sample means")
lines(fit)
lines(density(yy), col = "blue")
lines(x1,y1,type="l",lwd=2)



