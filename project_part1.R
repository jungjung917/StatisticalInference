
#sim_exp<- rexp(1000, 0.2)
set.seed(10302)
sim_exp_means = NULL
for (i in 1 : 1000) sim_exp_means = c(sim_exp_means, mean(rexp(40, 0.2)))
hist(sim_exp_means,freq=FALSE)
abline(v=mean(sim_exp_means),col="red")
abline(v=5.0,col="green" )

x1<-sim_exp_means
y1<- dnorm(x1, mean=5, sd=5)
plot(x1, y1)




sim_exp_mean <-mean(sim_exp_means)
sim_exp_sd <-sd(sim_exp_means)
sim_exp_mean;sim_exp_sd

x1<-sort(sim_exp_means)
y1<- dnorm(x1, mean=5, sd=0.8)
data_norm <- cbind(x1,y1)
head(data_norm)
plot(data_norm[,1], data_norm[,2],type="l")

hist(sim_exp_means,freq=FALSE)
lines(data_norm[,1], data_norm[,2])
abline(v=mean(sim_exp_means),col="red", lwd=3)
abline(v=5.0,col="green",lwd=3 )


set.seed(10302)
sim_exp_vars = NULL
for (i in 1 : 1000) sim_exp_vars = c(sim_exp_vars, var(rexp(40, 0.2)))


x2<-sort(sim_exp_vars)
y2<- dnorm(x2, mean=mean(sim_exp_vars), sd=sd(sim_exp_vars))
plot(x2,y2,type="l")

hist(sim_exp_vars,freq=FALSE)
lines(x2,y2)
abline(v=mean(sim_exp_vars),col="red")
abline(v=25.0,col="green" )
mean(sim_exp_vars)


sim_exp<- rexp(10000, 0.2)

hist(sim_exp, freq=FALSE)
lines(density(sim_exp))
hist(sim_exp_means)
