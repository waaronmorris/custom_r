
data=c(324.93, 324.68, 324.73, 324.35, 325.35, 325.23, 324.13, 324.53, 325.23, 324.60, 324.63, 325.15,
       328.33, 327.25, 327.83, 328.50, 326.68,327.78,326.88,328.35)
set.seed(1)
c<-cusum_score(data=data, delta=1, mu =325, sdv = 0.635)
plot(c$cusum,ylim=c(-20,20),xlab="Index",ylab="Cumulative Sum")
grid()
lines(c$s_hi,col="Blue")
points(c$s_hi,col="Blue",pch=18)
lines(c$s_lo,col="Red")
lines(c$h)
lines(-c$h)
points(c$s_lo,col="Red",pch=18)
legend(x=1,y=20,c("CUSUM","Upper_alert","Lower_alert"),
       col=c("Black","Blue","Red"),
       pch=c(1,18,18),
       lty=c(1,1,1))

plot(data, type = 'l')