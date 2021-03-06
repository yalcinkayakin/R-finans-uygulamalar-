library(evir)
data("danish")
summary(danish)
hist(danish , breaks = 200 , xlim = c(0,10))
sum(danish>20 / length(danish))
sum(danish[danish>20])/ sum(danish)

#tail behavior of claims...
emplot(danish)
emplot(danish, alog = "xy")
qplot(danish,trim = 100)

meplot(danish , omit = 4)

#fitting a gpd distribution to the tails
gpdfit <- gpd(danish,threshold = 10)
gpdfit$converged
gpdfit$par.ests
gpdfit$par.ses

#Quantile estimation using the fitted GPD model
tp <-tailplot(gpdfit)
gpd.q(tp,pp=0.999,ci.p = 0.95)
quantile(danish,probs = 0.999,type = 1)

#calculation of expected loss using the fitted GPD model
tp <- tailplot(gpdfit)
gpd.q(tp,pp=0.99)

gpd.sfall(tp,0.99)





