# to get reproducible results
set.seed(42) 

# -----
# ----- Non-Weather Driven Advertising Simulation
# -----
# Stage 1
# Determine the number of buyers
nb<-rbinom(n=1000,size=1000,prob=.025)

# Stage 2
# Probability of a good match between weather & advertiser preferences
pg<-pmax(rnorm(1000, mean = 0.2, sd = .1),0)
# Probability of a moderate match between weather & advertiser preferces
pm<-(1-pg)/2

# Stage 3a
# Probability of a return advertiser, given good weather match
prg<-rnorm(1000, mean = 0.6, sd = .1)
# Probability of a return advertiser, given a moderate weather match
prm<-rnorm(1000, mean = 0.25, sd = .1)

# Stage 3b
# Probability of a return advertiser, given a bad weather match
prb<-rnorm(1000, mean = 0.2, sd = .1)

myset<-as.data.frame(cbind(nb,pg,pm,prg,prm,prb))


Rev1<-700 # Revenue for an ad, non-weather scenario

Ren1<-1400 # Value of a returning customers, non-weather scenario

# Rev1 Calc
myset$brev<-nb*Rev1

# nren calc
myset$ren<-floor((nb*pg*prg)+(nb*pm*prm)+(nb*(1-pg-pm)*prb))

# RenRev Calc
myset$renrev<-myset$ren*Ren1

# TtlRev Calc
myset$rev<-myset$brev+myset$renrev

hist(myset$rev)

# -----
# ----- Weather Driven Advertising Simulation
# -----
# Stage 1
nb<-rbinom(n=1000,size=1000,prob=.035)

# Stage 2
# Probability that the ad will be selected & run 
prun<-rnorm(1000, mean = 0.8, sd = .1)

# Probability of a good match between weather & advertiser preferences
pg<-pmax(rnorm(1000, mean = 0.8, sd = .1),0)
# Probability of a moderate match between weather & advertiser preferces
pm<-(1-pg)/2


# Probability of a return advertiser, given good weather match
prg<-rnorm(1000, mean = 0.9, sd = .1)
# Probability of a return advertiser, given a moderate weather match
prm<-rnorm(1000, mean = 0.50, sd = .1)

# Stage 3b
# Probability of a return advertiser, given a bad weather match
prb<-rnorm(1000, mean = 0.2, sd = .1)


mywxset<-as.data.frame(cbind(nb,prun,pg,pm,prg,prm,prb))

Rev2<-750 # Revenue for an ad, Weather driven scenario

Ren2<-Rev2*3 # Value of a returning customers, Weather driven scenario

# Rev2 Calc
mywxset$brev<-nb*prun*Rev2

# nren, normal calc
mywxset$ren<-floor((nb*prun*pg*prg)+(nb*prun*pm*prm)+(nb*prun*(1-pg-pm)*prb))

# Number of advertisers to be run at the next iteration
mywxset$runlater<-nb-(nb*prun)

# Renewal Revenue Calculation: Renewal Customers + Ad that did not run but will run later
mywxset$renrev<-(mywxset$ren*Ren2)+mywxset$runlater*Rev2

# Total Revenue Calculation: Initial Purchase Revenue + Renewal Revenue
mywxset$rev<-mywxset$brev+mywxset$renrev

hist(mywxset$rev)

# ---------
# ---- Explore and Compare Results
# ---------
summary(myset$rev)
summary(mywxset$rev)

# Visulaize revenue distributions for each simulation
require(lattice)
histogram(~V1|V2,data=delme4,layout=c(1,2))

boxplot(myset[,c('brev','renrev','rev')])
boxplot(mywxset[,c('brev','renrev','rev')])

# ---------
# ---- Determine ROI Range
# ---------
PlatformCost<-25000
# Expected ROI is the median incremental revnue divided by the infrastructure cost
ExpectedROI<-(median(mywxset$rev)-median(myset$rev))/PlatformCost

# Low ROI is the incremental revenue between non-weather mean revenue + 1 standard deviation
# and weather mean revenue - 1 standard deviation, divided by infra cost
LowROI<-( mean(mywxset$rev)-sd(mywxset$rev)   -   mean(myset$rev)+sd(myset$rev)   )     /PlatformCost
HighROI<-( mean(mywxset$rev)+sd(mywxset$rev)   -   mean(myset$rev)-sd(myset$rev)   )     /PlatformCost
