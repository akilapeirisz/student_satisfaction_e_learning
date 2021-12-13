library(MASS)
library(readxl)
install.packages("gginference")
library(gginference)
excel_data1 <- read_excel("/Users/envy/Documents/R/Assignment/Questionnaire_answers.xlsx")
attach(excel_data1)
View(excel_data1)
#with(excel_data1, tapply(Satisfied_e_learning, Attended_physical_lec, mean))

#t.test(excel_data1$Satisfied_e_learning~excel_data1$Gender, paired=F, var.eq=F) 

d <- excel_data1
df<- d$Satisfied_e_learning

#t.test(d$Satisfied_e_learning~d$feed, paired=F, var.eq=F) 

#### one sample t test (one sided) #####
#a<-t.test (df, alternative="less", mu=3, conf.int=0.95)
#a


#### Bootstrap #####
set.seed(112358)   # for reproducibility
n <- length(d$Satisfied_e_learning)  # the number of observations to sample
n
B <- 10000  # the number of bootstrap samples
variable <- d$Satisfied_e_learning  # the variable we will resample from

elearning_satis_bootstrap <- matrix( sample(variable, size= n*B, replace=TRUE), 
                            nrow=n, ncol=B)
#dim(elearning_satis_bootstrap)

#### one sample t test (one sided) #####
aa <- t.test (elearning_satis_bootstrap, alternative="less", mu=3, conf.int=0.99)
aa

ggttest(aa)

aaa <- t.test (df, alternative="less", mu=3, conf.int=0.99)
ggttest(aaa)
aaa




test.stat1 <- abs(mean(d$Satisfied_e_learning[d$Gender=="Female"]) - mean(d$Satisfied_e_learning[d$Gender=="Male"]))  #diff in means
t

BootstrapSamples <- matrix( sample(variable, size= n*B, replace=TRUE), 
                                     nrow=n, ncol=B)

Boot.test.stat1 <- rep(0,B)
for (i in 1:B){
  # calculate the boot-test-stat1 and save it
  Boot.test.stat1[i] <- abs( mean(BootstrapSamples[1:29,i]) - 
                               mean(BootstrapSamples[30:70,i]) )
}


# before going too far, let's remind ourselves of the OBSERVED TEST STATS
#test.stat1;
# and, take a look at the first 20 Bootstrap-TEST STATS for 1 and 2
#round(Boot.test.stat1, 1)

# and, let's calculate the bootstrap p-value...
# notice how we can ask R a true/false question...(for the first 20)
(Boot.test.stat1 >= test.stat1)
# and if we ask for the mean of all of those, it treats 0=FALSE, 1=TRUE
#...calculate the p-value
mean( Boot.test.stat1 >= test.stat1)
# p > alpha (0.025)
# can't reject H0
# H0 e-learning student satisfaction is equal for both males and females
# H1 female satisfaction is higher than male satisfaction

# let's take a look at a density plot of all the Bootstrap test-stats, and 
# add in our Observed test stat


a2 <- t.test (d$Satisfied_e_learning, alternative="greater", mu=0, conf.int=0.99)
ggttest(a2)
a2

Boot.test.stat1

library(readxl)
excel_data2 <- read_excel("/Users/envy/Documents/R/Assignment/Questionnaire_answers.xlsx")
attach(excel_data2)
View(excel_data2)

d2 <- excel_data2

#### Bootstrap #####
set.seed(112358)   # for reproducibility
#n <- length(d2$Satisfied_e_learning)  # the number of observations to sample
#n
#B <- 10000  # the number of bootstrap samples
variable2 <- d2$Satisfied_e_learning  # the variable we will resample from

test.stat2 <- abs(mean(d2$Satisfied_e_learning[d$Attended_physical_lec=="0"]) - mean(d2$Satisfied_e_learning[d$Attended_physical_lec=="1"]))  #diff in means
#t

elearning_satis_bootstrap2 <- matrix( sample(variable2, size= n*B, replace=TRUE), 
                                      nrow=n, ncol=B)

Boot.test.stat2 <- rep(0,B)
for (i in 1:B){
  # calculate the boot-test-stat1 and save it
  Boot.test.stat2[i] <- abs( mean(elearning_satis_bootstrap2[1:23,i]) - 
                               mean(elearning_satis_bootstrap2[24:70,i]) )
}

mean( Boot.test.stat2 >= test.stat2)
# p > alpha (0.025)
# can't reject H0
# H0 e-learning student satisfaction is equal for both males and females
# H1 satisfaction is higher among students who have attended on premise lectures


a5 <- elearning_satis_bootstrap

original_sample.n <- length(a5)
original_sample.mean <- mean(a5)
original_sample.sd <- sd(a5)
original_sample.se <- original_sample.sd/sqrt(original_sample.n)

t <- (original_sample.mean - 3)/original_sample.se
t


df <- Boot.test.stat2


#install.packages("gginference")
library(gginference)

#Manual Calculation
tps <- d2$Satisfied_e_learning[d2$Attended_physical_lec=="0"]
tps2 <- d2$Satisfied_e_learning[d2$Attended_physical_lec=="1"]


original_sample.n <- length(tps)
original_sample.mean <- mean(tps)
original_sample.sd <- sd(tps)
original_sample.se <- original_sample.sd/sqrt(original_sample.n)
original_sample.t_val <- (original_sample.mean - 3)/original_sample.se

#For Original Sample
original_sample.t <- t.test(x=tps, y=tps2, alternative="less", mu=0)
ggttest(original_sample.t)
original_sample.t

print("Original Sample Stats")
original_sample.n
original_sample.mean
original_sample.sd
original_sample.se
original_sample.t_val
original_sample.t

#Bootstraping
B<-1000

Boots.BootstrapSamples <- matrix( sample(tps, size= original_sample.n*B, replace=TRUE), 
                                  nrow=original_sample.n, ncol=B)
Boots.means<-c()
Boots.means22<-c()

for (i in 1: B){
  Boots.means[i] <- mean(Boots.BootstrapSamples[1:23,i])
  Boots.means22[i] <- mean(Boots.BootstrapSamples[24:70,i])
}

Boots.t <- t.test(x=Boots.means, alternative="less", mu=3)
ggttest(Boots.t)

print("Bootstrap Samples Stats")
Boots.t










#install.packages("gginference")
library(gginference)

#Manual Calculation
tps <- d2$Satisfied_e_learning[d2$Gender=="Male"]
tps2 <- d2$Satisfied_e_learning[d2$Gender=="Female"]


original_sample.n <- length(tps)
original_sample.mean <- mean(tps)
original_sample.sd <- sd(tps)
original_sample.se <- original_sample.sd/sqrt(original_sample.n)
original_sample.t_val <- (original_sample.mean - 3)/original_sample.se
hist(tps)

#For Original Sample
original_sample.t <- t.test(x=tps, y=tps2, alternative="greater", mu=0)
ggttest(original_sample.t)
original_sample.t