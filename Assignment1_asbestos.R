
#####################################################
############Assignment 1: asbestos###################
#####################################################

#Author: Iwan Vaandrager
#Date: 04/11/2018
library(stats)
library(dplyr)

# p is proportion
#PAR = p * (p2-p1). slides
#PAR = p_exposed*(RR-1)/(1+p_exposed*(RR-1)), where RR is Incidence_exposed/Incidence_unexposed. wikipedia

#(i) the mesothelioma/lungcancer risks in individuals with “low” asbestos exposure (p1)
#Tabel with only high.exposure = 0. Calculate average. Bootstrap for variance and CI.

#(ii) the mesothelioma/lungcancer risks in individuals with  “relatively high” asbestos exposure (p2)
#Tabel with only high.exposure = 1. Calculate average. Bootstrap for variance and CI.

#(iii) an estimate of the population relative frequency of “relatively high” asbestos exposure (p) 
#      (report PAR)
#which is p. Find var(p) with taking mean of high.exposure of original. Bootstrap.

#delta method on var(p1) var(p2)  and var(p)

#(iv) For all give 95% CI, both through bootstrap or delta method

#load csv file
mesothelioma.file <- read.csv(file = 'mesothelioma.csv', header = TRUE)

#calculate 95% CI of high exposure risk (mean) using bootstrap
meso.small <- mesothelioma.file[,2:3]

pbootstrap=c()
for (i in 1:1000) {
  bootx = sample(meso.small$high.expsoure, length(meso.small$high.expsoure),replace=TRUE)
  pbootstrap[i] = mean(bootx)
}
hist(pbootstrap, main = 'Bootstrapped chance of high asbestos')
quantile(pbootstrap,probs=c(0.025,0.50,0.975))

#calculate 95% CI of cancer risk (mean) for non-high asbestos exposure using bootstrap
lowrisk <- meso.small[meso.small$high.expsoure == 0,]
p1bootstrap = c()
for (i in 1:1000) {
  bootx = sample(lowrisk$mesothelioma.or.lungcancer, length(lowrisk$mesothelioma.or.lungcancer),replace=TRUE)
  p1bootstrap[i] = mean(bootx)
}
hist(p1bootstrap)
quantile(p1bootstrap,probs=c(0.025,0.50,0.975))

#calculate 95% CI of cancer risk (mean) for high asbestos exposure using bootstrap
highrisk <- meso.small[meso.small$high.expsoure == 1,]

p2bootstrap=c()
for (i in 1:1000) {
  bootx = sample(highrisk$mesothelioma.or.lungcancer, length(highrisk$mesothelioma.or.lungcancer),replace=TRUE)
  p2bootstrap[i] = mean(bootx)
}
hist(p2bootstrap)
quantile(p2bootstrap,probs=c(0.025,0.50,0.975))

t.test(p2bootstrap,p1bootstrap)


#Calculate PAR for single instance
p <- mean(lowrisk$mesothelioma.or.lungcancer)
p2 <- mean(highrisk$mesothelioma.or.lungcancer)
p1 <- mean(lowrisk$mesothelioma.or.lungcancer)
singlePAR <- p*(p2-p1); singlePAR


total.highrisk <- sum(mesothelioma.file$high.expsoure == "1"); total.highrisk
total.lowrisk <-  sum(mesothelioma.file$high.expsoure == "0"); total.lowrisk
total.highrisk+total.lowrisk == length(mesothelioma.file$high.expsoure)
total.cancer <- sum(mesothelioma.file$mesothelioma.or.lungcancer == "1"); total.cancer
# 
# highrisk.cancer <- sum(mesothelioma.file$high.expsoure == "1" & mesothelioma.file$mesothelioma.or.lungcancer == '1')
# highrisk.nocancer <- sum(mesothelioma.file$high.expsoure == "1" & mesothelioma.file$mesothelioma.or.lungcancer == '0')
# lowrisk.cancer <- sum(mesothelioma.file$high.expsoure == "0" & mesothelioma.file$mesothelioma.or.lungcancer == '1')
# lowrisk.nocancer <- sum(mesothelioma.file$high.expsoure == "0" & mesothelioma.file$mesothelioma.or.lungcancer == '0')


