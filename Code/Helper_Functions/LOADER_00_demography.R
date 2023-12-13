####################################################
###Demography and contact matrices - From Ruiyun Li#
####################################################
###USA (United States)
load("Demography/D_USA.RData")
load("Demography/W_USA.RData")
W_USA[W_USA < 0 ]<-0 ###Ensure nothing's negative

###JAPAN
load("Demography/D_JPN.RData")
load("Demography/W_JPN.RData")
W_JPN[W_JPN < 0 ]<-0 ###Ensure nothing's negative

###SOUTH AFRICA
load("Demography/D_SAF.RData")
load("Demography/W_SAF.RData")
W_SAF[W_SAF < 0 ]<-0 ###Ensure nothing's negative

###Normalize these to 1
D_JPN <- D_JPN/(sum(D_JPN))
###Normalize these to 1
D_SAF <- D_SAF/(sum(D_SAF))