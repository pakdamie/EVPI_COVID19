###Demography and Contact Matrix Figure
library(ggplot2)
library(reshape2)
library(viridis)
library(patchwork)
###Demography and contact matrices - From Ruiyun Li
load("Demography/D_USA.RData")
load("Demography/W_USA.RData")
W_USA[W_USA < 0 ]<-0

load("Demography/D_JPN.RData")
load("Demography/W_JPN.RData")
W_JPN[W_JPN < 0 ]<-0
load("Demography/D_SAF.RData")
load("Demography/W_SAF.RData")
W_SAF[W_SAF < 0 ]<-0

###Normalize these to 1
D_JPN <- D_JPN/(sum(D_JPN))
###Normalize these to 1
D_SAF <- D_SAF/(sum(D_SAF))


#########################################
#########################################
colnames(W_USA) <- seq(1,80)
rownames(W_USA) <- seq(1,80)

colnames(W_JPN) <- seq(1,80)
rownames(W_JPN) <- seq(1,80)

colnames(W_SAF) <- seq(1,80)
rownames(W_SAF) <- seq(1,80)

W_USA_MELT <- melt(W_USA)


MELTED_W_USA <- melt(W_USA)
MELTED_W_USA$id <- 'United States'

MELTED_W_JPN <- melt(W_JPN)
MELTED_W_JPN$id <- 'Japan'

MELTED_W_SAF <- melt(W_SAF)
MELTED_W_SAF$id <- 'South Africa'

MELTED_W_ALL <- rbind(MELTED_W_USA, MELTED_W_JPN,
                      MELTED_W_SAF)

Contact_Rate <- ggplot(MELTED_W_ALL, aes(x = Var1, y= Var2, fill=value))+
  geom_tile()+facet_wrap(~id)+scale_fill_viridis(name="Contact rate",option='turbo')+
  coord_equal()+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  theme(strip.background = element_blank())+xlab("Age group 1")+ylab("Age group 2")

################################################
##############DEMOGRAPHY #######################
#################################################
D_USA_DF <- cbind.data.frame(value=D_USA, age=seq(1,80), id = 'United States')
D_JPN_DF <- cbind.data.frame(value=D_JPN, age=seq(1,80), id = 'Japan')
D_SAF_DF <- cbind.data.frame(value=D_SAF, age=seq(1,80), id = 'South Africa')


D_ALL_DF <- rbind(D_USA_DF, D_JPN_DF, D_SAF_DF)

Demography <- ggplot(D_ALL_DF , aes(x = age, y= value))+
  geom_col(color='black')+facet_wrap(~id)+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+theme_classic()+
  theme(strip.background = element_blank())+xlab("Age")+ylab("Proportion")

Contact_Rate/Demography
