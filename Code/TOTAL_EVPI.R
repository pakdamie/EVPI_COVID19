
##################################
###Super grid expanded countries##
##################################

#Instead of looking at one uncertainty at a time, now we run the model 
#with all different combinations of the six uncertainties (it is a very large
#file because like combinatorics). This makes Figure 2
#The data set are all  found in the TSS_ALL_CREATOR FILE 

#For easier analysis, I decided to split it up by country-
#  looked at United States (first)

#Var1 = Susceptibility
#Var2 = Transmissibility
#Var 3 = Severity
#Var 4 = Waning of Natural Immunity
#Var 5 = Waning of vaccine immunity 1
#Var 6 = Waning of vaccine immunity 2
#Var 7 = Vaccine rate 
#Var 8 = Vaccination coverage
#Var 9 = Priority group(the action)
#Var 10 = R0
#Var 11 = Country

###UNITED STATES

### This splits up the data based on the R0 (Var10) AND vaccine rate (Var7)
TSS_US_ALL$mort <- round(TSS_US_ALL$mort,2)


TSS_US_ALL_SPLIT <- split(
  TSS_US_ALL, #The US
  list(
    TSS_US_ALL$R0, # R0
    TSS_US_ALL$vacrate   # Vaccination rate
  )
)
###############################################################
###This looks at the total EVPI across all the uncertainties###                       ###########
###############################################################

TSS_EVPI_US <- lapply(TSS_US_ALL_SPLIT, function(x) EVPI_ALL_SUPER(x))

### We add in the different R0 and vacrate
EVPI_US_F <- cbind.data.frame(
  R0 = rep(c(1.15, 1.5, 2.5), 3),
  vacrate = rep(c(0.287, 0.69, 1.38), each = 3),
  do.call(rbind, TSS_EVPI_US)
)


vacrate_labellar <- c("25%","50%","75%")

###This makes one part of the figure 
##############################
###Cases for United States####
##############################
EVPI_US <- 
  ggplot(EVPI_US_F, aes(x = as.factor(R0), y = abs(evpi))) +
  geom_bar(stat = "identity", 
           fill = "#0d2848") +
  facet_wrap(~vacrate, 
             labeller = as_labeller(c(`0.287` = "25%",
                                      `0.69` = "50%",
                                      `1.38` = "75%")))+
  geom_hline(yintercept= 0)+
  geom_vline(xintercept = 0.4 ) +
  xlab("R0") +
  ylab("EVPI (Cases)") +
  ylim(0, 900) +
  ggtitle("USA")+
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 14, color = 'black'),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 15, color = 'black'),
        strip.text = element_text(size = 16),
        panel.border=element_blank())  

###This makes one part of the figure 
###################################
###Mortality for United States#####
###################################
EVPI_US_M <- ggplot(EVPI_US_F, aes(x = as.factor(R0), y = abs(evpi_m))) +
  geom_bar(stat = "identity", fill = "#0d2848",color = 'black', size = 1) +
  facet_wrap(~vacrate, 
             labeller = as_labeller(c(`0.287` = "25%",
                                      `0.69` = "50%",
                                      `1.38` = "75%")))+
  geom_hline(yintercept= 0)+
  geom_vline(xintercept = 0.4 ) +
  xlab("R0") +
  ylab("EVPI (Mortality)") +
  ylim(0, 1)+
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 14, color = 'black'),
        
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 15, color = 'black'),
        strip.text = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        panel.border=element_blank())  




#Combine
#EVPI_US / EVPI_US_M                                                                                    

###########################
###South Africa ###########
###########################

TSS_SAF_ALL_SPLIT <- split(
  TSS_SAF_ALL,
  list(
    TSS_SAF_ALL$R0, # R0
    TSS_SAF_ALL$vacrate # Vaccination rate
  )
)

TSS_EVPI_SAF <- lapply(TSS_SAF_ALL_SPLIT, function(x) EVPI_ALL_SUPER(x))

EVPI_SAF_F <- cbind.data.frame(
  R0 = rep(c(1.15, 1.5, 2.5), 3),
  vacrate = rep(c(0.287, 0.69, 1.38), each = 3),
  do.call(rbind, TSS_EVPI_SAF)
)


###This makes one part of the figure 
###################################
###Cases for South Africa     #####
###################################
EVPI_SAF <- ggplot(EVPI_SAF_F, aes(x = as.factor(R0), y = abs(evpi))) +
  geom_bar(stat = "identity", fill = "#009d9a") +
  facet_wrap(~vacrate, 
             labeller = as_labeller(c(`0.287` = "25%",
                                      `0.69` = "50%",
                                      `1.38` = "75%")))+ 
  geom_hline(yintercept= 0)+
  geom_vline(xintercept = 0.4 ) +
  xlab("R0") +
  ylab("") +
  ylim(0, 900) +
  ggtitle("South Africa")+
  theme_bw() +
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 14, color = 'black'),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 15, color = 'black'),
        strip.text = element_text(size = 16),
        panel.border=element_blank())  



###################################
###Mortality for South Africa #####
###################################
EVPI_SAF_M <- ggplot(EVPI_SAF_F, aes(x = as.factor(R0), y =abs(evpi_m))) +
  geom_bar(stat = "identity", fill = "#009d9a",color = 'black', size = 1) +
  facet_wrap(~vacrate, 
             labeller = as_labeller(c(`0.287` = "25%",
                                      `0.69` = "50%",
                                      `1.38` = "75%")))+ 
  geom_hline(yintercept= 0)+
  geom_vline(xintercept = 0.4) +
  xlab("R0") +
  ylab("") +
  ylim(0, 1)+
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 14, color = 'black'),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 15, color = 'black'),
        strip.text = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        panel.border=element_blank())  





#EVPI_SAF / EVPI_SAF_M                                                                             

######################################
###JAPAN (look above for comments )###
######################################

TSS_JPN_ALL_SPLIT <- split(
  TSS_JPN_ALL,
  list(
    TSS_JPN_ALL$vacrate,
    TSS_JPN_ALL$R0
  )
)

TSS_EVPI_JPN <- lapply(TSS_JPN_ALL_SPLIT, function(x) EVPI_ALL_SUPER(x))

EVPI_JPN_F <- cbind.data.frame(
  R0 = rep(c(1.15, 1.5, 2.5), 3),
  vacrate = rep(c(0.287, 0.69, 1.38), each = 3),
  do.call(rbind, TSS_EVPI_JPN)
)


###This makes one part of the figure 
######################
###Cases for Japan  #          
#####################
EVPI_JPN <- ggplot(EVPI_JPN_F, aes(x = as.factor(R0), y = abs(evpi))) +
  geom_bar(stat = "identity", fill = "#9f1853") +
  facet_wrap(~vacrate, 
             labeller = as_labeller(c(`0.287` = "25%",
                                      `0.69` = "50%",
                                      `1.38` = "75%")))+ 
  geom_hline(yintercept= 0)+
  geom_vline(xintercept = 0.4 ) +
  xlab("R0") +
  ylab("Cases") +
  ylim(0, 900) +
  ggtitle("Japan")+
  theme_bw()+
  theme(strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 14, color = 'black'),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(size = 15, color = 'black'),
        strip.text = element_text(size = 16),
        panel.border=element_blank())  


###This makes one part of the figure 
######################
###Cases for Japan  #          
#####################
EVPI_JPN_M <- ggplot(EVPI_JPN_F, aes(x = as.factor(R0), y = abs(evpi_m))) +
  geom_bar(stat = "identity", fill = "#9f1853") +
  facet_wrap(~vacrate,
             labeller = as_labeller(c(`0.287` = "25%",
                                      `0.69` = "50%",
                                      `1.38` = "75%"))) +
  geom_hline(yintercept= 0)+
  geom_vline(xintercept = 0.4 ) +
  xlab("R0") +
  ylab("EVPI (Mortality)") +
  ylim(0, 1)+
  theme_bw()+
  theme(strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 14, color = 'black'),
        axis.title = element_text(size = 15, color = 'black'),
        strip.text = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        panel.border=element_blank())  
#EVPI_JPN / EVPI_JPN_M                                                                             


###THIS IS FIGURE 2: TOTAL EVPI THAT I PUT THROUGH ILLUSTRATOR
###BUT SHOULD BE THE SKELETON
(EVPI_JPN + EVPI_SAF + EVPI_US) / 
  (EVPI_JPN_M + EVPI_SAF_M + EVPI_US_M)


ggsave("EVPI_TOTAL.pdf", width = 12, height = 5, units = 'in')
##############################################
###EVPXI (Expected value of partial perfect )#
##############################################
#For the expected value of partial perfect information
#I kept this in a seperate script to make it easier
#to read 
#You can find it as Main_Analysis_EVPXI.R and this is used to create 
#FIGURE 2B                                            

###################
#END SCRIPT HERE###
###################