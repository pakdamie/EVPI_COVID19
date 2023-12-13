###############################
###R SCRIPT TO MAKE FIGURE 2###
################################
###Important script for outputs

###THIS FUNCTION CALCULATES THE OUTPUT- CASELOAD AND MORTALITY-
###You run this before you create the graph
Output_Calculator <- function(list1,listofvariables1,time){

   ###The times we are doing with (2 years with 1/12 months)
  times = seq(0,1,1/12)

  ###This gets the total incidence- this is the total incidence
  ###of the symptomatic (both unvaccinated and vaccinated) with the
  ###asymptomatic, because everything is in proportion we multiplied
  ###it
  TOTAL_INC_ALL<- lapply(list1, function (x) 1e5 * rowSums(x[,722:961]))

  ###This adds the time data.fame to each list entry so
  ###we can subset-based on what time we want

  TOTAL_INC_ALL_T <- lapply(TOTAL_INC_ALL, function (x) data.frame(times, x))

  ###Allows user to pick what time they want to subset-
  ###I chose 1
  TOTAL_INC_ALL_T_S<- lapply(TOTAL_INC_ALL_T ,function(x) x[x$times==time,])

  ###Combines the variable together and d represents
  ###the total cases
  TOTAL_INC_ALL_T_F<- cbind.data.frame(do.call(rbind, TOTAL_INC_ALL_T_S),listofvariables1)

  ###Decide to split it up for easier reading

  ###Death- the CFR was gotten from Verity- This was in percentages and I
  ###multiplied it by 1/100 to get the actualvalue. Only,
  ###symptomatic people can die (it wouldn't make sense
  ###if you were asymptomatic and die from COVID-19?)
  CFR_Verity <- c(.00260, .0148, .0600, 0.146,.295, 1.25, 3.99, 8.61, 13.40)/100

  ###From Ruyiun's code where I use splines to annualize
  ###contact her if you want more information
  CFR = array(NA, 80)

  CFR_sp <- spline(log(CFR_Verity[1:8]), xout=1:8) # log: to keep pos
  CFR_sp_pred = spline(CFR_sp$y, xout=seq(0,8,1/10))

  ### This is the CFR for unvaccinated
  CFR = exp(CFR_sp_pred$y)[-1]
  ### This is the CFR for vaccinated (90% reduction)
  CFR_V= exp(CFR_sp_pred$y)[-1] * 0.1

  ###This is the for unvaccinated,sympomatic total cases
  a_IU<- lapply(list1, function (x)  (x[,722:801]))

  ###This is the for unvaccinated,sympomatic total cases
  a_IV <- lapply(list1, function (x) (x[,802:881]))

  ###We check for times
  b_IU <- lapply(a_IU, function (x) data.frame(times, x))
  b_IV <- lapply(a_IV, function (x) data.frame(times, x))

  ###We get the total deaths by taking the total cases multiplied by the CFR
  c_IU <- lapply(b_IU, function(x) sum((1e5 * x[x$times==time, 2:81]) *(CFR)))
  c_IV <- lapply(b_IV, function(x) sum((1e5 * x[x$times==time, 2:81]) *(CFR_V)))


  death_IU<- do.call(rbind, c_IU)
  death_IV<- do.call(rbind, c_IV)

  ###ADD this together to get the total mortality
  TOTAL_INC_ALL_T_F$mort <-   death_IU + death_IV

  return(TOTAL_INC_ALL_T_F)


}


###########################
###Misc script for ggplot##
###########################
grapher_output <- function(x, uncertainty, vacornot) {
  uncertainty <- uncertainty

  if (vacornot == "vac") {
    label <- c("Life-long", "2 Year", "1 year", "6 months")
  } else {
    label <- c("High Reduction", "Med Reduction", "Low Reduction")
  }

  if (uncertainty == "sus") {
    title <- "Susceptibility"
  } else if (uncertainty == "trans") {
    title <- "Transmissibility"
  } else if (uncertainty == "sev") {
    title <- "Severity"
  } else if (uncertainty == "wn") {
    title <- "Waning immunity (natural)"
  } else if (uncertainty == "wv1") {
    title <- "Waning immunity V1 (vaccinated)"
  } else if (uncertainty == "wv2") {
    title <- "Waning immunity V2 (vaccinated)"
  }




  ggplot_output_C <- NULL
  ggplot_output_M <- NULL




  Country_dat <- split(x, x$country)

  for (k in seq(1, 3)) {
    ggplot_output_C[[k]] <- ggplot(
      Country_dat[[k]],
      aes(x = inc, y = as.factor(get(uncertainty)))
    ) +
      geom_tile(aes(fill = cases, color = min_cases),
                size = 0.9, width = 0.95, height = 0.92
      ) +
      facet_grid(vacrate ~ R0) +
      scale_fill_viridis(option = "mako", name = "Cases") +
      scale_color_manual(values = c("red", NA)) +
      geom_label(aes(label = formatC(cases, digits = 3, format = "e")), color = "black") +
      scale_x_discrete(
        expand = c(0, 0),
        labels = c("18-30", "31-59", "60-80")
      ) +
      scale_y_discrete(
        expand = c(0, 0),
        labels = c(label)
      ) +
      xlab("Action") +
      ylab(title) +
      ggtitle(paste0("Accumulated Cases ", Country_dat[[k]]$country,
                     title,
                     sep = " - "
      )) +
      output_theme
  }

  for (d in seq(1, 3)) {
    ggplot_output_M[[d]] <- ggplot(
      Country_dat[[d]],
      aes(x = inc, y = as.factor(get(uncertainty)))
    ) +
      geom_tile(aes(fill = mort, color = min_mort),
                size = 0.9, width = 0.95, height = 0.92
      ) +
      facet_grid(vacrate ~ R0) +
      scale_fill_viridis(option = "mako") +
      scale_color_manual(values = c("red", NA)) +
      geom_label(aes(label = formatC(mort, digits = 3, format = "e")), color = "black") +
      scale_x_discrete(
        expand = c(0, 0),
        labels = c("18-30", "31-59", "60-80")
      ) +
      scale_y_discrete(
        expand = c(0, 0),
        labels = label
      ) +
      xlab("Action") +
      ylab(title) +
      ggtitle(paste0("Total Mortality", Country_dat[[d]]$country,
                     title,
                     sep = " - "
      )) +
      output_theme
  }
  return(c(ggplot_output_C, ggplot_output_M))
}

###Highlights the most important
Minimizer_Highlighter <- function(x,uncertainty){

  ###Replace the column names
  colnames(x) <- c("times","cases","sus",
                   "trans","sev","wn",
                   "wv1","wv2","vacrate",
                   "vacall","inc","R0",
                   "country","mort")


  ###this splits up the output data.frame based
  ###on the different vaccination rates, R0, and country,
  ###and the uncertainty value that we're interested in-
  ###this creates a list element that we can lapply with

  tmp<- split(x,list(x$vacrate,
                     x$R0, x$country,
                     x[,uncertainty]))


  ###This is a function that says- for each list element,
  ###if the case number is the minimum -say it's the minimum, if not
  ###then say it's not- this data.frame allows me to plot it in
  ###ggplot2
  cases_min<- lapply(tmp, function(x) ifelse((x$cases== min(x$cases))==TRUE,'Min','Not' ))

  mortality_min <- lapply(tmp, function(x) ifelse((x$mort== min(x$mort))==TRUE,'Min','Not' ))


  ###This makes it so that it gives me back the data.frame to
  ###graph
  for(k in seq(1, length(tmp))){
    tmp[[k]]$min_cases <-   cases_min[[k]]
    tmp[[k]]$min_mort <- mortality_min[[k]]

  }
  return(do.call(rbind, tmp))

}

###Grapher Total
grapher_output_Total <- function(x, uncertainty){
    
    Minimized_Full_DF <- Minimizer_Highlighter(x, uncertainty)
  
    Minimized_Full_DF_Optimal_Cases <- subset(Minimized_Full_DF, 
                                      Minimized_Full_DF$min_cases == "Min")
  
    Minimized_Full_DF_Optimal_Mort <- subset(Minimized_Full_DF, 
                                            Minimized_Full_DF$min_mort == "Min")
   FULL_GG <- NULL 
  
   case_graph <- ggplot(Minimized_Full_DF_Optimal_Cases, 
         aes(x = as.factor(get(uncertainty)), 
                 y = country)) + 
    geom_tile(aes(fill = inc),color = 'black',size = 0.8)+ 
    facet_grid(vacrate~R0,
               labeller = label_both)+
    scale_x_discrete(limits = rev) +
    scale_fill_manual(values = c("age1" = "#003f5c" ,
                                 "age2" = "#bc5090" ,
                                 "age3" = "#e5c733" ))+
     xlab("")+
     ylab("")+
     theme_classic()+
     theme(axis.line = element_blank(),
           strip.background = element_blank(),
           legend.position = 'none')+
     coord_equal()

  mort_graph <-  ggplot(Minimized_Full_DF_Optimal_Mort, 
                        aes(x = as.factor(get(uncertainty)), 
                            y = country)) + 
    geom_tile(aes(fill = inc),color = 'black', size = 0.8)+ 
    facet_grid(vacrate~R0,
               labeller = label_both)+
    scale_x_discrete(limits = rev) +
    scale_fill_manual(values = c("age1" = "#003f5c" ,
                                 "age2" = "#bc5090" ,
                                 "age3" = "#e5c733" ))+
    xlab("")+
    ylab("")+
    theme_classic()+
    theme(axis.line = element_blank(),
          strip.background = element_blank(),
          legend.position = 'none')+
    coord_equal()
  
  FULL_GG[[1]] <- case_graph 
  FULL_GG[[2]] <- mort_graph
  
  return(FULL_GG)
  
}


###Highlights the most important in the permutation
Minimizer_Highlighter_Perm <- function(x,uncertainty){


  colnames(x) <- c("times","x","sus","trans","sev",
                   "wn","wv1","wv2","vacrate",
                   "vacall",'inc',"R0",'country1','country2',
                   'mort')


  tmp<- split(x,list(x$vacrate,
                     x$R0, x$country1,x$country2,
                     x[,uncertainty]))

  tmp<-  tmp[sapply(  tmp, nrow)>0]

  cases_min<- lapply(tmp, function(x) ifelse((x$x== min(x$x))==TRUE,'Min','Not' ))
  mortality_min <- lapply(tmp, function(x) ifelse((x$mort== min(x$mort))==TRUE,'Min','Not' ))


  for(k in seq(1, length(tmp))){
    tmp[[k]]$min_cases <-   cases_min[[k]]
    tmp[[k]]$min_mort <- mortality_min[[k]]

  }
  df1 <- do.call(rbind, tmp)
  rownames(df1) <- NULL

  return(df1)

}


###########################
###Output theme for Figure#
###########################
output_theme <-
  theme(
    axis.ticks.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.position = "none",
    strip.background = element_blank(),
    panel.spacing = unit(1, "lines")
  )

