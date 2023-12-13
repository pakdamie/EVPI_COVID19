


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

