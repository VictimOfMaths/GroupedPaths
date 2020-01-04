groupedpath <- function(data, group, time, outcome, fill, xlabel, ylabel, title, caption){
  #data = the data frame with the original data in to graph
  #group = the grouping variable (e.g. age group) in data, must be an ordered factor in quote marks
  ########################################################
  #NB the data frame needs to be ordered on this variable
  #will try and remove this requirement
  ########################################################
  #time = the time variable in data, must be continuous in quote marks
  #outcome = the y-axis variable, must be continuous in quote marks
  #fill = the colour to fill the polygons
  #xlabel = the x-axis label for the graph (in quote marks)
  #ylabel = the y-axis label for the graph (in quote marks)
  #title = the plot title, use empty quotes if not required
  #caption = the plot caption, use empty quotes if not required

  require(ggplot2, cowplot)
  
  #size data to scale polygons
  group_names <- unique(data[[group]])
  times <- unique(data[[time]])
  n_groups <- length(group_names)
  n_time <- length(times)
  
  #sort the data and add index variable for plotting
  data <- data[order(data[[group]], data[[time]]),]
  data$index <- c(1:(n_groups*n_time))
  
  #set up y-axis values for each age group
  for (i in 1:n_groups) {
    name <- paste0("x",i)
    assign(name, 0)
    
    for (j in 1:n_time) {
      assign(name, rbind(eval(as.symbol(name)), data[[outcome]][data[[group]]==group_names[i] & data[[time]]==times[j]]))
      
    }
    assign(name, rbind(eval(as.symbol(name)), 0))
  }
  
  #generate plot - start with bottom group
  plot <- ggplot()+
    geom_polygon(aes(x=c(1,1:n_time,n_time), y=x1), fill=fill)
    
  #loop over remaining groups, adding them to the graph
  for (i in 2:n_groups) {
    #have to specify it this way as defining the data in the aes call only plots the last group
    loopdata <- data.frame(x=c(1+n_time*(i-1), (1+n_time*(i-1)):(n_time*i), n_time*i), y=eval(as.symbol(paste0("x",i))))
    
    plot <- plot+
      geom_polygon(data=loopdata, aes(x,y), fill=fill)
  }
  
  #add path to highlight trends and tidy up graph
  plot <- plot+
    geom_path(data=data,aes(x=index, y=data[[outcome]], group=data[[group]]), arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
    theme_classic()+
    scale_x_continuous(name=xlabel, breaks=seq(n_time/2, n_groups*n_time-n_time/2, by=n_time), labels=group_names)+
    #rescale y-axis to ensure that inset key fits
    scale_y_continuous(name=ylabel, limits=c(0,max(data[[outcome]]*1.4)))+
    labs(title=title, caption=caption)
  
  #generate inset graph
  insetvalues <- runif(n_time, 0.5,1)
  insetvalues2 <- c(0,insetvalues,0)
  
  inset <- ggplot()+
    geom_polygon(aes(x=c(1,1:n_time,n_time), y=insetvalues2), fill=fill)+
    geom_line(aes(x=c(1:n_time), y=insetvalues), arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
    theme_classic()+
    #Remove all clutter
    theme(axis.line=element_blank(), axis.text=element_blank(),axis.ticks=element_blank(),
          axis.title=element_blank())
  
  
  finalplot <- ggdraw()+
    draw_plot(plot)+
    draw_plot(inset, x=0.85, y=0.75, width=0.1, height=0.2)+
    draw_label(min(times), x=0.87, y=0.76, size=10)+
    draw_label(max(times), x=0.94, y=0.76, size=10)+
    draw_label("Key", x=0.88, y=0.95, size=10)
  
  return(finalplot)
}


