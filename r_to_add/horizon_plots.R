# approach 1 --------------------------------------------------------------
require(ggplot2)
require(magrittr)
require(dplyr)
require(reshape2)
require(xtsExtra)
require(visCOS)

runoff_data <- get_runoff_example() %>% 
  remove_chunk() %>% 
  mark_periods()

melted_runoff <- runoff_data %>% 
  select(starts_with("Q"),starts_with("posix"))   %>% 
  reshape2::melt(., id.vars = "posixdate")

melted_obs<- runoff_data %>% 
  select(starts_with("QOBS"),starts_with("posix"))   %>% 
  reshape2::melt(., id.vars = "posixdate")

horizonscale = 20
origin = 0 
horizon_ggplot <- function(df, title = "") {
  #df parameter should be in form of date (x), grouping, and a value (y)
  colnames(df) <- c("date","grouping","y")
  #get some decent colors from RColorBrewer
  #we will use colors on the edges so 2:4 for red and 7:9 for blue
  require(RColorBrewer)
  col.brew <- brewer.pal(name="RdBu",n=10)
  
  #get number of bands for the loop
  # limit to 3 so it will be much more manageable
  nbands = 3
  
  #loop through nbands to add a column for each of the positive and negative bands
  for (i in 1:nbands) {
    #do positive
    df[,paste("ypos",i,sep="")] <- ifelse(df$y > origin,
                                          ifelse(abs(df$y) > horizonscale * i,
                                                 horizonscale,
                                                 ifelse(abs(df$y) - (horizonscale * (i - 1) - origin) > origin, abs(df$y) - (horizonscale * (i - 1) - origin), origin)),
                                          origin)
    #do negative
    df[,paste("yneg",i,sep="")] <- ifelse(df$y < origin,
                                          ifelse(abs(df$y) > horizonscale * i,
                                                 horizonscale,
                                                 ifelse(abs(df$y) - (horizonscale * (i - 1) - origin) > origin, abs(df$y) - (horizonscale * (i - 1) - origin), origin)),
                                          origin)
  }
  #melt data frame now that we have added a column for each band
  #this will fit ggplot2 expectations and make it much easier
  df.melt <- melt(df[,c(1:2,4:9)],id.vars=1:2)    
  #name the columns for reference
  #try to be generic
  colnames(df.melt) <- c("date","grouping","band","value")
  
  #use ggplot to produce an area plot
  p <- ggplot(data=df.melt) +
    geom_area(aes(x = date, y = value, fill=band),
              #alpha=0.25,
              position="identity") +  #this means not stacked
    scale_fill_manual(values=c("ypos1"=col.brew[7],  #assign the colors to each of the bands; colors get darker as values increase
                               "ypos2"=col.brew[8],
                               "ypos3"=col.brew[9],
                               "yneg1"=col.brew[4],
                               "yneg2"=col.brew[3],
                               "yneg3"=col.brew[2])) +
    ylim(origin,horizonscale) + #limit plot to origin and horizonscale
    facet_grid(grouping ~ .) + #do new subplot for each group
    ggtitle(title)
  return(p)
}

horizon_ggplot(melted_obs, "test")
