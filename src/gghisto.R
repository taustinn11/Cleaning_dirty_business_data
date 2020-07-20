## gghisto function

## Plot a histogram for each variable in a data.frame regardless of class
### Note, this works best by saving the output (list) to an object name and calling that object
### Then, individual plots can be extracted and modified further if needed

gghisto <- function(df, na.rm = TRUE, ...) {
  
  varz <- names(df)
  collection <- list()
  
  for (i in seq_along(varz)) {
    
    x = varz[i]
    
    plot <- ggplot(df, aes_string(x = x)) +
      theme_classic() +
      ggtitle(paste("Distribution of values from variable", varz[i], sep = " "))+
      theme(axis.text.x = element_text(angle = 45, vjust = -0.5))+
      if (is.character(x) | is.logical(x)) {stat_count()}
    if (is.numeric(x)) {geom_histogram()}
    if (lubridate::is.Date(x)){geom_histogram()}
    else {stat_count()} 
    
    
    collection[[i]] <- plot
  }
  names(collection) <- varz
  return(collection)
}