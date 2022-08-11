#' Plotting fuction
#' 
#' @param hex HEX codes to plot 
#' @param hsv_df HSV dataframe to plot - only one of HEX or HSV can be non-null
#' 
#' @return plotted colors returned



pltCols <- function(hex = NULL, hsv_df = NULL, info = TRUE){
  
  library(ggplot2)
  
  
  if (!is.null(hex)) {
    # if (length(hex) > 15) {
    #   yrow <- ceiling(length(hex)/15)
    # } else {
    #   yrow <- 1
    # }
    # df <- data.frame (x = rep(1:15, yrow)[1:length(hex)], 
    #                  y = rep(1:yrow, each = 15)[1:length(hex)], val = hex)
    # df$y <- abs(df$y-yrow)
    
    
    df <- data.frame(x = 1:length(hex), y = rep(1, length(hex)), val = hex)
    plt <- ggplot(df, aes(x = x, y = y, 
                          fill = val)) + 
      geom_tile() +
      scale_fill_identity() +
      theme(axis.line = element_blank(), axis.title = element_blank(),
            axis.ticks = element_blank(), plot.margin = margin(0,0,0,0, "cm"),
            axis.text = element_blank(), panel.background = element_blank(),
            panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), legend.position = "none")
  }
  
  if (!is.null(hsv_df)) {
    
    if(all(c("h", "s", "v")  %in% colnames(hsv_df) ) == FALSE){
      print("Essential columns missing from hsv data frame ")
    }
    df <- data.frame(x = 1:nrow(hsv_df), y = rep(1, nrow(hsv_df)))
    hsv_df <- cbind(df, hsv_df)
    plt <- ggplot(data = hsv_df, aes(x = x, y = y, fill = hsv(h,s, v))) + 
      scale_fill_identity() +
      geom_tile() +
      theme(axis.line = element_blank(), axis.title = element_blank(),
            axis.ticks = element_blank(), plot.margin = margin(4,1,4,1, "cm"),
            axis.text = element_blank(), panel.background = element_blank(),
            panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), legend.position = "none")  
    
  }
  
  return(plt)
  
} 
