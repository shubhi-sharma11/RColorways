#' Create a palette of colors based on user input 
#' 
#' @param user_cols a vector of R default colors or a vector of HEX 
#' @param num_cols number of colors wanted in the output 
#' @param full TRUE/FALSE, whether the full gradient should be plotted or not
#' @return a full gradient plot, your palette plot, vector of HEX codes 
#'
#' @example mycols <-  createCols(c("violet", "darkred"), 6)[["hex"]]
#' @export

createPal <- function(user_cols, num_cols, full = TRUE, plot = TRUE) {
  
  colfunc <- colorRampPalette(user_cols)
  hex <- colfunc(200)
  
  rgb_p <- grDevices::col2rgb(hex)
  rgb_p <- t(rgb_p)
  
  cc <- kmeans(rgb_p, num_cols)
  cen <- as.data.frame(round(cc$centers, 0))
  cen$hex <- apply(cen, 1, function(x){
    
    grDevices::rgb(red = x["red"], 
                   green = x["green"], 
                   blue = x["blue"], maxColorValue = 255)
  })
  
  
  cen_hsv <- grDevices::rgb2hsv(r = cen$red, g= cen$green, b = cen$blue)
  cen_hsv <- t(cen_hsv)
  cen_hsv <- data.frame(cen_hsv)
  
  
  cen_hsv <- cen_hsv[order( cen_hsv[, "h"], cen_hsv[, "v"]), ]
  cen_hsv$hex <- hsv(cen_hsv$h, cen_hsv$s, cen_hsv$v)
  
  cen_hsv$x = 1:nrow(cen_hsv)
  cen_hsv$y <- 1
  
  plt <- pltCols(hex = hex) + ggtitle("Full gradient:")
  
  plt2 <- pltCols(hex = cen_hsv$hex) + labs(subtitle = "Your palette:")
  
  if(plot & full) plot(plt); plot(plt2)
  if(plot & !full) plot(plt2)
  
  return(cen_hsv$hex)
}
