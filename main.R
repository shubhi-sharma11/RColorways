
######## Functions ########

#' Convert HEX codes to RGB data frame 
#' 
#' @param hexvec a vector of HEX codes 
#' @return a dataframe with columns "r", "g", "b"
#' @export
#' @importFrom grDevices col2rgb

col2rgb2 <- function(hexvec) {
  df <- data.frame(hex = hexvec,
                   r = rep(1, length(hexvec)), 
                   g = rep(1, length(hexvec)), 
                   b = rep(1, length(hexvec)))
  
  df[, c("r", "g", "b")] <-  apply(df, 1, function(x){
    r <-  unlist(grDevices::col2rgb(x["hex"]))
    return(as.numeric(r))
  })
  
  return(df)
}

#' Convert HEX values to HSV values 
#' 
#' @param HEXVEC hex code string vector 
#' @return a dataframe of HSV values with columns "hex", "h", "s", "v"
#' @export
#' @importFrom grDevices rgb2hsv
#' @importFrom grDevices col2rgb


col2hsv <- function(HEXVEC) {
  
  RGB <- grDevices::col2rgb(HEXVEC)
  x <- apply(RGB, 2, grDevices::rgb2hsv)
  x2 <- t(x)
  colnames(x2) <- c('h', 's', 'v')
  
  return(x2)
  
}

#' Convert RGB values to HSV values 
#' 
#' @param df a dataframe of RGB values with columns "r", "g", "b"
#' @return a dataframe of HSV values with columns "h", "s", "v"
#' @export
#' @importFrom grDevices rgb2hsv


rgb2hsv2 <- function(df) {
  df2 <- data.frame(h = rep(1, nrow(df)), 
                    s = rep(1, nrow(df)), 
                    v = rep(1, nrow(df)))
  
  df2[, c("h", "s", "v")] <-  apply(df, 1, function(x){
    r <-  unlist(grDevices::rgb2hsv(x["r"], x["g"], x["b"]))
    return(as.numeric(r))
  })
  
  return(df2)
}


#' Reverses a palette 
#' 
#' @param hexvec hex code string vector 
#' @return hex code vector but like, reversed
#' @export

reversePal <- function(hexvec) {
  n <- length(hexvec)
  reverse <- hexvec[n:1]
  return(reverse)
}

#' Colorblindness check 
#' 
#' @param pal color palette to take
#' @return returns a plot of different color blindness check 
#' @export 
#' @importFrom colorBlindness cvdSimulator
#' @import ggplot2

cbCheck <- function(pal) {
  
  if(!("colorBlindness" %in% installed.packages())) install.packages("colorBlindness")
  
  deu <- colorBlindness::cvdSimulator(pal, type = "deuteranope")
  pro <- colorBlindness::cvdSimulator(pal, type = "protanope")
  
  plt1 <- pltCols(pal, info = F) + ggtitle("Normal vision")
  plt2 <- pltCols(deu, info = F) + ggtitle("Deuteranope")
  plt3 <- pltCols(pro, info = F) + ggtitle("Protanope")
  
  gridExtra::grid.arrange(plt1, plt2, plt3)
  
}

#' Create a palette of colors based on user input 
#' 
#' @param user_cols a vector of R default colors or a vector of HEX 
#' @param num_cols number of colors wanted in the output 
#' @param full TRUE/FALSE, whether the full gradient should be plotted or not
#' @return a full gradient plot, your palette plot, vector of HEX codes 
#' 
#' @example mycols <-  createCols(c("violet", "darkred"), 6)[["hex"]]
#' @export
#' @improtFrom grDevices col2rgb
#' @importFrom grDevices colorRampPalette
#' @importFrom grDevices rgb
#' @import ggplot2

createCols <- function(user_cols, num_cols, full = TRUE, plot = TRUE) {
  
  colfunc <- grDevices::colorRampPalette(user_cols)
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
  #cen_hex <- rgb(r = cen$red, g = cen$green, b = cen$blue, maxColorValue = 255)
  cen_hsv <- t(cen_hsv)
  cen_hsv <- data.frame(cen_hsv)
  
  
  cen_hsv <- cen_hsv[order( cen_hsv[, "h"], cen_hsv[, "v"]), ]
  cen_hsv$hex <- hsv(cen_hsv$h, cen_hsv$s, cen_hsv$v)
  
  # Extra ordering, keeping it commented out for the moment
  
  # sv <- seq(min(cen_hsv$h), max(cen_hsv$h), by= 0.1)
  # cen_hsv$group <- findInterval(cen_hsv$h, sv)
  # 
  # cen_hsv$group <- as.factor(cen_hsv$group)
  # cen_hsv <- cen_hsv[order(cen_hsv$group, cen_hsv$s), ]
  # 
  # cc <- ceiling(nrow(cen_hsv)/2) + 1
  # half <-   cen_hsv[cc:nrow(cen_hsv), ] 
  # half <- half[order(half$group, -half$s), ]
  # cen_hsv[cc:nrow(cen_hsv), ]  <- half
  cen_hsv$x = 1:nrow(cen_hsv)
  cen_hsv$y <- 1
  
  plt <- pltCols(hex = hex) + ggtitle("Full gradient:")
  
  plt2 <- pltCols(hex = cen_hsv$hex) + labs(subtitle = "Your palette:")
  
  if(plot & full) plot(plt); plot(plt2)
  if(plot & !full) plot(plt2)
  
  return(cen_hsv$hex)
}

#' Interpolate - stretch out a discrete palette
#' 
#' @param user_cols Palette to be stretched - can be R defined colors or HEX codes
#' @param num_cols Number of colors in returned palette 
#' 
#' @return A vector of hex codes that represent that 'stretched' out discrete palette
#' @export
#' @importFrom grDevices colorRampPalette

stretchPal <- function(user_cols, num_cols, interpolation = "linear"){
  
  colfunc <- grDevices::colorRampPalette(user_cols, interpolate= interpolation)
  hex <- colfunc(num_cols)
  return(hex)
}

#' Plotting fuction
#' 
#' @param hex HEX codes to plot 
#' @param hsv_df HSV dataframe to plot - only one of HEX or HSV can be non-null
#' 
#' @return plotted colors returned
#' @export
#' @import ggplot2


pltCols <- function(hex = NULL, hsv_df = NULL, info = TRUE){
  library(ggplot2)
  
  if (!is.null(hex)) {
    
    df <- data.frame(x = 1:length(hex), y = rep(1, length(hex)), val = hex)
  } 
  
  if (!is.null(hsv_df)) {
    
    hex <- hsv(h = hsv_df[, "h"], s = hsv_df[, "s"], v = hsv_df[, "v"])
    df <- data.frame(x = 1:nrow(hsv_df), y = rep(1, nrow(hsv_df)), val = hex)
  }
  
  cc <- clashCheck(hex)
  ccW <- clashCheckWhite(hex)
  
  if(min(cc) < 20) {
    
    pair <- which(cc < 20)
    dec <- paste0("Poor - Consider changing colors ", pair, " and ", pair + 1)
  } else {
    
    if(min(cc) > 45) {

      dec <- "Good"
    } else {
      
      dec <- "Very Good"
    }
  }
  
  cc_sub <- paste0("\n Palette contrast check : ", dec)
  
  ccW <- clashCheckWhite(hex)
  
  if(min(ccW) < 20) {
    
    pairW <- which(ccW < 20)
    decW <- paste0("Poor - Consider changing color ", pairW)
  } else {
      decW <- "Good"
  }
  
  ccW_sub <- paste0("\n Palette contrast with white check : ", decW)
  
  if(info) {
    sub <- paste0("Your palette: ", cc_sub, ccW_sub)
  } else {
    sub <- ""
  }
  
    plt <- ggplot(df, aes(x = x, y = y, 
                          fill = val)) + 
      geom_tile() +
      scale_fill_identity() +
      theme(axis.line = element_blank(), axis.title = element_blank(),
            axis.ticks = element_blank(), plot.margin = margin(0,0,0,0, "cm"),
            axis.text = element_blank(), panel.background = element_blank(),
            panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), legend.position = "none") + 
      ggtitle(label = "", subtitle = sub )
      
  
  return(plt)
  
} 


#' Random palette generator 
#' 
#' @param num_cols Number of colors in returned palette 
#' 
#' @return Random palette returned (and plotted)
#' @export 


ranColorGenerator <- function(num_cols){
  
  ran <- rg$hex[sample(1:nrow(rg), num_cols)]
  plt <- pltCols(hex = ran)
  return(list("plot" = plot(plt), "hex" = ran))
}


#' Pick a shade 
#' 
#' If you want to replace a color in your color way with another, shadePicker
#' helps you find a new shade 
#' 
#' @param hex1 the HEX code of the color to be replaced 
#' @return A list of HEX codes representing color replacements and a plot
#' @export 
#' @importFrom grDevices col2rgb
#' @importFrom grDevices rgb2hsv


shadePicker <- function(hex1){
  
  rgbVal <- grDevices::col2rgb(hex1)
  hsvVal <- grDevices::rgb2hsv(rgbVal[1:3, 1])
  
  hsvGrid <- expand.grid(h = hsvVal["h",1], s = hsvVal["s",1], v = seq(0, 1, by = 0.2))
  hsvGrid <- rbind(hsvGrid, t(hsvVal))
  hsvGrid <- hsvGrid[order(hsvGrid["v"]), ]
  
  hexPlt <- apply(hsvGrid, 1, function(x){hsv(x["h"], x["s"], x["v"])})
  
  lab <- paste0(1:length(hexPlt), ". ", hexPlt)
  
  if(toupper(hex1) == hex1) {
    original <- as.numeric(which((hexPlt) == hex1))
  } else { original <- as.numeric(which(tolower(hexPlt) == hex1))
  }
  
  lab[original] <- paste0(lab[original], "**") 
  
  
  plt <- pltCols(unique(hexPlt))  + geom_label(label = lab, col= "white", size = 3) + labs("caption" = "**ORIGINAL")
  
  return(list("plot" = plot(plt) ,"hexCodes" =hexPlt))
  
}

#' Palette Visualization 
#' 
#' @param palette Name of a palette to view. Default is NULL which will display all the built-in palettes
#' 
#'  @return Plots  
#'  @export 
#'  @import ggplot2

displayPals <- function(palette = NULL) {
  
  
  if(is.null(palette)) {
    
    # We want to plot all palettes stored in ALL 
    load("data/ALL.Rdata") #loads object ALL
    
    # One "page" per palette theme 
    plt <- list()

    for(i in 1:length(ALL)) { 
      
      df <- do.call(cbind, ALL[[i]])
      df2 <- reshape2::melt(df)
      
      colnames(df2) <- c("x", "y", "val")
      
      plt[[i]] <-  ggplot(df2, aes(x = x, y = as.factor(y), fill = val)) + 
        geom_tile(height = 0.9) + 
        scale_fill_identity() + 
        theme(axis.line = element_blank(), axis.title = element_blank(),
              axis.ticks = element_blank(), plot.margin = margin(0,0,0,0, "cm"),
              axis.text.x = element_blank(), panel.background = element_blank(),
              panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), legend.position = "none") + 
        ggtitle(names(ALL[i]))
    }
    
    lapply(plt, plot)
    
  }
  
  if(!is.null(palette)) {
    
    foc <- which(palette == names(ALL))
    
    if(length(foc) == 0){
      print("Please select one of the following palettes: ")
      print(names(ALL))
      
    } else {
      df <- do.call(cbind, ALL[[foc]])
      df2 <- reshape2::melt(df)
      
      colnames(df2) <- c("x", "y", "val")
      
      plt <-  ggplot(df2, aes(x = x, y = as.factor(y), fill = val)) + 
        geom_tile(height = 0.9) + 
        scale_fill_identity() + 
        theme(axis.line = element_blank(), axis.title = element_blank(),
              axis.ticks = element_blank(), plot.margin = margin(0,0,0,0, "cm"),
              axis.text.x = element_blank(), panel.background = element_blank(),
              panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), legend.position = "none") + 
        ggtitle(names(ALL[foc]))
      plot(plt)
    }
  } 
  
  
}

#' Clash check 
#' 
#' @param palette Name of a palette to check
#' 
#'  @return A distance between palette components
#'  @export 
#'  @importFrom grDevices col2rgb


clashCheck <- function(pal) {
  
  x <- t(grDevices::col2rgb(pal))
  dist <- matrix(0, nrow = nrow(x)-1, ncol = 1)
  # I think 35 could be a threshold 
  for(i in 1:nrow(x)-1) dist[i] <-  sqrt((x[i, "red"] - x[i+1, "red"])^2 + 
                                           (x[i, "green"] - x[i+1, "green"])^2 +
                                           (x[i, "blue"] - x[i+1, "blue"])^2)
  
  return(dist)
  
}

#' Clash check with white
#' 
#' @param palette Name of a palette to check
#' 
#'  @return A distance between palette components
#'  @export 

clashCheckWhite <- function(pal) {
  
  dist <- matrix(0, nrow = length(pal), ncol = 1)
 for(i in 1:length(pal)) dist[i] <- clashCheck(c("#FFFFFF", pal[i])) 
 
 return(dist)
}

#' Relative Luminance ratio
#' 
#' @param palette Name of a palette to check
#' 
#'  @return A ratio
#'  @export 


relLumRatio <- function(pal) {
  
  x <- t(col2rgb(pal))
  
  rl <- apply(x, 1, function(y) 0.2126 * y["red"] + 0.7152 * y["green"] + 0.0722 * y["blue"])
  ratio <- matrix(0, nrow = nrow(x)-1, ncol = 1)
  
  for (i in 1:nrow(x)-1) ratio[i] <- (rl[i] + 0.05)/rl[i + 1] + 0.05
  
  return(ratio)
}



#' Visualize palette  structure
#' 
#' @param palette Name of a palette to view. 
#' 
#'  @return Plots  
#'  @export
#'  @import ggplot2


structurePal <- function(pal) {
  df <- data.frame(col2hsv(pal))
  df$Index <- 1:nrow(df)
  hplot <- ggplot(df) +
    geom_line(aes(x = Index, y = h)) +
    geom_point(aes(x = Index, y = h, fill = hsv(h, s, v)), 
               pch = 21, size = 5) + 
    scale_fill_identity() +
    theme_bw()
  
  splot <- ggplot(df) +
    geom_line(aes(x = Index, y = s)) +
    geom_point(aes(x = Index, y = s, fill = hsv(h, s, v)), 
               pch = 21, size = 5) + 
    scale_fill_identity() +
    theme_bw()
  
  vplot <- ggplot(df) +
    geom_line(aes(x = Index, y = v)) +
    geom_point(aes(x = Index, y = v, fill = hsv(h, s, v)), 
               pch = 21, size = 5) + 
    scale_fill_identity() +
    theme_bw()
  
  return(gridExtra::grid.arrange(hplot, splot, vplot))
}

#' get Hue
#' 
#' @param color optional - user defined color. This can be NULL, a HEX code or a R-defined color
#' 
#'  @return a valid hue 
#'  @export


getHue <- function(color = NULL) {
  
  if(class(color) == "character") {
    
    # Characters can either be HEX codes or R colors 
    if(grepl("#", color)) {
      hue <- col2hsv(color)[, "h"]
      sat <- col2hsv(color)[, "s"]
      val <- col2hsv(color)[, "v"]
    } else {
      s <- which(rg$colors == color)
      if(length(s) != 1) stop("Please specify a valid color")
      h <- rg$hex[s]
      hue <- col2hsv(h)[, "h"]
      sat <- col2hsv(color)[, "s"]
      val <- col2hsv(color)[, "v"]
    }
  }
  
  if(is.null(color)) {
    hue <- runif(1, 0, 1)
    sat <- NULL
    val <- NULL
  }
  
  if(!(0 <= hue  & hue <= 1 )) stop("Please specify a valid color")
  
  return(list(hue = hue, sat = sat, val = val))
}


#' Checks to ensure hsv code relates to valid color
#' 
#' @param hsvdf a data frame or matrix with columns h, s, v
#' 
#'  @return a df or matrix that specifies a valid color
#'  @export

posNegCheck <- function(hsvdf) {
  
  # H 
  pos <- which(hsvdf[, "h"] > 1)
  if(length(pos > 1)) hsvdf[pos, "h"] <- hsvdf[pos, "h"] - 1
  
  neg <- which(hsvdf[, "h"] < 0)
  if(length(neg > 1)) hsvdf[neg, "h"] <- hsvdf[neg, "h"] + 1
  
  # S
  
  pos <- which(hsvdf[, "s"] > 1)
  if(length(pos > 1)) hsvdf[pos, "s"] <- 1
  
  neg <- which(hsvdf[, "s"] < 0)
  if(length(neg > 1)) hsvdf[neg, "s"] <-0
  
  # V
  
  pos <- which(hsvdf[, "v"] > 1)
  if(length(pos > 1)) hsvdf[pos, "v"] <- 1
  
  neg <- which(hsvdf[, "v"] < 0)
  if(length(neg > 1)) hsvdf[neg, "v"] <-0
  
  return(hsvdf)
}

#' Create a monochrome color palette
#' 
#' @param num_cols optional - is the user defined number of colors in the desired palette
#' @param color optional - user defined color. This can be NULL, a HEX code or a R-defined color
#' @param noise TRUE/FALSE add some noise?
#' @param plot TRUE/FALSE whether to display the palette or not 
#' @param interpolation which method to use to interpolate. This can be either a spline or linear
#' 
#'  @return HEX codes of desired palette and (optional) a plot
#'  @export


createMonochrome <- function( num_cols = 5, color = NULL, 
                              plot = TRUE, interpolation = "spline", noise = F, 
                              descending = F) {
  
  hue <- getHue(color)$hue
  
  load("data/templates.Rdata") #loads object templates
  monochrome_schemes <- grep("monochrome", names(templates))
  choose <- sample( 1:length(monochrome_schemes), 1)
  template <- templates[[choose]]
  
  if(descending) template <- templates$monochrome


  
  if(noise){
  template[, "s"] <- template[, "s"] + rnorm(nrow(template), 0, 0.1)
  template[, "v"] <- template[, "v"] + rnorm(nrow(template), 0, 0.1)
  }
  
  template[, "h"] <- seq(hue, hue+0.04, length.out = nrow(template)) 
  
  pos <- which(template[, "h"] > 1)
  if(length(pos > 1)) template[pos, "h"] <- template[pos, "h"] - 1
  
  template <- posNegCheck(template)
  newpal <- hsv(h = template[, 'h'], 
                s = template[, 's'], 
                v = template[, 'v'])
  
  numpal <- stretchPal(newpal, num_cols, interpolation = interpolation)
  plt <- pltCols(numpal)
  if(plot) plot(plt)
  return(numpal)
}


#' Create a sequential/analogous color palette
#' 
#' @param num_cols optional - is the user defined number of colors in the desired palette
#' @param color optional - user defined color. This can be NULL, a HEX code or a R-defined color
#' @param plot TRUE/FALSE whether to display the palette or not 
#' @param interpolation which method to use to interpolate. This can be either a spline or linear
#' 
#'  @return HEX codes of desired palette and (optional) a plot
#'  @export



createSequential <- function(num_cols = 5, color = NULL, plot = TRUE, interpolation = "spline") {
  # TODO: Give options for the type of color scheme i.e. light/dark/pastel etc. 
  load("data/templates.Rdata") #loads object templates
  hue <- getHue(color)$hue
  
  # Randomly choose a template between 2 and 6
  i <- round(runif(1, 2, 6), digits = 0)
  template <- templates[[i]]
  
  ran <- sample(1:2, 1)
  if(ran == 1) {
    template[, "h"] <- seq(hue - 0.2, hue + 0.2, length.out = nrow(template))
  } else {
    template[, "h"] <- seq(hue + 0.3, hue - 0.2, length.out = nrow(template))
  }
  
  neg <- which(template[, "h"] < 0)
  if(length(neg > 1)) template[neg, "h"] <- template[neg, "h"] + 1
  
  pos <- which(template[, "h"] > 1)
  if(length(pos > 1)) template[pos, "h"] <- template[pos, "h"] -1
  newpal <- hsv(h = template[, 'h'], 
                s = template[, 's'], 
                v = template[, 'v'])
  # createCols(user_cols = newpal, num_cols = 9)
  numpal <- stretchPal(newpal, num_cols, interpolation = interpolation)
  plt <- pltCols(numpal)
  if(plot) plot(plt)
  return(numpal)
}


#' Create a divergent color palette
#' 
#' @param num_cols optional - is the user defined number of colors in the desired palette
#' @param color optional - user defined color. This can be NULL, a HEX code or a R-defined color
#' @param plot TRUE/FALSE whether to display the palette or not 
#' @param interpolation which method to use to interpolate. This can be either a spline or linear
#' 
#'  @return HEX codes of desired palette and (optional) a plot
#'  @export


createDivergent <- function(num_cols = 9, color = NULL, plot = TRUE, interpolation = "spline") {

hue <- getHue(color)$hue

# Create a 9 len pal 

bone <- "#ffefc8"
neu <- "#e7dec6"
# neu2 <- "#fefae0"
gtint <- "#FFFCEB"

neutrals <- c(bone, neu, gtint)
left <- createMonochrome(color, num_cols = 4, plot = FALSE, noise = F, descending = T)
mid_raw <- left[4]
mhsv <- col2hsv(mid_raw)
mhsv[, "s"] <- 0.2; mhsv[, "v"] <- 0.999
mid <- hsv(h = mhsv[, "h"], s = mhsv[, "s"], v = mhsv[, "v"])

hue2 <- runif(1, min = hue +0.1, max = hue + 0.8)
if(hue2 > 1) hue2 <- hue2 -1

rhsv <- hsv(h = hue2, s = 0.8, v = 0.8 )
right <- createMonochrome(color = rhsv, num_cols = 4, plot = FALSE, noise = F, descending = T)
rf <- reversePal(right)

pick <- neutrals[sample(1:length(neutrals), 1)]
stitch <- c(left, pick, rf)

stitch2 <- stretchPal(stitch, num_cols = num_cols)
plt <- pltCols(stitch2)

if(plot) plot(plt)
return(stitch2)

}

#' Create a complemenatary color palette
#' 
#' @param num_cols optional - is the user defined number of colors in the desired palette
#' @param color optional - user defined color. This can be NULL, a HEX code or a R-defined color
#' @param plot TRUE/FALSE whether to display the palette or not 
#' @param interpolation which method to use to interpolate. This can be either a spline or linear
#' 
#'  @return HEX codes of desired palette and (optional) a plot
#'  @export


createComplementary <- function(num_cols = 5, color = NULL, plot = TRUE, interpolation = "spline") {
  
  hue1 <- getHue(color)$hue
  
  left <- createMonochrome(color, num_cols = 3, plot = FALSE, noise = F, descending = T)
  hue2 <- runif(1, min = hue1 +0.5, max = hue1 + 0.55)
  
  if(hue2 > 1) hue2 <- hue2 - 1
  hue2_seq <- seq(hue2, hue2 + 0.1, length.out = 2)
  
  template <- templates$complementary
  
  template[, "h"] <- c(left, hue2_seq)
  
  neg <- which(template[, "h"] < 0)
  if(length(neg > 1)) template[neg, "h"] <- template[neg, "h"] + 1
  
  pos <- which(template[, "h"] > 1)
  if(length(pos > 1)) template[pos, "h"] <- template[pos, "h"] -1
  
  newpal <- hsv(h = template[, 'h'], 
                s = template[, 's'], 
                v = template[, 'v'])
  
  numpal <- stretchPal(newpal, num_cols, interpolation = interpolation)
  plt <- pltCols(numpal)
  if(plot) plot(plt)
  return(numpal)
}


#' Create a triad color palette
#' 
#' @param num_cols optional - is the user defined number of colors in the desired palette
#' @param color optional - user defined color. This can be NULL, a HEX code or a R-defined color
#' @param plot TRUE/FALSE whether to display the palette or not 
#' @param interpolation which method to use to interpolate. This can be either a spline or linear
#' 
#'  @return HEX codes of desired palette and (optional) a plot
#'  @export


createTriad <- function(num_cols = 5, color = NULL, plot = TRUE, interpolation = "spline") {
  
  hue1 <- getHue(color)$hue
  
  # if(!is.null(getHue(color)$sat) & !is.null(getHue(color)$val)) 
  
  hue2 <- hue1 + 0.4
  hue3 <- hue1 + 0.6
  
  if(hue2 > 1) hue2 <- hue2 - 1
  if(hue3 > 1) hue3 <- hue3 -1
  
  sat <- runif(3, 0, 1)
  val <- runif(3, 0, 1)
  
  template <- data.frame(h = c(hue1, hue2, hue3), 
                         s = sat, 
                         v = val)
  
  neg <- which(template[, "h"] < 0)
  if(length(neg > 1)) template[neg, "h"] <- template[neg, "h"] + 1
  
  pos <- which(template[, "h"] > 1)
  if(length(pos > 1)) template[pos, "h"] <- template[pos, "h"] -1
  
  newpal <- hsv(h = template[, 'h'], 
                s = template[, 's'], 
                v = template[, 'v'])
  
  numpal <- stretchPal(newpal, num_cols, interpolation = interpolation)
  plt <- pltCols(numpal)
  if(plot) plot(plt)
  return(numpal)
}


#' Create a tetrad color palette
#' 
#' @param num_cols optional - is the user defined number of colors in the desired palette
#' @param color optional - user defined color. This can be NULL, a HEX code or a R-defined color
#' @param plot TRUE/FALSE whether to display the palette or not 
#' @param interpolation which method to use to interpolate. This can be either a spline or linear
#' 
#'  @return HEX codes of desired palette and (optional) a plot
#'  @export


createTetrad <- function(num_cols = 5, color = NULL, plot = TRUE, interpolation = "spline") {
  
  hue1 <- getHue(color)$hue
  
  hue1 <- runif(1, 0, 1)
  hue2 <- hue1 +0.1
  
  hue3 <- hue1 + 0.5
  hue4 <- hue2 + 0.5
  
  if(hue2 > 1) hue2 <- hue2 - 1
  if(hue3 > 1) hue3 <- hue3 -1
  if(hue4 > 1) hue4 <- hue4 -1
  
  sat <- runif(4, 0, 1)
  val <- runif(4, 0, 1)
  
  template <- data.frame(h = c(hue1, hue2, hue3, hue4), 
                         s = sat, 
                         v = val)
  
  neg <- which(template[, "h"] < 0)
  if(length(neg > 1)) template[neg, "h"] <- template[neg, "h"] + 1
  
  pos <- which(template[, "h"] > 1)
  if(length(pos > 1)) template[pos, "h"] <- template[pos, "h"] -1
  
  newpal <- hsv(h = template[, 'h'], 
                s = template[, 's'], 
                v = template[, 'v'])
  
  numpal <- stretchPal(newpal, num_cols, interpolation = interpolation)
  plt <- pltCols(numpal)
  if(plot) plot(plt)
  return(numpal)
}

#' Fill continuous data on a ggplot
#' 
#' @param palette Palette name from default palettes or your created palette stored as a hex code vector
#' @param na.value value for nas in data, default is middle grey
#' @param limits numeric vector of length 2 providing limits of scale. NULL defaults to scale range 
#' @param reverse TRUE/FALSE on whether to reverse the color palette or not 
#' @param user_cols user specified colors for the palette
#' 
#'  @return Function to be used for filling ggplot continuous data with chosen palette
#'  @import ggplot2
#'  @export

scale_fill_colorway_c <- function(palette, na.value = "grey50", limits = NULL, 
                                  reverse = FALSE, user_cols = NULL) {
  

  if(is.null(palette) & !is.null(user_cols) ) {
    
   palette <-  createCols(user_cols = user_cols, num_cols = 10, 
                          plot = FALSE, full = FALSE)
  }
  
  if(reverse) palette <- reversePal(palette)
  
  # Reverse doesn't work right now 
  # low <- palette[1]
  # high <- palette[length(palette)]
  
  scale_fill_gradientn(colors = palette, na.value = na.value, 
                       limits = limits)
}


#' Color continuous data on a ggplot
#' 
#' @param palette Palette name from default palettes or your created palette stored as a hex code vector
#' @param na.value value for nas in data, default is middle grey
#' @param limits numeric vector of length 2 providing limits of scale. NULL defaults to scale range 
#' @param reverse TRUE/FALSE on whether to reverse the color palette or not 
#' @param user_cols user specified colors for the palette
#' 
#'  @return Function to be used for filling ggplot continuous data with chosen palette
#'  @import ggplot2
#'  @export

scale_color_colorway_c <- function(palette, na.value = "grey50", limits = NULL, 
                                  reverse = FALSE, user_cols = NULL) {
  
  
  if(is.null(palette) & !is.null(user_cols) ) {
    
    palette <-  createCols(user_cols = user_cols, num_cols = 10, 
                           plot = FALSE, full = FALSE)
  }
  
  if(reverse) palette <- reversePal(palette)
  
  # Reverse doesn't work right now 
  # low <- palette[1]
  # high <- palette[length(palette)]
  
  scale_color_gradientn(colors = palette, na.value = na.value, 
                       limits = limits)
}



#' Color discrete data on a ggplot
#' 
#' @param palette Palette name from default palettes or your created palette stored as a hex code vector
#' @param na.value value for nas in data, default is middle grey
#' @param limits numeric vector of length 2 providing limits of scale. NULL defaults to scale range 
#' @param reverse TRUE/FALSE on whether to reverse the color palette or not 
#' @param user_cols user specified colors for the palette
#' @param num_cols should correspond to number of categories 
#' 
#'  @return Function to be used for filling ggplot continuous data with chosen palette
#'  @export
#'  @import ggplot2

scale_color_colorway_d <- function(palette, na.value = "grey50", limits = NULL, 
                                   reverse = FALSE, user_cols = NULL, num_cols = 5) {
  
  
  if(is.null(palette) & !is.null(user_cols) ) {
    
    palette <-  createCols(user_cols = user_cols, num_cols = num_cols, 
                           plot = FALSE, full = FALSE)
  } else {
    
    palette <- stretchPal(palette, num_cols)
  }
  
  if(reverse) palette <- reversePal(palette)
  
  scale_color_manual(values = palette, na.value = na.value, 
                        limits = limits)
}


#' Fill discrete data on a ggplot
#' 
#' @param palette Palette name from default palettes or your created palette stored as a hex code vector
#' @param na.value value for nas in data, default is middle grey
#' @param limits numeric vector of length 2 providing limits of scale. NULL defaults to scale range 
#' @param reverse TRUE/FALSE on whether to reverse the color palette or not 
#' @param user_cols user specified colors for the palette
#' @param num_cols should correspond to number of categories 
#' 
#'  @return Function to be used for filling ggplot continuous data with chosen palette
#'  @export
#'  @import ggplot2

scale_fill_colorway_d <- function(palette, na.value = "grey50", limits = NULL, 
                                   reverse = FALSE, user_cols = NULL, num_cols = 5) {
  
  
  if(is.null(palette) & !is.null(user_cols) ) {
    
    palette <-  createCols(user_cols = user_cols, num_cols = num_cols, 
                           plot = FALSE, full = FALSE)
  } else {
    
    palette <- stretchPal(palette, num_cols)
  }
  
  if(reverse) palette <- reversePal(palette)
  
  scale_fill_manual(values = palette, na.value = na.value, 
                     limits = limits)
}

#' Increase value of a palette
#' 
#' @param pal String of hex codes
#' @param plot TRUE/FALSE display plot of new palette
#' 
#'  @return Lightened color palette 
#'  @export


lighten <- function(pal, plot = T) {
  
 hsv_df <-  col2hsv(pal)
 hsv_df[, "v"] <- hsv_df[, "v"] + 0.3
 
 hsv_fin <- posNegCheck(hsv_df)
 
 pal_fin <- hsv(h = hsv_fin[, "h"], 
                s = hsv_fin[, "s"], 
                v = hsv_fin[, "v"])
 
 plt <- pltCols(pal_fin)
 
 if(plot) plot(plt)
 return(pal_fin)
}

