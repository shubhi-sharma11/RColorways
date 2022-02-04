library(ggplot2)

######## Functions ########

#' Convert HEX codes to RGB data frame
#'
#' @param hexvec a vector of HEX codes
#' @return a dataframe with columns "r", "g", "b"
#'


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

#' Convert RGB values to HSV values
#'
#' @param df a dataframe of RGB values with columns "r", "g", "b"
#' @return a dataframe of HSV values with columns "h", "s", "v"


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


#' Create a palette of colors based on user input
#'
#' @param user_cols a vector of R default colors or a vector of HEX
#' @param num_cols number of colors wanted in the output
#' @param full TRUE/FALSE, whether the full gradient should be plotted or not
#' @return a full gradient plot, your palette plot, vector of HEX codes
#'
#' @example mycols <-  createCols(c("violet", "darkred"), 6)[["hex"]]

createCols <- function(user_cols, num_cols, full = TRUE) {

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


  cen_hsv <- rgb2hsv(r = cen$red, g= cen$green, b = cen$blue)
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

  plt2 <- pltCols(hsv_df = cen_hsv) + labs(subtitle = "Your palette:")

  return(list(plot(plt), plot(plt2), "hex" = cen_hsv$hex))
}

#' Interpolate - stretch out a discrete palette
#'
#' @param user_cols Palette to be stretched - can be R defined colors or HEX codes
#' @param num_cols Number of colors in returned palette
#'
#' @return A vector of hex codes that represent that 'stretched' out discrete palette

stretchPal <- function(user_cols, num_cols){

  colfunc <- grDevices::colorRampPalette(user_cols)
  hex <- colfunc(num_cols)
  return(hex)
}

#' Plotting fuction
#'
#' @param hex HEX codes to plot
#' @param hsv_df HSV dataframe to plot - only one of HEX or HSV can be non-null
#'
#' @return plotted colors returned

pltCols <- function(hex = NULL, hsv_df = NULL){

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

    if(all(c("x","y","h", "s", "v")  %in% colnames(hsv_df) ) == FALSE){
      print("Essential columns missing from hsv data frame ")
    }

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


#' Random palette generator
#'
#' @param num_cols Number of colors in returned palette
#'
#' @return Random palette returned (and plotted)


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
#'

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

displayPals <- function(palette = NULL) {


  if(is.null(palette)) {

    # We want to plot all palettes stored in ALL

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


############ Color wheel plotting #########

r  <- seq(0,1,length=201)
th <- seq(0,2*pi, length=201)
d  <- expand.grid(r=r,th=th)
gg <- with(d,data.frame(d,x=r*sin(th),y=r*cos(th),
                        z=hcl(h=360*th/(2*pi),c=100*r, l=65)))

ggplot(gg) +
  geom_point(aes(x,y, color=z), size=3)+
  scale_color_identity()+labs(x="",y="") +
  coord_fixed()

# Create hsv grid
d = expand.grid(h=seq(0,1,0.01), s=seq(0,1,0.05), v=1)

ggplot() +
  #coord_polar(theta="x") +
  # scale_x_continuous(breaks=NULL) +
  #scale_y_continuous(breaks=NULL) +
  scale_fill_identity() +
  geom_rect(data=d, mapping=aes(xmin=h, xmax=h+resolution(h),
                                ymin=s, ymax=s+resolution(s),
                                fill=hsv(h,s,v)))+ theme_bw() +
  labs(x = "hue", y = "saturation") + ggtitle("hsv grid, v = 1")


###### Color wheels #######

all <- expand.grid(h=seq(0, 1, 0.01), s= seq(0, 1, 0.05), v = seq(0, 1, 0.2))

ggplot() +
  #coord_polar(theta="x") +
  #scale_x_continuous(breaks=NULL) +
  #scale_y_continuous(breaks=NULL) +
  scale_fill_identity() +
  geom_rect(data=all, mapping=aes(xmin=h, xmax=h+resolution(h),
                                  ymin=s, ymax=s+resolution(s),
                                  fill=hsv(h,s,v)))+ theme_bw() +
  facet_wrap(vars(v))




