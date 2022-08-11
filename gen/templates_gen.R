


########## Prelim Data ##########

rg <- data.frame(colors = colors())
rg$rgb <- apply(rg, 1, function(x){
  rgb<- paste0(grDevices::col2rgb(x["colors"]), collapse = ",")
})

rg$r <- apply(rg, 1, function(x){
  r<- grDevices::col2rgb(x["colors"])[1]
  return(r)
}) 

rg$g <- apply(rg, 1, function(x){
  g <- grDevices::col2rgb(x["colors"])[2]
  return(g)
}) 

rg$b <- apply(rg, 1, function(x){
  b <- grDevices::col2rgb(x["colors"])[3]
  return(b)
}) 

rg$hex <- apply(rg, 1, function(x){
  vals <- strsplit(x["rgb"], ",")
  vals <- as.numeric(unlist(vals))
  hex <- rgb(red = vals[1], green = vals[2], blue = vals[3], maxColorValue = 255)
  return(hex)
})


samples <- list(monochrome = c("#011523","#012948","#014270","#1A5C85","#347494","#4A91B0","#6EAFCB","#94C8DC","#B9DDE8"),
                sequential = c("#E7889B", "#D1798A", "#BA6979", "#A45968", "#8D4957", "#773A46", '#602A35', "#4A1A24", '#330A12'),
                sequential2 = c("#E7889B", "#D1798A", "#BA6979", "#A45968", "#8D4957", "#773A46", '#602A35', "#4A1A24", '#330A12'),
                divergent = c("#9D1B7C", "#BB548A", "#D98D97","#E0B6AF", "#E0B6AF", "#9CA334", "#749025", "#4B7D15", "#296614"),
                complementary = c("#023047", "#219EBC", "#ABD8ED", "#FFB703", "#F58300"))


templates <- list(monochrome = col2hsv(samples$monochrome), 
                  sequential = col2hsv(samples$sequential),
                  sequential2 = col2hsv(samples$sequential2),
                  low_light = data.frame(h = rep(1, 9), s = rep(0.8, 9), v= rep(0.5, 9)),
                  pastel = data.frame(h = rep(1, 9), s = rep(0.6, 9), v= rep(0.9999, 9)), 
                  bright = data.frame(h = rep(1, 9), s = rep(0.999, 9), v= rep(0.9999, 9)), 
                  u = data.frame(h = rep(1, 9), s = c(0.999, 0.8, 0.7, 0.6, 0.5, 0.6, 0.7, 0.8, 0.99), 
                                 v = c(0.999, 0.8, 0.7, 0.6, 0.5, 0.6, 0.7, 0.8, 0.99)),
                  divergent = col2hsv(samples$divergent),
                  complementary = col2hsv(samples$complementary))

save(templates, file = "data/templates.Rdata")

# assign("templates", templates, envir = .GlobalEnv)
# assign("samples", samples, envir = .GlobalEnv)
# assign("rg", rg, envir = .GlobalEnv)



############ Color wheel plotting #########

# r  <- seq(0,1,length=201)
# th <- seq(0,2*pi, length=201)
# d  <- expand.grid(r=r,th=th)
# gg <- data.frame(h = th, s = r, l = r)
# gg <- with(d,data.frame(d,x=r*sin(th),y=r*cos(th),
#                         z=hcl(h=360*th/(2*pi),c=100*r, l=65)))
# 
# 
# ggplot(gg) +
#   geom_point(aes(x,y, color=z), size=3)+
#   scale_color_identity()+labs(x="",y="") +
#   coord_fixed()
# 
# # Create hsv grid
# d = expand.grid(h=seq(0,1,0.01), s=seq(0,1,0.05), v=1)
# 
# ggplot() +
#   coord_polar(theta="x") +
#   scale_x_continuous(breaks=NULL) +
#   scale_y_continuous(breaks=NULL) +
#   scale_fill_identity() +
#   geom_rect(data=d, mapping=aes(xmin=h, xmax=h+resolution(h),
#                                 ymin=s, ymax=s+resolution(s),
#                                 fill=hsv(h,s,v)))+ theme_bw() +
#   labs(x = "hue", y = "saturation") + ggtitle("hsv grid, v = 1")
# 


###### Color wheels #######
# 
# all <- expand.grid(h=seq(0, 1, 0.01), s= seq(0, 1, 0.05), v = seq(0, 1, 0.2))
# 
# ggplot() +
#   coord_polar(theta="x") +
#   scale_x_continuous(breaks=NULL) +
#   scale_y_continuous(breaks=NULL) +
#   scale_fill_identity() +
#   geom_rect(data=all, mapping=aes(xmin=h, xmax=h+resolution(h),
#                                   ymin=s, ymax=s+resolution(s),
#                                   fill=hsv(h,s,v)))+ theme_bw() +
#   facet_wrap(vars(v)) 
# 
# 
# 
# ######## Color scheme color wheel vis #######
# df <- as.data.frame(col2hsv(Sequential$BlPr))
# df2 <- col2rgb2(Divergent$OrNPur)
# 
# p <- ggplot() +
#   coord_polar(theta="x") +
#   scale_x_continuous(breaks=NULL) +
#   scale_y_continuous(breaks=NULL) +
#   scale_fill_identity() +
#   geom_rect(data=all, mapping=aes(xmin=h, xmax=h+resolution(h),
#                                   ymin=s, ymax=s+resolution(s),
#                                   fill=hsv(h,s,mean(df$v))))+ theme_bw() 
#  p + geom_point(data = df, aes(x = h, y = s), pch = 1)
# 
#  
# 

