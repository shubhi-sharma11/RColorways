# Define UI for dataset viewer app ----
library(shiny)

# default_reds <- list("Red-Neutral" = c("#4F000B", "#720026", "#CE4257", "#FF7F51", "#FF9B54"), 
#                      "Burnt" = c("#03071e", "#370617", "#6a040f", "#9d0208", "#d00000", "#dc2f02", "#e85d04", "#f48c06", "#faa307", "#ffba08"), 
#                      "Red-Pink" = c("#590d22", "#800f2f", "#a4133c", "#c9184a", "#ff4d6d", "#ff758f", "#ff8fa3", "#ffb3c1", "#ffccd5", "#fff0f3"), 
#                      "Summer" = c("#372b2b", "#850a0a", "#a70b0b", "#f1ba0a", "#f6de26"), 
#                      "Tulip" = c("#5e0b15", "#90323d", "#d9cab3", "#bc8034", "#8c7a6b"), 
#                      "BBQ" = c("#ffc857", "#e9724c", "#c5283d", "#481d24", "#255f85"), 
#                      "Angry-Peach" = c("#010101", "#28100b", "#4e2015", "#752f1f", "#9c3e28", "#c24e32", "#e95d3c"))
# 
# default_yellow <- list("Bright" = c("#ff4800", "#ff5400", "#ff6000", "#ff6d00", "#ff7900", "#ff8500", "#ff9100", "#ff9e00", "#ffaa00", "#ffb600"), 
#                        "Sandy" = c("#ffe169", "#fad643", "#edc531", "#dbb42c", "#c9a227", "#b69121", "#a47e1b", "#926c15", "#805b10", "#76520e"), 
#                        "Bumblebee" = c("#d6d6d6", "#ffee32", "#ffd100", "#333533", "#202020"), 
#                        "Yellow-Orange" = c("#f7f722", "#f2cf1c", "#eda716", "#e77f0f", "#e25709"), 
#                        "Red-Yellow" = c("#ff0000", "#ff8700", "#ffd300", "#ffdc33", "#ffeb85"))
# 
# default_green <- list("Lime-Lemon" = c("#007f5f", "#2b9348", "#55a630", "#80b918", "#aacc00", "#bfd200", "#d4d700", "#dddf00", "#eeef20", "#ffff3f"), 
#                       "Seagreen" = c("#99e2b4", "#88d4ab", "#78c6a3", "#67b99a", "#56ab91", "#469d89", "#358f80", "#248277", "#14746f", "#036666"), 
#                       "Mossy" = c("#1f2421", "#216869", "#49a078", "#9cc5a1", "#dce1de"),
#                       "Calm" = c("#031926", "#468189", "#77aca2", "#9dbebb", "#f4e9cd"), 
#                       "Greenblue" = c("#d9ed92","#b5e48c", "#99d98c", "#76c893", "#52b69a", "#34a0a4","#168aad", "#1a759f", "#1e6091", "#184e77"),
#                       "Tropical" = c("#132a13", "#31572c", "#4f772d", "#90a955", "#ecf39e"))
# 
# default_blue <- list("Deepsea" = c("#006466", "#065a60", "#0b525b", "#144552", "#1b3a4b", "#212f45", "#272640", "#312244", "#3e1f47", "#4d194d"), 
#                      "Glacial" = c("#03045e", "#023e8a", "#0077b6", "#0096c7", "#00b4d8", "#48cae4", "#90e0ef", "#ade8f4", "#caf0f8"), 
#                      "Muted" = c("#012a4a", "#013a63", "#01497c", "#014f86", "#2a6f97", "#2c7da0", "#468faf", "#61a5c2", "#89c2d9", "#a9d6e5"), 
#                      "BlueGrey" = c("#0466c8", "#0353a4", "#023e7d", "#002855", "#001845", "#001233", "#33415c", "#5c677d", "#7d8597", "#979dac"),
#                      "DawnDusk" = c("#07c8f9", "#09a6f3", "#0a85ed", "#0c63e7", "#0d41e1"), 
#                      "BlueNeutral" = c("#160f29", "#246a73", "#368f8b", "#f3dfc1", "#ddbea8"),
#                      "BluePurple" = c("#97dffc", "#858ae3", "#613dc1", "#4e148c", "#2c0735"), 
#                      "Violet" = c("#b28dff", "#bc98ff", "#c5a3ff", "#d1b1ff", "#d7b8ff", "#dcbfff", "#e3c7ff", "#e9cfff", "#f2daff", "#fbe4ff"), 
#                      "Purple"= c("#10002b", "#240046", "#3c096c", "#5a189a", "#7b2cbf", "#9d4edd", "#c77dff", "#e0aaff"))
# 
# default_grey <- list("Tinted" = c("#bfc3ba", "#a9aca9", "#60495a", "#3f3244", "#2f2235"), 
#                      "GreyNeutral" = c("#565264", "#706677", "#a6808c", "#ccb7ae", "#d6cfcb"),
#                      "Classic" = c("#f8f9fa", "#e9ecef", "#dee2e6", "#ced4da", "#adb5bd", "#6c757d", "#495057", "#343a40", "#212529"))
# 
# # Divergent, Sequential, Pastel, Retro, Rainbow, Terrain
# 
# default_divergent <- list("HotCold" = c("#001219", "#005f73", "#0a9396", "#94d2bd", "#e9d8a6", "#ee9b00", "#ca6702", "#bb3e03", "#ae2012", "#9b2226"), 
#                           "Modern" = c("#ff8811", "#f4d06f", "#fff8f0", "#9dd9d2", "#392f5a"), 
#                           "Cenote" = c("#006d77", "#83c5be", "#edf6f9", "#ffddd2", "#e29578"),
#                           "Orchard" = c("#386641", "#6a994e", "#a7c957", "#f2e8cf", "#bc4749"), 
#                           "Superhero" = c("#283d3b", "#197278", "#edddd4", "#c44536", "#772e25"), 
#                           "Bio" = c("#05668d", "#427aa1", "#ebf2fa", "#679436", "#a5be00"), 
#                           "Sahara" = c("#7c6a0a", "#babd8d", "#ffdac6", "#fa9500", "#eb6424"), 
#                           "USA" = c("#800016", "#a0001c", "#c00021", "#ff002b", "#fdf0d5", "#407ba7", "#004e89", "#002962", "#00043a"), 
#                           "VioletCoral" = c("#5769ad", "#a4a8d4", "#ebe3eb", "#f6998c", "#f15b52"), 
#                           "Watermelon" = c("#cc2b2b", "#df6868", "#f5e2c8", "#6fb67a", "#1f7b2d"))
# 
# 
# default_sequential <- list( "BoldRedBlue" = c("#453f78", "#759aab", "#6b0504", "#a3320b", "#e6af2e"), 
#                             "SummerEvening" = c("#fa7921", "#fe9920", "#b9a44c", "#566e3d", "#0c4767"),
#                             "Oasis" = c("#264653", "#2a9d8f", "#e9c46a", "#f4a261", "#e76f51"), 
#                             "SunSea" = c("#ff5400", "#ff6d00", "#ff8500", "#ff9100", "#ff9e00", "#00b4d8", "#0096c7", "#0077b6", "#023e8a", "#03045e"),
#                             "Parrot" = c("#034732", "#008148", "#c6c013", "#ef8a17", "#ef2917"), 
#                             "Subdued" = c("#94b9af", "#90a583", "#9d8420", "#942911", "#593837"), 
#                             "LightPeacock" = c("#666a86", "#788aa3", "#92b6b1","#b2c9ab", "#e8ddb5"), 
#                             "PinkBlue" = c("#f72585", "#b5179e", "#7209b7", "#560bad", "#480ca8", "#3a0ca3", "#3f37c9", "#4361ee", "#4895ef", "#4cc9f0"), 
#                             "Volcano" = c("#003049", "#d62828", "#f77f00", "#fcbf49", "#eae2b7"), 
#                             "IndianInk" = c("#000814", "#001d3d", "#003566", "#ffc300", "#ffd60a"), 
#                             "NeonNight" = c("#053c5e", "#1d3958", "#353652", "#4c334d", "#643047", "#7c2e41", "#942b3b", "#ab2836", "#c32530", "#db222a"), 
#                             "FallEvening" = c("#1f2041", "#4b3f72", "#ffc857", "#119da4", "#19647e")) 
# 
# default_pasteltints <- list("Icecream" = c("#fec5bb", "#fcd5ce", "#fae1dd", "#f8edeb", "#e8e8e4", "#d8e2dc", "#ece4db", "#ffe5d9", "#ffd7ba", "#fec89a"), 
#                             "Newborn" = c("#cdb4db", "#ffc8dd", "#ffafcc", "#bde0fe", "#a2d2ff"),
#                             "Peonies" = c("#d8e2dc", "#ffe5d9", "#ffcad4", "#f4acb7", "#9d8189"))
# 
default_retro <- list("Stereo" = c("#003049", "#d62828", "#f77f00", "#fcbf49", "#eae2b7"),
                      "Candy" = c("#ed6a5a", "#f4f1bb", "#e6ebe0", "#9bc1bc", "#36c9c6"),
                      "Diner" = c("#e63946", "#f1faee", "#a8dadc", "#457b9d", "#1d3557"),
                      "Stripes" = c("#f4f1de", "#e07a5f", "#3d405b", "#81b29a", "#f2cc8f"),
                      "Rollerblades" = c("#ffbe0b", "#fb5607", "#ff006e", "#8338ec", "#3a86ff"),
                      "Beach" = c("#0081a7", "#00afb9", "#fdfcdc", "#fed9b7", "#f07167"),
                      "70s" = c("#335c67", "#fff3b0", "#e09f3e", "#9e2a2b", "#540b0e"),
                      "Holiday" = c("#ffbc42","#d81159", "#8f2d56", "#218380", "#73d2de"))

default_rainbows <- list("Pastel" = c("#ffadad", "#ffd6a5", "#fdffb6", "#caffbf", "#9bf6ff", "#a0c4ff", "#bdb2ff", "#ffc6ff"),
                         "Standout" = c("#ef476f", "#ffd166", "#06d6a0", "#118ab2", "#073b4c"),
                         "Aesthetic" = c("#f94144", "#f3722c", "#f8961e", "#f9844a", "#f9c74f", "#90be6d", "#43aa8b", "#4d908e", "#577590", "#277da1"),
                         "Simple" = c("#f94144", "#f3722c", "#f8961e", "#f9c74f", "#90be6d", "#43aa8b", "#577590"),
                         "Highlights" = c("#ff0000", "#ff8700", "#ffd300", "#deff0a", "#a1ff0a", "#0aff99", "#0aefff", "#147df5", "#580aff", "#be0aff"))
# 
# default_terrains <- list("MossyRocks" = c("#797d62", "#9b9b7a", "#d9ae94", "#f1dca7", "#ffcb69", "#d08c60", "#997b66"),
#                          "Arizona" = c("#cb997e", "#ddbea9", "#ffe8d6", "#b7b7a4", "#a5a58d", "#6b705c"), 
#                          "Dirt" = c("#ede0d4", "#e6ccb2", "#ddb892", "#b08968", "#9c6644", "#7f5539"), 
#                          "Evening" = c("#582f0e", "#7f4f24", "#936639", "#a68a64", "#b6ad90", "#c2c5aa", "#a4ac86", "#656d4a", "#414833", "#333d29"))


Monochrome <- list("Red" =  c("#4F000B", "#610019", "#720026", "#A0213F", "#CE4257", "#E76154", "#FF7F51", "#FF8D53", "#FF9B54"),
                   "Red_dark" = c("#370617", "#6a040f", "#9d0208", "#d00000", "#dc2f02", "#e85d04", "#f48c06", "#faa307", "#ffba08"),
                   "Yellow" =  c("#663800", "#8F4F00", "#B86500", "#DC8003", "#FF9B05", "#FFAE29", "#FFC14D", "#FFCF75", "#FFDEA1"), 
                   "Green" = c("#007f5f", "#2b9348", "#55a630", "#80b918", "#aacc00", "#bfd200", "#dddf00", "#eeef20", "#ffff3f"), 
                   "Green_dark" = c("#002400", "#143005", "#273B09", "#405013", "#58641D", "#6A7A34", "#7B904B", "#BDC596", "#FEFAE0"), 
                   "Blue" = c("#03045e", "#023e8a", "#0077b6", "#0096c7", "#00b4d8", "#48cae4", "#90e0ef", "#ade8f4", "#caf0f8"), 
                   "Blue_dark" = c("#011523", "#012948", "#014270", "#1A5C85", "#347494", "#4A91B0", "#6EAFCB", "#94C8DC", "#B9DDE8"), 
                   "Purple" = c("#60086F", "#613371", "#6D5480", "#726485", "#78748B", "#9F9FAB", "#C6C5CB", "#D5D4DB", "#E4E3E6"), 
                   "Purple_dark" = c("#292929", "#36172E", "#56153F", "#74114F", "#851D59", "#8B335E", "#914862", "#975E67", "#9C736B"), 
                   "Grey" = c("#f8f9fa", "#e9ecef", "#dee2e6", "#ced4da", "#adb5bd", "#6c757d", "#495057", "#343a40", "#212529"))


# Sequential (Multi-hue)

Sequential <- list("RdYBlu" = c("#481D24", "#872331", "#C5283D", "#D74D45", "#E9724C", "#F49D52", "#FFC857", "#255F85", "#373E55"),
                   "RdYBrown" = c("#CC0000", "#E33505", "#F96909", "#F89D0E", "#F8B710", "#B27D19", "#8F531C", "#6C281F", "#300702"),
                   "OrYBlu" = c("#FF5400", "#FF7900", "#FF9E00", "#E6AF37", "#E4D87B", "#72C6AA", "#00AACC", "#023E8A", "#03045E"),
                   "LimeBlue" = c("#d9ed92","#b5e48c", "#99d98c", "#52b69a", "#34a0a4","#168aad", "#1a759f", "#1e6091", "#184e77"), 
                   "GrYl" = c("#00523D", "#0B6844", "#167D4B", "#55A630", "#80B918", "#AACC00", "#D5DF22", "#EAEB43", "#FFFF85"), 
                   "GrYRd" = c("#266204", "#317E05", "#7B9014", "#C0B716", "#FAD016" ,"#F96336","#D62E29", "#991821", "#550C18"), 
                   "GrYBlu" = c("#1C7C36", "#62AD59", "#A8DD7C", "#D9ED92", "#FBF8C4", "#34A0A4", "#24798B", "#145172", "#0E2C44"), 
                   "CyanBur" = c("#93D4ED", "#8AAEE3", "#8D94D9", "#9775C7", "#9F51B5", "#A33498", "#951664", "#7D1547", "#661426"),
                   "BlYRd" = c("#241851", "#1D3E78", "#026C7C", "#02C1DE", "#FBDB4B", "#F96336", "#D62E29", "#991821", "#550C18"),
                   "BlPr" = c("#1B5F9E", "#5471BA", "#7D88C7", "#A4A7D8", "#CAC5E8", "#B287AD", "#994872", "#7D1533", "#661416"),
                   "BlYPink" = c("#2F4858", "#33658A", "#55DDE0", "#FFD166", "#F6AE2D", "#F38F4D", "#EF6F6C", "#E44063", "#D81159"),
                   "BlRd" =  c("#1d3958", "#353652", "#4c334d", "#643047", "#7c2e41", "#942b3b", "#ab2836", "#c32530", "#db222a"),
                   "PrBlN" = c("#230C0F", "#4A0D67", "#463379","#42588B", "#4AACB4", "#73C4C1", "#9BDCCD","#E0DDCF", "#F1F0EA") 
                   )

# Divergent 

Divergent <- list("BlackYl" = c("#000000", "#403E36", "#807C6B", "#C0BAA1", "#FFF8D6", "#FFE564", "#FFD60A", "#FFC300", "#FFA217"),
                  "PrYRd" = c("#290029", "#3E2463", "#52489C", "#B191FF", '#FBDB4B', "#F96336", "#D62E29", "#991821", "#550C18"),
                  "PrYl"= c("#46237A", "#5D428B", "#73619B", "#A09EBB", "#FFF8D6", "#FFE564", "#FFD60A", "#FFC300", "#FFA217"),
                  "BluNBr" = c("#274C77", "#5A82A2", "#8AADC0", "#B9D8DE", "#FFEDD6", "#EFC680", "#A67D44", "#815722", "#5B3000"),
                  "BlNGn" = c("#274C77", "#5A82A2", "#8AADC0", "#B9D8DE", "#EAF7CF", "#D1E399", "#B7CE63", "#60A857", "#09814A"),
                  "BlYl" = c("#000814", "#001D3D", "#003566", "#809592", "#FFF8D6", "#FFE564", "#FFD60A", "#FFC300", "#FFA217"),
                  "BlNPink" = c( "#0081A7", "#0098B0", "#00AFB9", "#7ED5CA", "#FDFCDC", "#FDEAC9", "#FED9B7", "#F7A48F", "#F07167"),
                  "BlWRd" = c("#283D3B", "#205759", "#197278", "#83A7A6", "#EDDDD4", "#D89185", "#C44536", "#9D392D" ,"#772E25"),
                  "GrNPur" = c("#18592E", "#119E40", "#63BB6F", "#B8DCA8", "#EDEDB9", "#E1BFF2", "#BD7CDE", "#6B0B9C", "#36084D"),
                  "OrNGr" = c("#317E05", "#889F04", "#B7BA3D", "#E6D575", "#FFEBB5", "#F1BA5C", "#E38903", "#D76A03", "#BF3100"),
                  "GrYlPur" = c("#00523D", "#157949", "#55A630", "#AACC00", "#FFF899", "#E8D498", "#955CAD", "#762696", "#320A3E"),
                  "OrNPur" = c("#8C3E03", "#C85604", "#DC7F28", "#F0A84C", "#EFE094", "#D7AAEE", "#BD7CDE", "#6B0B9C", "#36084D"),
                  "OrNPur2" = c("#FF8811", "#FAAC40", "#F4D06F", "#FAE4B0", "#FFF8F0", "#CEE9E1", "#6CC6BB", "#61549E", "#3B2683"),
                  "RdNPur" = c("#6A041D", "#F50000", "#FD642E", "#FAAC40", "#FFDAA9", "#D8CFDC", "#B0A6C7", "#61549E", "#3B2683"),
                  "RdNGr" = c("#871D1D", "#C63333", "#D86D66", "#E9A798", "#F3E6C8", "#B1CEA1", "#6FB67A", "#479954", "#1F7B2D"),
                  "RdWBl" = c("#550304", "#9B1013", "#E11D21", "#E77375", "#F5F3F4", "#D3D3D3", "#8D7D7C", "#5E5858", "#0B090A"),
                  "RdYBlu2" = c("#AE2012", "#BB3E03", "#CA6702", "#EE9B00", "#E9D8A6", "#94D2BD", "#0A9396", "#005F73", "#001219"),
                  "PinkNGr" = c("#9D1B7C", "#BB548A", "#D98D97", "#E0B6AF", "#E7DEC6", "#9CA334", "#749025", "#4B7D15", "#296614"),
                  "PinkBlue2" = c("#FE938C", "#F2A694", "#E6B89C", "#E8C5A4", "#EAD2AC", "#C3C1B2", "#9CAFB7", "#6F98AE", "#4281A4"),
                  "BrNBl2" = c("#9E631B", "#BAA454", "#C6C07D", "#CCCE91", "#D1DCA5", "#7BB683", "#489A75", "#157D66", "#146266"),
                  "BrNGr" = c("#3D320F", "#8A5911", "#AD7B23", "#C2AC69", "#E5EBD6", "#ADC487", "#7B9948", "#597738", "#375427")

)

# Qualitative palettes 
Qualitative <- list( "Pal1" = c("#FA7921", "#566E3D", "#FEAC48", "#3E2F5B", "#0C4767", "#892F50", "#4C2719", "#3B7D75", "#780116"),
                     "Pal3" = c("#A8F0E7", "#1B998B", "#FF1053", "#FF6978", "#FFE68E", "#F4B942", "#296EB4", "#12355B", "#502358"),
                     "Pal4" = c("#F0B3A8", "#992E1C", "#007A78", "#60CADF", "#948FFF", "#424EF5", "#B5952A", "#5C4C12", "#24592A"),
                     "Pal7" = c("#8AD6FF", "#007DD1", "#B0FF6B", "#0ECC00", "#FF9694", "#FF0004", "#FFBF6B", "#FF8000", "#D88AFF"),
                     "Pal9" = c("#F09124", "#360363", "#6504BA", "#CEB3E5", "#89BF5A", "#1F754D", "#75A2E1", "#B02365", "#5E0830"))

# main <- list("Red" = default_reds,
#              "Yellow" = default_yellow, 
#              "Green" = default_green, 
#              "Blue" = default_blue, 
#              "Grey" = default_grey, 
#              "Divergent" = default_divergent, 
#              "Sequential" = default_sequential, 
#              "PastelTint" = default_pasteltints, 
#              "Retro" = default_retro, 
#              "Rainbow" = default_rainbows, 
#              "Terrain" = default_terrains)

main <- list("Monochrome" = Monochrome, 
             "Sequential" = Sequential, 
             "Divergent" = Divergent, 
             "Qualitative" = Qualitative, 
             "Rainbow" = default_rainbows, 
             "Retro" = default_retro)

main_names <- list("Monochrome_names" = as.character(names(main$Monochrome)), 
                   "Sequential_names" = as.character(names(main$Sequential)), 
                   "Divergent_names" = as.character(names(main$Divergent)), 
                   "Qualitative_names" = as.character(names(main$Qualitative)), 
                   "Retro_names" = as.character(names(main$Retro)), 
                   "Rainbow_names" = as.character(names(main$Rainbow)))

stretchPal <- function(user_cols, num_cols){
  
  colfunc <- grDevices::colorRampPalette(user_cols)
  hex <- colfunc(num_cols)
  return(hex)
}

ui <- fluidPage(
  
  # App title ----
  titlePanel("Test your colorway!"),
  
  sidebarPanel(
  selectInput("category", "Choose a theme:", names(main)),
  selectInput("choice", "Select a palette:", c(main_names$Monochrome_names)), 
  plotOutput(outputId = "colorway", height = "50px"),
  selectInput("plottype", "Select a plot type",
                          c("Scatter", "Boxplot", "Choropleth", "Hexbin"))
 
  ),
  
 #sidebarPanel(  ),
  
  mainPanel(
    # plot
    # mainPanel(
    #   fluidC(
    #     column(width = 6,
    #            plotOutput("distPlot")
    #     ),
    #     column(width = 6,
    #            plotOutput("colorway"))
    #   )
    plotOutput(outputId = "distPlot")
   
  )

)

server <- function(input, output, session) {
  observe({
    updateSelectInput(session, "choice", choices = main_names[[paste0(input$category, "_names")]])
  })
  
  output$distPlot <- renderPlot({
    
    library(ggplot2)
    
    if(input$plottype == "Scatter"){
    
    df <- data.frame(x = rnorm(1000))
    df$y <- .3*(df$x) + 0.1*rnorm(1000)
    df$y[sample( 1:1000, 100)] <- 0.5+.3*(df$x)[sample( 1:1000, 100)] + abs(0.1*rnorm(100))
    df$y[sample( 1:1000, 100)] <- -0.5 +.3*(df$x)[sample( 1:1000, 100)] - abs(0.1*rnorm(100))
    
    big_list <- main[[input$category]]
    sv <- which(input$choice == names(big_list))
    pal <- big_list[[sv]]
    
   pl <-  ggplot(df, aes(x = x, y = y, col = y)) + geom_point(size = 3)+
      scale_color_gradientn(colors = pal, limits = c(min(df$y), max(df$y))) + ylim(c(-1,1)) + xlim(c(-2,2))+ 
      theme_bw() 
    }
    
    if(input$plottype == "Boxplot"){
      
      A <- rnorm(100, 3, 4)
      B <- rnorm(100, 0, 5)
      C <- rnorm(100, -1, 10)
      D <- rnorm(100, 5, 2)
      E <- rnorm(100, 1, 4)
      
      df <- data.frame("data" = c(A, B, C, D, E), 
                       "group" = rep(c("A", "B", "C", "D", "E"), each = 100))
      
      big_list <- main[[input$category]]
      sv <- which(input$choice == names(big_list))
      pal <- big_list[[sv]]
      
      if(length(pal) > 5) {
        pal <- pal[c(1, 3, 5, 7, 9)]
      }
      pl <-  ggplot(df, aes(x = as.factor(group), y = data, fill = as.factor(group))) +
        geom_boxplot()+
        scale_fill_manual(values = pal) + 
        theme_bw() 
      
      
    }
    
    if(input$plottype == "Choropleth"){
      
      big_list <- main[[input$category]]
      sv <- which(input$choice == names(big_list))
      pal <- big_list[[sv]]
      
      states <- map_data("state")
      arrests <- USArrests
      names(arrests) <- tolower(names(arrests))
      arrests$region <- tolower(rownames(USArrests))
      
      choro <- merge(states, arrests, sort = FALSE, by = "region")
      choro <- choro[order(choro$order), ]
      pl <- ggplot(choro, aes(long, lat)) +
        geom_polygon(aes(group = group, fill = assault)) + 
        scale_fill_gradientn(colors = pal) + theme_bw()
    }
    
    
    if(input$plottype == "Hexbin"){
      big_list <- main[[input$category]]
      sv <- which(input$choice == names(big_list))
      pal <- big_list[[sv]]
      
      var1 <- rnorm(1500, 5, 4)
      var2 <- rnorm(1500,3, 2)
      posterior <- data.frame("iteration" = 1:length(data), 
                              "var1" = var1, 
                              "var2" = var2)
    
      
      pl <- bayesplot::mcmc_hex(posterior, pars = c("var1", "var2")) + 
        scale_fill_gradientn(colors = pal) + labs("fill" = "Probability")
      
    }
    
   pl
  })
  
  output$colorway <- renderPlot({
    
    library(ggplot2)
    
    big_list <- main[[input$category]]
    sv <- which(input$choice == names(big_list))
    pal <- big_list[[sv]]
    
    pltCols(pal)
    
    
    
    
  })
  
  
  
}


shinyApp(ui, server)


