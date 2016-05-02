

# Code to upload data from CellSet files (encoded as xlsx files) and group them into a single file.
# Read xlsx files (all stored in the same directory)

t1 <- Sys.time()
library(readxl)

dir <- "~/Desktop/datapoints_old_images/"
list.f <- list.files(dir)
j <- 1
rs <- NULL
for(f in list.f){
  name <- gsub(".xlsx", "", f)
  for(i in 1:20){
    tryCatch({
      temp <- read_excel(paste0(dir, f), sheet = i)
      temp <- temp[!is.na(temp[,1]),]
      rs <- rbind(rs, data.frame(line=name, root=i, cell_type=temp$Label, value=scale(temp[["Average flourescence"]])))
      print(j); j <- j+1
    },warning = function(w) {
    }, error = function(e) {
    })
  }
}
remove(temp, i, f, list.f, name)

# Average the data by line, root, cell type
library(plyr)
mean_data <- ddply(rs, .(line, root, cell_type), summarise, value=mean(value))
mean_data <- mean_data[!is.na(mean_data$value),]

mean_data_2 <- ddply(rs, .(line), summarise, value=mean(value))

# Reshape the data to have them in the proper form for the analysis
library(reshape2)
data <- dcast(mean_data, line + root ~ cell_type)

write.csv(data, "~/Desktop/root_data.csv")

t2 <- Sys.time()


library(ggplot2); library(gridExtra)
my_hist<-ggplot(diamonds, aes(clarity, fill=cut)) + geom_bar() 

#create inset table 
my_table<- tableGrob(head(diamonds)[,1:3], 
                     gpar.coretext =gpar(fontsize=8), gpar.coltext=gpar(fontsize=8),  
                     gpar.rowtext=gpar(fontsize=8)) 

#Extract Legend 
g_legend<-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 

legend <- g_legend(my_hist) 


length(my.col)
y <- seq(from=temp$)
x <- rep(1, length(my.col))
temp.col <- data.frame(x, y, my.col)
ggplot(temp.col, aes(x, y, colour=factor(my.col))) + 
  geom_point(pch=15, size=4) + 
  scale_colour_manual(values=my.col) +
  theme_minimal() + 
  theme(legend.position="none") + 
  xlim(c(0.9, 1.1))

print(t2-t1)


