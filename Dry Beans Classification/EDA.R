
library("Hmisc")
library(corrplot)
library(GGally)
library("dplyr")
library("gplots")
library("ggplot2")
library("ggpubr")
library("readxl")
library("parallel")
library(ggfortify)
numCores <- detectCores()
numCores


Dry_Bean_Dataset <- read_excel("Downloads/DryBeanDataset/Dry_Bean_Dataset.xlsx")
View(Dry_Bean_Dataset)
data = Dry_Bean_Dataset

head(data)[,1:17]




dim(data) # check if the data has been properly imported
names(data)
summary(data) #get the summary

classes <- table(data$Class)
classes
print(ifelse(length(classes)==2, "Binary Classification", "MultiClass Classification"))

#groupby class
df <- data %>%
  group_by(Class) %>%
  summarise(counts = n())
df

n <- nrow(data) #get the total no of rows
df2 <- data[1:16]


percentage <- (classes/n*100)
percentage
barplot(percentage, ylim= c(0,40), ylab= "Percentage", main = "Barplot of beans class distribution" , cex.names=0.7)

# Display cout X class bar w/ numeric val

theme_set(theme_pubr())


ggplot(df, aes(x = Class, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()

#Creating a histogram of all the features for Univariate analysis
h1 <- mapply(hist,as.data.frame(df2),main=colnames(df2),xlab=colnames(df2))
#Univariate Analysis Feature vs Class

make_plot <- function(df, x_var, y_var, x_name, y_name){
  p <- ggplot(df, aes(x_var, y_var), xlab = x_name, ylab = y_name) +geom_violin()
  p + geom_boxplot(width=0.1) + xlab(x_name) + ylab(y_name)
}

make_plot(data, data$Class, data$Area, 'Class', 'Area' )
make_plot(data, data$Class, data$Perimeter,'Class','Perimeter')
make_plot(data, data$Class, data$MajorAxisLength,'Class','Major Axis Length')
make_plot(data, data$Class, data$MinorAxisLength,'Class','Minor Axis Length')
make_plot(data, data$Class, data$AspectRation,'Class','Aspect Ration')
make_plot(data, data$Class, data$Eccentricity,'Class','Eccentricity')
make_plot(data, data$Class, data$ConvexArea,'Class','Convex Area')
make_plot(data, data$Class, data$EquivDiameter,'Class','EquivDiameter')
make_plot(data, data$Class, data$Extent,'Class','Extent')
make_plot(data, data$Class, data$Solidity,'Class','Solidity')
make_plot(data, data$Class, data$roundness,'Class','Roundness')
make_plot(data, data$Class, data$Compactness,'Class','Compactness')
make_plot(data, data$Class, data$ShapeFactor1,'Class','ShapeFactor1')
make_plot(data, data$Class, data$ShapeFactor2,'Class','ShapeFactor2')
make_plot(data, data$Class, data$ShapeFactor3,'Class','ShapeFactor3')
make_plot(data, data$Class, data$ShapeFactor4,'Class','ShapeFactor4')
            




par(mfrow = c(1, 1))
data.cor = cor(df2)
data.cor = cor(df2, method = c("pearson"))
data.rcorr = rcorr(as.matrix(df2))
data.rcorr

data.coeff = data.rcorr$r
data.p = data.rcorr$P

data.p

corrplot(data.cor, type = "lower", order = "hclust")
heatmap.2(data.cor, scale = "none", col = bluered(100), trace = "none", density.info = "none")

par(mfrow = c(1, 1))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(data.cor, method = "color", col = col(200),  
         type = "lower", order = "hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "darkblue", tl.srt = 90, #Text label color and rotation
         # Combine with significance level
         p.mat = data.p, data.coeff = 0.01,
         diag = FALSE
)

#Multivariate
pca_res <- prcomp(df2, scale. = TRUE)

autoplot(pca_res, data = data, colour = 'Class')

pl <- mclapply(autoplot(pca_res, data = data, colour = 'Class'))

ggpairs(data[,1:16], aes(color = data$Class), upper=NULL) + theme_bw()

c <- data[, 17]
c
l <- length(unique(c))
l
pairs(df,
      pch = 22,
      bg = hcl.colors(l, "Temps")[species],
      col = hcl.colors(l, "Temps")[species])



pairs(df2,aes(color = data$Class),) +theme_bw()


std_dev_data=round(apply(data[,-16],2,sd)) # Standard deviation by column
mean_data=round(apply(data[,-16],2,mean),4) # Mean for each column


c_hist <- function(x,y){
  hist(x, main = y ,xlab = y)
  abline(v = mean(x), col="red", lwd=3, lty=2)
}

h <- lapply(df2, colname(df2) ,FUN= c_hist)
h


train<-sample_frac(data, 0.75)
sid<-as.numeric(rownames(train)) # because rownames() returns character
test<-data[-sid,]
train
test