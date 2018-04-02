#####################################################
#####################################################
#####################################################
#####################################################

# The code is originally wrote for the following project
# Paper: IIGW
# Data: EBTKE hot water Gorontalo (Yuanno Rezky)
# Team leader: Prihadi S.
# Code and analysis: Dasapta Erwin Irawan and Prana Ugi

#####################################################
#####################################################
#####################################################
#####################################################

# Note: remove the '#' symbol only if you haven't done it previously


# Set up working folder

# install packages

## for data manipulation
install.packages('tidyverse')
library('tidyverse')

## for scatter plot matrix
install.packages("ggcorrplot") # omit tag if you have install it
library('ggcorrplot')

## for multivariabel analysis (PCA and Cluster)
## from Bioconductor server
## pcaMethods package from Bioconductor server
source("http://bioconductor.org/biocLite.R") 
biocLite("pcaMethods")
library('pcaMethods')

install.packages('cluster')
library(cluster) # for cluster analysis
install.packages('FactoMineR')
library('FactoMineR')
install.packages('factoextra')
library('factoextra')

# if you like vegan you can use it
#install.packages('vegan')
#library(vegan)

## for multiregression package
install.packages('gam')
library('gam')
install.packages('rpart')
library(rpart)

## locating and imputing missing data
install.packages('mice')
library('mice')

# Load data
df <- read.csv("data_copy.csv", header=T) # load data 
row.names(df) <- df$Code # setting row names
df2 <- df[4:32] # exclude Location name

# Evaluating data 
str(df2) # observe data structure (str)
head(df2) #observe data 
tail(df2) # observe data
is.na(df2) # checking NA if any

# Exploratory using pairs() function
# Assesing data patterns
cor.tab <- cor(df2)
cor.tab

ggcorrplot::ggcorrplot(cor.tab)              # making heatmap

# Run PCA 
res.pca <- PCA(df2, graph = FALSE)
md.pattern(df2) # from package 'mice'


res.pca <- FactoMineR::PCA(df2, graph = FALSE)
eigenvalues <- res.pca$eig
head(eigenvalues[, 1:2])
barplot(eigenvalues[, 2], names.arg=1:nrow(eigenvalues), 
        main = "Variances",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")

# Add connected line segments to the plot
lines(x = 1:nrow(eigenvalues), 
      eigenvalues[, 2], 
      type="b", pch=19, col = "red")
factoextra::fviz_pca(res.pca, choix = "var", col.var="contrib") 

plot(res.pca, choix = "ind")

# Run Cluster 
distance <- dist(scale(df), method = "euclidean")
cluster <- hclust(distance, method = "complete")
plot(cluster, cex = 0.6, hang = -1, main = "Hot water") 
rect.hclust(cluster, k = 3, border = 2:4) 

# Run Reg Tree
tree.fit <- rpart(Cl ~ ., data = df) # df must be in data frame class
printcp(tree.fit) # display the results
plotcp(tree.fit) # visualize cross-validation results
summary(tree.fit) # detailed summary of splits
plot(tree.fit, uniform=TRUE,
     main="Tree classification of hot water samples")
text(tree.fit, use.n=TRUE, all=TRUE, cex=.8)

####################################
####################################
####################################
gam1 <- gam(Cl ~ Ca + Na + Mg + 
              K + SO + H + 
              O + C + Sr + Br + 
              I + Fe, data = df[4:17])
summary(gam1)
gam.check(gam1, type=c("deviance","pearson","response"))

gam2 <- gam(Cl ~ Ca + Na + Mg + 
              K + SO + H + 
              O + C + Sr + Br + 
              I + Fe, data = df[4:17])
summary(gam2)
gam.check(gam2, type=c("deviance","pearson","response"))

AIC(mod_lm)
AIC(mod_lm1)



# Ref:
## http://www2.stat.unibo.it/montanari/Didattica/Multivariate/CA_lab.pdf
## http://cc.oulu.fi/~jarioksa/opetus/metodi/sessio3.pdf
## http://www2.stat.unibo.it/montanari/Didattica/Multivariate/PCA_lab1.pdf
## http://bioconductor.wustl.edu/bioc/vignettes/pcaMethods
## https://cran.r-project.org/web/packages/vegan/vignettes/intro-vegan.pdf
## http://cc.oulu.fi/~jarioksa/opetus/metodi/vegantutor.pdf
## https://www3.nd.edu/~mclark19/learn/GAMS.pdf



