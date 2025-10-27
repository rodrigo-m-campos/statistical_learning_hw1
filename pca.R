data("iris")
data = iris
library(GGally)
library(factoextra)

# Check missing values
hist(rowMeans(is.na(data)))
barplot(colMeans(is.na(data)), las=2)

# Look at data
boxplot(data, las=2, col="darkblue")

data_num = data[,-c(5)]

# Scale
boxplot(scale(data_num), las = 2, col = "darkblue")

# See correlation
R = cor(data_num)
ggcorr(data_num, label = T)

# PCA time!
pca = prcomp(data_num, scale=T)
summary(pca)

# EIGENVALUES!
eigen(R)

# How many components do we take?
screeplot(pca,main="Screeplot",col="blue",type="barplot",pch=19)

# Same thing with the other package
fviz_screeplot(pca, addlabels = TRUE)



