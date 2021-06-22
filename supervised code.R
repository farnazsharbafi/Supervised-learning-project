install.packages("astrodatR")
library(astrodatR)
data('SDSS_ptsrc_train')
data('SDSS_ptsrc_test')

# Study the response variable

summary(SDSS_ptsrc_train)
hist(SDSS_ptsrc_train$Class , col ="violet", main="Distribution of the Classes", xlab = "Classes")

# Check the distributions

a <- rnorm(SDSS_ptsrc_train$u_g, mean = 0, sd = 1)
hist(a, breaks=20, freq=FALSE, col="antiquewhite1", xlab="u_g",main="u_g distribution")
xfit_ug<-seq(min(a),max(a),length=40)
yfit_ug <-dnorm(xfit_ug)
lines(xfit_ug, yfit_ug, col="darkorchid", lwd=2)

b <- rnorm(SDSS_ptsrc_train$g_r, mean = 0, sd = 1)
hist(b, breaks=20, freq=FALSE, col="antiquewhite1", xlab="g_r",main="g_r distribution")
xfit_gr <-seq(min(b),max(b),length=40)
yfit_gr <-dnorm(xfit_gr)
lines(xfit_gr, yfit_gr, col="darkorchid", lwd=2)

c <- rnorm(SDSS_ptsrc_train$r_i, mean = 0, sd = 1)
hist(c, breaks=20, freq=FALSE, col="antiquewhite1", xlab="r_i",main="r_i distribution")
xfit_ri <-seq(min(c),max(c),length=40)
yfit_ri <-dnorm(xfit_ri)
lines(xfit_ri, yfit_ri, col="darkorchid", lwd=2)

d <- rnorm(SDSS_ptsrc_train$i_z, mean = 0, sd = 1)
hist(d, breaks=20, freq=FALSE, col="antiquewhite1", xlab="i_z",main="i_z distribution")
xfit_iz <-seq(min(d),max(d),length=40)
yfit_iz <-dnorm(xfit_iz)
lines(xfit_iz, yfit_iz, col="darkorchid", lwd=2)

# Study the correlation
install.packages("Hmisc")
library("Hmisc")
cormatrix <- round(cor(SDSS_ptsrc_train, method = c("spearman")), 3)
col <- colorRampPalette(c("darkslategray1", "deepskyblue4", "black"))(20)
heatmap(x = cormatrix, col = col, symm = TRUE, Colv = NA, Rowv = NA, cexRow=0.8, cexCol = 0.8, main="Correlation between colors")

# Check outliers
boxplot(SDSS_ptsrc_train[, -5], col= "antiquewhite")

## -- Linear Discriminant Analysis
install.packages("MASS")
library(MASS)
lda_model <- lda(SDSS_ptsrc_train$Class~.,data = SDSS_ptsrc_train)
lda_model
pairs(lda_model, col="deepskyblue4")

install.packages("caret")
install.packages('e1071', dependencies=TRUE)
library(caret)
lda_validation <- SDSS_ptsrc_train
origin_validation <- SDSS_ptsrc_train
lda_validation[,5] <- predict(lda_model, newdata = origin_validation)$class
confusionMatrix(data= as.factor(lda_validation$Class), reference= as.factor(origin_validation$Class))
table(lda_validation$Class, origin_validation$Class)

# -- Decision Tree
install.packages("randomForest")
library("randomForest")

tree_validation <- SDSS_ptsrc_train
origin_validation_tree <- SDSS_ptsrc_train
tree_validation[,5] <- as.factor(tree_validation[,5])
tree_model <- randomForest(tree_validation$Class ~ ., data=tree_validation, ntree=100, mtry=3, importance=TRUE)
tree_model
tree_validation[,5] <- predict(tree_model, tree_validation, type = "class")
confusionMatrix(data= as.factor(tree_validation[,5]), reference= as.factor(origin_validation_tree$Class))

# Classification of the test observations
vec <- 1:12884
SDSS_ptsrc_test$new_col <- vec
colnames(SDSS_ptsrc_test) <- c("u_g", "g_r", "r_i", "i_z", "Class")
SDSS_ptsrc_test[,5] <- predict(tree_model, SDSS_ptsrc_test, type = "class")
View(SDSS_ptsrc_test)
