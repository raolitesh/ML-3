# codes are written in R studio
```r
# Read the dataset
library(readr)
glass<- read.csv(file.choose())
View(glass)
str(glass)
summary(glass)
```
```r
#converting output variable to factor
glass$Type <- factor(glass$Type)


#table of Glass type
table(glass$Type)
```
```r
# normalization function
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
  }
```
```r
#split the data into test and training

indv <- sample(2,nrow(glass),replace = T,prob = c(0.8,0.2))

glass_train <- glass[indv==1,]
glass_test <- glass[indv==2,]

glass_train_norm<- as.data.frame(lapply(glass_train[,-10],norm))
glass_test_norm <- as.data.frame(lapply(glass_test[-10], norm))

#Get labels for training and test datasets

glass_train_labels <- glass_train[,10]
glass_test_labels <- glass_test[,10]
```

```r
# Build a KNN model on taining dataset
library("class")
# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model
# we have to test on test dataset
glass_pred <- knn(train = glass_train_norm, test = glass_test_norm, cl = glass_train_labels, k=1)
```
```r
## Now evaluation of the model performance

# install package gmodels
install.packages("gmodels")
library("gmodels")

# Create cross table of predicted and actual

CrossTable( x =  glass_test_labels, y = glass_pred)
#accuracy with K= 1 is 46.34%

#model with k=10

glass_pred10 <- knn(train = glass_train_norm, test = glass_test_norm, 
                    cl = glass_train_labels,k=10)
CrossTable( x =  glass_test_labels, y = glass_pred10)
#accuracy with K= 10 is 56.09%

#model with k=20

glass_pred20 <- knn(train = glass_train_norm, test = glass_test_norm, 
                    cl = glass_train_labels, k=20)
CrossTable( x =  glass_test_labels, y = glass_pred20)
#accuracy with K= 20 is 61%


#model with k=25

glass_pred25 <- knn(train = glass_train_norm, test = glass_test_norm, 
                    cl = glass_train_labels, k=25)
CrossTable( x =  glass_test_labels, y = glass_pred25)
#accuracy with K= 25 is 70.73%

#model with k=30

glass_pred30 <- knn(train = glass_train_norm, test = glass_test_norm, 
                    cl = glass_train_labels, k=30)
CrossTable( x =  glass_test_labels, y = glass_pred30)
#accuracy with K= 30 is 70.73%

#model with k=35

glass_pred35 <- knn(train = glass_train_norm, test = glass_test_norm, 
                    cl = glass_train_labels, k=35)
CrossTable( x =  glass_test_labels, y = glass_pred35)
#accuracy with K= 35 is 63.41%

# as model with k value 25 and 30 are giving the highest and same accuracy we can stop at k= 25
```



