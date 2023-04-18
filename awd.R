## Load libraries
library(tidyverse)
library(skimr)      # skimming data frames
library(ggthemes)
library(patchwork)  # combine separate ggplots into the same graphic
library(corrplot)
library(dplyr)
library(ellipse)
library(free)
library(densitr)
library(rsample)    # initial_split()
library(DescTools)  # PseudoR2()
library(sjPlot)     # tab_model(), print regression models as HTML table
library(caret)      # confusionMatrix()
library(mlr)        # Machine Learning in R (for SVM)
library(rpart)      # Recursive Partitioning and Regression Trees
library(rpart.plot)
library(ranger)     # Random forest
library(lightgbm)   # LightGBM (GBDT: gradient boosting decision tree)
library(psych) #statistics
library(pander)
library(CatEncoders) #Data wrangling/transformation
library(kernlab)
library(randomForest)

## Load data
df <- read_csv("D:/My R projects/Digital_health/apple watch data/aw_fb_data.csv")

#Summarize data

head(df, 18) #first 5 rows of the dataset
dim(df) #dimension of the dataset
data.frame(colnames(df)) #lists names of the column 
sapply(df, class) # list types for each attribute
summary(df) #summary of the dataset

#Data wrangling

#define list of columns to remove
remove_cols <- c('device', 'Unnamed: 0','X1', 'age', 'gender','entropy_heart', 'entropy_setps','corr_heart_steps', 'norm_heart', 'intensity_karvonen','sd_norm_heart', 'steps_times_distance','hear_rate')

#remove columns in list
new_df = subset(df, select = !(names(df) %in% remove_cols)) 

#view updated data frame

new_df
dim(new_df)
sapply(new_df, class)
summary(new_df)

mycorrelations <- cor(new_df[1:7], use="pairwise.complete.obs" ) #Excluding the activity column
mycorrelations %>%
  pander()
unique(new_df['activity'])

# variable to encode values
ac_data = new_df$activity
lab_enc=LabelEncoder.fit(ac_data)
new_ac_data= transform(lab_enc, ac_data)
head(new_ac_data)
head(ac_data) #verify the transformed data

new_df$activity = new_ac_data
unique(new_df$activity)

ac_corr <- cor(new_df)
ac_corr %>%
  pander()




## Machine learning Taining model

validation_index <- createDataPartition(new_df$activity, p = 0.8, list = FALSE, times = 1)
# select 20% of the data for validation
validation <- new_df[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- new_df[validation_index,]



head(dataset, 18) #first 5 rows of the dataset
dim(dataset) #dimension of the dataset
data.frame(colnames(dataset)) #lists names of the column 
sapply(dataset, class) # list types for each attribute
summary(dataset) #summary of the dataset

dataset$activity <- as.numeric(dataset$activity)
sapply(dataset, class)



#Data Visualization

# split input and output
x <- dataset[,1:7]
y <- dataset[,8]

# boxplot for each attribute on one image
par(mfrow=c(1,7))
par(mar=c(1,1,1,1))
for(i in 1:7) {boxplot(x[,i], main=names(dataset$activity)[i])}

# barplot for class breakdown
plot(y)

# scatterplot matrix
dev.off()
featurePlot(x=x, y= as.factor(dataset$activity), plot="ellipse")

# box and whisker plots for each attribute

par(mar=c(1,1,1,1))

featurePlot(x=x, y=as.factor(dataset$activity), plot="box")

# density plots for each attribute by class value
ggplot(dataset, aes(x = calories)) + 
  geom_density(aes(color = as.factor(activity)))

#Run algorithms


# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
formula <- as.formula(activity ~ .)


# a) linear algorithms

set.seed(7)
fit.glm <- caret::train(formula, data = new_df, method = "glm", trControl=control)



# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- caret::train(formula, data= new_df, method="rpart", trControl=control)
# kNN
set.seed(7)
fit.knn <- caret::train(formula, data= new_df, method="knn", trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- caret::train(formula, data= new_df, method="svmRadial", trControl=control)

# Random Forest
set.seed(7)
fit.rf <- caret::train(formula, data= new_df, method="rf", trControl=control)


# summarize accuracy of models
results <- resamples(list(glm=fit.glm, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

# compare accuracy of models
dotplot(results)

# summarize Best Model
print(fit.glm)
