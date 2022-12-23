############PART_1###################
------------------------------------------
#IMPORT REQUIERED LIBRARAIES
library(readr)
library(ggplot2)
library(dplyr)
#library(tidyr)
library(corrplot)
library(caret)
library(rms)
library(MASS)
library(e1071)
library(ROCR)
library(gplots)
library(pROC)
library(rpart)
library(ggpubr)
library(ggplot2)
library(caret)
library(pROC)

'--------------------------------------
'
#IMPORTING THE DATASET

datta<- read.csv("D:/uottowa/applied DS/2/Assignment 2/Churn Dataset.csv")

set.seed(100)
str(datta)

summary(datta)



#######################################################################################

#scatter plot

library(corrplot)
# 
# datta_clean %>%
#   dplyr::select (TotalCharges, MonthlyCharges, tenure) %>%
#   cor() %>%
#   corrplot.mixed(upper = "circle", tl.col = "black", number.cex = 0.7)


reduced_data = subset(datta,col = c("red","blue"),select = c("tenure","MonthlyCharges","TotalCharges"))
pairs(reduced_data,pch = 19)


#######################################################################################


##heat map


install.packages("corrplot")
source("http://www.sthda.com/upload/rquery_cormat.r") # to use rquery.cormat
rquery.cormat(datta[c(3,6,19,20)])
cormat<-rquery.cormat(datta[c(3,6,19,20)], graphType="heatmap")
'--------------------------------------------'


'----------------------------------------'
#checking missing values


sapply(datta, function(x) sum(is.na(x)))




library(dplyr)

datta %>%
  summarise_all(
    funs(sum(is.na(.)))
  ) %>%
  tidyr::gather(ColumnTitle, NAs, customerID:Churn)

#visualize missing value
options(repr.plot.width = 6, repr.plot.height = 4)
missing_data <- datta %>% summarise_all(funs(sum(is.na(.))/n()))
missing_data <- tidyr::gather(missing_data, key = "variables", value = "percent_missing")
ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "red", aes(color = I('white')), size = 0.3)+
  xlab('variables')+
  coord_flip()+ 
  theme_bw()

#report -> Total charges have 11 NA values.which suggests that they are new clients and their total charges are zeroes. 
#So, all the NA values will be replaced by zeroes.


'--------------------------------------------'

#VISUALIZING THE CATEGORICAL DATA FIRST WITH RESPECT TO CHURN:

print("Percentage of customers churn")
prop.table(table(datta$Churn))

#From the table above we notice that 73.4% of the customers did not churn. 
#This can server as our baseline model i.e. if we predict every customer to not churn we will be right on average 73.4% of the times. 


options(repr.plot.width = 6, repr.plot.height = 4)
datta %>% 
  group_by(Churn) %>% 
  summarise(Count = n())%>% 
  mutate(percent = prop.table(Count)*100)%>%
  ggplot(aes(reorder(Churn, -percent), percent), fill = Churn)+
  geom_col(fill = c("#FC4E07", "#E7B800"))+
  geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.01,vjust = -0.5, size =3)+ 
  theme_bw()+  
  xlab("Churn") + 
  ylab("Percent")+
  ggtitle("Churn Percent")


# Analyzing the three continuous variables to CHURN:

options(repr.plot.width =6, repr.plot.height = 2)
ggplot(datta, aes(y= tenure, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")


ggplot(datta, aes(y= MonthlyCharges, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")



ggplot(datta, aes(y= TotalCharges, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")

#######################################################################
# convert to numeric data
datta$gender <- ifelse(datta$gender == "Male",1,0)
datta$Partner <- ifelse(datta$Partner == "Yes",1,0)
datta$Dependents <- ifelse(datta$Dependents == "Yes",1,0)
datta$PhoneService <- ifelse(datta$PhoneService == "Yes",1,0)
datta$MultipleLines <- ifelse(datta$MultipleLines == "Yes",1,ifelse(datta$MultipleLines == "No",0, -1))
datta$InternetService <- ifelse(datta$InternetService == "No",0,ifelse(datta$InternetService == "DSL",1, 2))
datta$OnlineSecurity <- ifelse(datta$OnlineSecurity == "No",0,ifelse(datta$OnlineSecurity == "Yes",1, -1))
datta$OnlineBackup <- ifelse(datta$OnlineBackup == "No",0,ifelse(datta$OnlineBackup == "Yes",1, -1))
datta$DeviceProtection <- ifelse(datta$DeviceProtection == "No",0,ifelse(datta$DeviceProtection == "Yes",1, -1))
datta$TechSupport <- ifelse(datta$TechSupport == "No",0,ifelse(datta$TechSupport == "Yes",1, -1))
datta$StreamingTV <- ifelse(datta$StreamingTV == "No",0,ifelse(datta$StreamingTV == "Yes",1, -1))
datta$StreamingMovies <- ifelse(datta$StreamingMovies == "No",0,ifelse(datta$StreamingMovies == "Yes",1, -1))
datta$PaperlessBilling <- ifelse(datta$PaperlessBilling == "No",0,ifelse(datta$PaperlessBilling == "Yes",1, -1))
datta$Churn <- ifelse(datta$Churn == "No",0,ifelse(datta$Churn == "Yes",1, -1))
datta$Contract <- ifelse(datta$Contract == "Month-to-month",0,ifelse(datta$Contract == "One year",1, 2))
datta$PaymentMethod <- ifelse(datta$PaymentMethod == "Bank transfer (automatic)" | datta$PaymentMethod == "Credit card (automatic)",2,
                              ifelse(datta$PaymentMethod == "Electronic check",1, 0))

head(datta)


#drop the first column
datta <- subset (datta, select = -customerID)

############################################################

install.packages("corrplot")

library(corrplot)
library(reshape2) #to meld the data frame
library(ggplot2)

corrplot(cor(datta[,1:20]),        # Correlation matrix
         method = "circle",                # Correlation plot method (method = number, circle, pie, or color)
         type = "full",                   # Correlation plot style (also "upper" and "lower")
         diag = TRUE,                     # If TRUE (default), adds the diagonal
         tl.col = "black",                # Labels color
         bg = "white",                    # Background color
         title = "",                      # Main title
         col = NULL,                      # Color palette
         tl.cex =0.7,
         cl.ratio =0.2)                            
###########################################################################


#Split the dataset into 80 training/20 test set and fit a decision tree to the training data.

# install.packages("caret")
# install.packages("numDeriv")
library(caret)
set.seed(123)
split_train_test <- createDataPartition(datta$Churn,p=0.8,list=FALSE)
dtrain<- datta[split_train_test,]
dtest<-  datta[-split_train_test,]

# Remove Total Charges from the training dataset

# dtrain <- dtrain[,-19]
# dtest <- dtest[,-19]

# install.packages("rpart.plot")
library(rpart.plot)

#decision tree
tree_fit <- rpart(Churn ~., data = dtrain, method="class")

rpart.plot(tree_fit)

'--------------------------------------------------'
#build a decision tree
model_tree <- rpart(Churn ~., data = dtrain, method="class")
pred_tree = predict(model_tree, newdata = dtest , type="class")
confusionMatrix(as.factor(dtest$Churn),pred_tree)

rpart.plot(model_tree , extra= 106)


#decision tree using information gain

tree_ig = rpart(Churn ~., data = dtrain, method="class",parms = list(split = "information"))
pred_ig = predict(tree_ig,newdata = dtest , type="class")
confusionMatrix(as.factor(dtest$Churn),pred_ig)

rpart.plot(tree_ig , extra= 106)


library(pROC)
library(readxl)
#ROC FOR DT
roc(as.numeric(as.factor(dtest$Churn)),
    as.numeric(c(as.numeric(pred_tree))),plot=TRUE,direction = "<",
    percent = TRUE , legacy.axes = TRUE,xlab ="false positive",ylab ="true positive")

####################################################################
Model <- rpart(Churn ~ ., data = dtrain, method = "class", 
               control = rpart.control(cp = 0))
summary(Model)
rpart.plot(Model)
# Compute the accuracy of the pruned tree
dtest$pred <- predict(Model, dtest, type = "class")
base_accuracy <- mean(dtest$pred == dtest$Churn)
confusionMatrix(as.factor(dtest$Churn),dtest$pred)



#prepurning
# Grow a tree with minsplit of 100 and max depth of 8
model_preprun <- rpart(Churn ~ ., data = dtrain, method = "class", 
                       control = rpart.control(cp = 0, maxdepth = 8,minsplit = 100))
# Compute the accuracy of the pruned tree
dtest$pred <- predict(model_preprun, dtest, type = "class")
accuracy_preprun <- mean(dtest$pred == dtest$Churn)


#Postpruning
# Prune the Hr_Model based on the optimal cp value
model_pruned <- prune(Model, cp = 0.0084 )
# Compute the accuracy of the pruned tree
dtest$pred <- predict(model_pruned, dtest, type = "class")
accuracy_postprun <- mean(dtest$pred == dtest$Churn)
data.frame(base_accuracy, accuracy_preprun, accuracy_postprun)



'-------------------------------------------------------------'

#Classify the data using the XGBoost model with nrounds = 70 and max depth = 3.


#####################################################################
library(xgboost)
library(caret)  
library(e1071) 


# createDataPartition() function from the caret package to split the original dataset into a training and testing set and split data into training (80%) and testing set (20%)
dt = createDataPartition(datta$Churn, p = 0.7, list = F)
train = datta[dt, ]
test = datta[-dt, ]

X_train = data.matrix(train[,1:19])                  # independent variables for train
y_train = train[,20]                                # dependent variables for train

X_test = data.matrix(test[,1:19])                    # independent variables for test
y_test = test[,20]                                   # dependent variables for test



#Classify the data using the XGBoost model 
rams = list(
  max.depth = 3, 
  eta = 1,
  subsample=1,
  nthread = 2,
  set.seed=250,
  booster='gbtree'
)

xgboost <- xgboost(data = as.matrix(X_train),
                   label = y_train ,
                   nrounds = 70, 
                   params = rams ,
                   verbose = 1)

#use model to make predictions on test data


pred_test =  predict(xgboost , newdata =as.matrix(X_test) )

pred_test = ifelse(pred_test>.5 , 1 , 0)

confusionMatrix(factor(pred_test, levels = c(0,1)), factor(y_test, levels = c(0,1)) , mode= 'everything')

#use model to make predictions on train data
pred_train =  predict(xgboost , newdata =as.matrix(X_train) )

pred_train = ifelse(pred_train>.5 , 1 , 0)

confusionMatrix(factor(pred_train, levels = c(0,1)), factor(y_train, levels = c(0,1)) , mode= 'everything')



#ROC FOR xgboost
roc(as.numeric(as.factor(dtest$Churn)),
    as.numeric(c(as.numeric(pred_test))),plot=TRUE,direction = "<",
    percent = TRUE , legacy.axes = TRUE,xlab ="False positive rate",ylab ="True positive rate")








########################KERAS##############################
# install.packages("keras")
# install.packages("tensorflow")
# library(keras)
# library(tidyverse)
# library(tensorflow)
# use_condaenv("r-tensorflow")
# install_keras(method="virtualenv", envname="myenv", pip_options = "--no-cache-dir")
# install_tensorflow(method="virtualenv", envname="myenv", pip_options = "--no-cache-dir")


install.packages("devtools")

devtools::install_github("rstudio/keras")
devtools::install_github("rstudio/reticulate")

install.packages("tensorflow")

#Note that on Windows you need a working installation of Anaconda.
library(tensorflow)
reticulate::use_python("C:/Users/mm/anaconda3/Lib/site-packages/tensorflow/python")
install_tensorflow()
install_tensorflow(gpu=TRUE)

#loading keras library
library(keras)




de_train = as.matrix(dtrain)
de_test = as.matrix(dtest)
dimnames(de_train) = NULL

model <- keras_model_sequential()

model %>%
  layer_dense(units = 16, activation = 'relu', input_shape = 19, kernel_initializer = "uniform",) %>% 
  layer_dropout(rate = 0.1) %>% 
  layer_dense(units = 8, activation = 'tanh', kernel_initializer = "uniform",) %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 1, activation = 'sigmoid', kernel_initializer = "uniform",)



model %>% compile(
  loss      = 'mean_squared_error',
  optimizer = 'adam',
  metrics = c('accuracy')
)

history <- model %>% fit(de_train[,1:19], de_train[,20], epochs = 10, batch_size = 128)

testPred <- predict_classes(model, de_test[,1:19], batch_size = 32, verbose = 1)
levels(testpred)
confusionMatrix(factor(testPred), factor(de_test[,20]), positive="1")

pred <- prediction(testPred, de_test[,20])
perf <- performance(pred,"tpr","fpr")
plot(perf,col ="yellow", add = TRUE)

legend("topleft",
       c("DT","Xgboost", "NN"),
       fill=c("blue","red", "yellow",
              cex = 0.5),)



#ROC FOR DT
roc(as.numeric(as.factor(de_test$Churn)),
    as.numeric(c(as.numeric(pred))),plot=TRUE,direction = "<",
    percent = TRUE , legacy.axes = TRUE,xlab ="Specivity",ylab ="sensativity")












#######################PART_2###############################
# install.packages("arules")
library(arules)
# install and load arulesViz
# install.packages("arulesViz")
library(arulesViz)



trans <- read.transactions('D:/uottowa/applied DS/2/Assignment 2/transactions.csv', sep=',')



summary(trans)


# Create an item frequency plot for the top 10 items
if (!require("RColorBrewer")) {
  # install color package of R
  install.packages("RColorBrewer")
  #include library RColorBrewer
  library(RColorBrewer)
}

frequentItems <- eclat (trans, parameter = list(supp = 0.07, maxlen = 15)) # calculates support for frequent items
inspect(frequentItems)

itemFrequencyPlot(trans,topN=10,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot") 


# Min Support as 0.002,, confidence as 0.20,maxlen=3
association.rules <- apriori(trans, parameter = list(supp=0.002, conf=0.20,maxlen=3))

rules_lift_1 <- sort (association.rules, by="lift", decreasing=TRUE) # 'high-lift' rules.
inspect(head(rules_lift_1)) # show the support, lift and confidence for all rules


# Min Support as 0.002,, confidence as 0.20,maxlen=2
association.rules <- apriori(trans, parameter = list(supp=0.002, conf=0.20,maxlen=2))

rules_lift_2 <- sort (association.rules, by="lift", decreasing=TRUE) # 'high-lift' rules.
inspect(head(rules_lift_2)) # show the support, lift and confidence for all rules

