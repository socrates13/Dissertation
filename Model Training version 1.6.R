
#Model Training version 1.6 

#Name: Sokratis Dimitrios Chronopoulos
#Student ID: 201755818


# Input load. Please do not change #
`dataset` = read.csv('C:/Users/soc_x/REditorWrapper_c5186d63-559c-4985-82b4-8654ef12956e/input_df_e5a0227b-9eda-486d-a3ed-55063851ab6a.csv', check.names = FALSE, encoding = "UTF-8", blank.lines.skip = FALSE);

#Check if used packages have been installed in this R session

list.of.packages <- c("dplyr", "plyr", "reshape", "Amelia", "caTools", "e1071", "caret", "tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, force=FALSE)


library(dplyr)
library(plyr)
library(Amelia) # for missing values map
library(reshape)
library(caTools) # for sample.split
library(e1071)
library(caret)
library(tidyverse)
library(broom)

# Function for replacing NAs with modes

Mode <- function (x) {
xtab <- table(x)
xmode <- as.numeric(names(which(xtab == max(xtab))))
if (length(xmode) > 1) xmode <- ceiling(mean(xmode)) #if there are more than one mode, then return the average 
return(xmode)
}

#Setting the current directory as working directory


path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(path))

###### Exploratory Data Analysis  #######


#Produce a map with missing values vs observed in raw dataset

missmap(dataset, main = "Missing values vs observed")

#calculate NA values proportion per variable from dataframe (raw data): True=NAs, False=Non-NAs
for (i in 1:(length(dataset[-(1:5)]))){
  temp <-  plyr::count(is.na(dataset[i+5]))
  temp$freq<-(temp$freq/nrow(dataset))*100
  print(temp)
}

#Create a new dataframe composed of the events occured for each team per fixture

attach(dataset)
unique_features <- as.data.frame(unique(data.frame(FXID,Team,FXHTID,FXATID,HTFTSC,ATFTSC,FXHTResult, FXATResult)))
detach(dataset)

#Create and populate a new column called FXResult

unique_features$FXResult<-factor(NA,levels = c("L","W"), ordered = FALSE)

for (fixture in unique_features$FXID){
    for (team in unique_features$Team[unique_features$FXID==fixture]){
        if (Reduce("&",lapply(strsplit(team," ")[[1]],grepl,unique_features$FXHTID[unique_features$FXID==fixture & unique_features$Team==team]))){
          unique_features$FXResult[unique_features$FXID==fixture&unique_features$Team==team]<-unique_features$FXHTResult[unique_features$FXID==fixture & unique_features$Team==team]
          }
       else if (Reduce("&",lapply(strsplit(team," ")[[1]],grepl,unique_features$FXATID[unique_features$FXID==fixture & unique_features$Team==team]))){
              unique_features$FXResult[unique_features$FXID==fixture&unique_features$Team==team]<-unique_features$FXATResult[unique_features$FXID==fixture & unique_features$Team==team]
              }  
      }
}

#Print the proportions of W/L in terms of aggregation and Home and Away Teams Performance Ratio on the given dataset 

" for testing purpose / future use : inserts FXResult into original dataset

dataset$FXResult<-factor(NA,levels = c('L','W'), ordered = FALSE)
for (fixture in unique(dataset$FXID)){
for (team in unique(dataset$Team[dataset$FXID==fixture])){
dataset$FXResult[dataset$FXID==fixture&dataset$Team==team]<-unique_features$FXResult[unique_features$FXID==fixture&unique_features$Team==team]
}
}
"

"Create 6 levels with counts of different combinations occuring from the following variables from each team of the occured fixtures:

Level 1: count(Action Name, Action Type, Action Result, Qualifier 3, Qualifier 4, Qualifier 5)
Level 2: count(Action Name, Action Type, Action Result, Qualifier 3, Qualifier 4)
Level 3: count(Action Name, Action Type, Action Result, Qualifier 3)
Level 4: count(Action Name, Action Type, Action Result)
Level 5: count(Action Name, Action Type)
Level 6: count(Action Name)
"

#level1<-plyr::count(dataset, vars=c('FXID','FXHTID','Team','`Action Name`', '`Action Type`', '`Action Result`','`Qualifier 3`', '`Qualifier 4`', '`Qualifier 5`'))
#level2<-plyr::count(dataset, vars=c('FXID','FXHTID','Team','`Action Name`', '`Action Type`', '`Action Result`','`Qualifier 3`', '`Qualifier 4`'))
#level3<-plyr::count(dataset, vars=c('FXID','FXHTID','Team','`Action Name`', '`Action Type`', '`Action Result`','`Qualifier 3`'))
level4<-plyr::count(dataset, vars=c('FXID','FXHTID','Team','`Action Name`', '`Action Type`', '`Action Result`'))
level5<-plyr::count(dataset, vars=c('FXID','FXHTID','Team','`Action Name`', '`Action Type`'))
level6<-plyr::count(dataset, vars=c('FXID','FXHTID','Team','`Action Name`'))

#Now we are going to transform these matrices and merge the features to receive unique values of these combinations

#level1_final <- cast(level1, FXID+FXHTID+Team~Action.Name+Action.Type+Action.Result+Qualifier.3+Qualifier.4+Qualifier.5)
#level2_final <- cast(level2, FXID+FXHTID+Team~Action.Name+Action.Type+Action.Result+Qualifier.3+Qualifier.4)
#level3_final <- cast(level3, FXID+FXHTID+Team~Action.Name+Action.Type+Action.Result+Qualifier.3)
level4_final <- cast(level4, FXID+FXHTID+Team~Action.Name+Action.Type+Action.Result)
level5_final <- cast(level5, FXID+FXHTID+Team~Action.Name+Action.Type)
level6_final <- cast(level6, FXID+FXHTID+Team~Action.Name)

#levels_list <- list(level1_final,level2_final,level3_final,level4_final,level5_final,level6_final)
levels_list <- list(level4_final,level5_final,level6_final)


#Now, we can safely remove the individual dataframes we have created in prior to finalise the dataframe will be used for producing the correlations

rm(level1,level2,level3,level4,level5,level6)

rm(level1_final,level2_final,level3_final,level4_final,level5_final,level6_final)


#Here, we create the final dataframe named global_table that includes all the instances occurring for each team during the selected fixtures


global_table <- levels_list[[1]] #Fill in the table with the first level according to the order that levels have been inserted into teh list

for (i in 2:length(levels_list)) # Continue the fullfilment using a for-loop for the remaining levels
{
  global_table <- merge(global_table,levels_list[[i]],by= c('FXID','FXHTID','Team'))
  
}

#calculate NA values proportion per variable level from dataframe (global_table): True=NAs, False=Non-NAs


for (i in 1:(length(global_table[-(1:3)]))){
    temp <-  plyr::count(is.na(global_table[i+3]))
    temp$freq<-(temp$freq/nrow(global_table))*100
    print(temp)
}

#Create the dataframe with the tranformed data used to make the Machine Learning Model

global_table$FXResult<-factor(NA,levels = c("L","W"), ordered = FALSE)

for (fixture in global_table$FXID){
    for (team in global_table$Team[global_table$FXID==fixture]){
          if (Reduce("&",lapply(strsplit(team," ")[[1]],grepl,unique_features$FXHTID[unique_features$FXID==fixture & unique_features$Team==team]))){
          global_table$FXResult[global_table$FXID==fixture & global_table$Team==team]<-as.factor(unique_features$FXHTResult[unique_features$FXID==fixture])
          }
          else if (Reduce("&",lapply(strsplit(team," ")[[1]],grepl,unique_features$FXATID[unique_features$FXID==fixture & unique_features$Team==team]))){
          global_table$FXResult[global_table$FXID==fixture & global_table$Team==team]<-as.factor(unique_features$FXATResult[unique_features$FXID==fixture])
          }    
      }
}



write.csv(global_table,"Global Table.csv",row.names=FALSE) #Export the dataframe to a .csv file

#global_table <- read.csv("Global Table.csv")


#Create a  reduced global table removing all columns and rows including NAs in more than 40%

g_other<- global_table[, colSums(is.na(global_table)) < 0.4*(nrow(global_table)-1)] #columns

g_other<-g_other[!(rowSums(is.na(g_other))/ncol(g_other)>0.4),]

#Remove redudant columns

#(The 3-variable columns which end with 'NA' are equal to the 2-variable columns 
#that share the first two variables)
g_other<-select(g_other, -ends_with("NA"))

#Remove non-numeric columns (e.g., FXID)

g_other<-g_other[-(1:3)]


# Cleaning NAs using central tendency: Mode


for (var in 1:ncol(g_other)) {
  g_other[is.na(g_other[,var]),var] <- Mode(g_other[,var])
}


###### Machine Learning Implementation  #######



#Split data into training and test sets

set.seed(101) #for reproducibility
sample = sample.split(g_other$FXResult, SplitRatio = .75) #splitting data..
train = subset(g_other, sample == TRUE)  #Training set: 75%
test  = subset(g_other, sample == FALSE) #Test set: 25%


#Fit a logistic model with no variables accounted for this model: Null model


model_0<- glm(FXResult~ 1, data=train, family="binomial")

#Fit a logistic model with all data from all variables: Full model

model_full <- glm(FXResult~. , data=train, family="binomial")



#Find the best model using stepwise selection

stepwise = step(model_0,
scope=list(lower=formula(model_0),upper=formula(model_full)), direction="both")

#Remove the intercept from the model -> there is no chance to Win if all variables are equal to 0

stepwise <- update(stepwise, .~. -1)

#save model parameters in a .csv file

tidy_stepwise<- tidy(stepwise)

write.csv(tidy_stepwise,"Model 1.csv",row.names=FALSE)

#Save odds ratios into a file

odd_ratios_stepwise<-data.frame(c(round(exp(stepwise$coefficients),4)))

lower.ci <- round(odd_ratios_stepwise*exp(-1.96*tidy_stepwise$std.error),4)
upper.ci<- round(odd_ratios_stepwise*exp(1.96*tidy_stepwise$std.error),4)

odd_ratios_stepwise <-cbind(odd_ratios_stepwise,lower.ci, upper.ci)
colnames(odd_ratios_stepwise)  <- c("OR", "Lower 95% CI", "Upper 95% CI")
write.csv(odd_ratios_stepwise,"Odds ratios (Classifier 1).csv",row.names=TRUE)

#Save the model for future use (apply to a dataset of unseen observations)

saveRDS(stepwise, file = "lg1.RDS")


# Evaluate the model using the training set

fitted_probs <- predict(stepwise,newdata=train,type='response')
fitted_results <- ifelse(fitted_probs > 0.5,"W","L")

fitted_results <- factor(fitted_results,levels = c("L","W"), ordered = FALSE)



misClasificError_train <- mean(fitted_results != train$FXResult)


confusionMatrix(fitted_results, train$FXResult, positive="W")


# Creating performance object (sources:http://scaryscientist.blogspot.com/2016/03/roc-receiver-operating-characteristics.html, https://github.com/chupvl/R_scripts/blob/master/rocr_wLegend.R accessed: 14/03/2019)

library("ROCR")

#setting visual parameters
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)

par(mfrow=c(1,2)) #Illustrate 2 plots side by side

# calculating the values for ROC curve 

perf.obj_train <- prediction(predictions=fitted_probs, labels=train$FXResult)
                             
# Get data for ROC curve
roc.obj_train <- performance(perf.obj_train, measure="tpr", x.measure="fpr")
plot(roc.obj_train,
     main="Classifier 1 (training) - ROC Curve",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="blue")
abline(0,1,col="grey")

# Get AUC value from ROC curve

auc <- performance(perf.obj_train, measure = "auc")
auc <- auc@y.values[[1]]

# adding ROC AUC to the center of the plot

maxauc<-max(round(auc, digits = 2))

maxauct <- paste(c("AUC = "),maxauc,sep="")
legend(0.37,0.34, c(maxauct),border="white",cex=1.33,box.col = "white")


#Evaluate the model using the test set

#Predict likelihood of outcome = "W" and convert them to the predicted outcome 

predicted_probs <- predict(stepwise,newdata=test,type='response')
predicted_results <- ifelse(predicted_probs > 0.5,"W","L")

predicted_results <- factor(predicted_results,levels = c("L","W"), ordered = FALSE)


table(test$FXResult, predicted_results)

misClasificError_pred <- mean(predicted_results != test$FXResult)
print(paste('Accuracy of logistic regression model (test dataset):',1-misClasificError_pred))

confusionMatrix(predicted_results, test$FXResult, positive="W")

#Create a ROC curve

perf.obj_test <- prediction(predictions=predicted_probs, labels=test$FXResult)
                            
# Get data for ROC curve
roc.obj_test <- performance(perf.obj_test, measure="tpr", x.measure="fpr")
plot(roc.obj_test,
     main="Classifier 1 (test) - ROC Curve",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="blue")
abline(0,1,col="grey")

# Get AUC value from ROC curve

auc_test <- performance(perf.obj_test, measure = "auc")

# now converting S4 class to vector
auc_test <- unlist(slot(auc_test, "y.values"))
# adding min and max ROC AUC to the center of the plot

maxauc_test<-max(round(auc_test, digits = 2))

maxauct_test <- paste(c("AUC = "),maxauc_test,sep="")
legend(0.37,0.34, maxauct_test,border="white",cex=1.33,box.col = "white")


###Model optimisation###


# Assess the deviance of variables to evaluate the use/non-use of a variable into the model (goodness-of-fit) 

stepwise.anova <-anova(stepwise, test="Chisq")


#Update the model using the variables that are statistically significant (Pr(>|z|)<=0.05) and lower the deviance significantly (Pr(>Chi)<=0.05)

qualified_variables <- names(stepwise$coefficients[which(tidy_stepwise$term[which(tidy_stepwise$p.value<0.05)] %in% rownames(stepwise.anova)[which(stepwise.anova$`Pr(>Chi)`<0.05)])]) #eliminated variables


updated_formula <- as.formula(paste(".~", paste(qualified_variables, collapse="+")))


model_updated <-update(stepwise, updated_formula)

#compare the two models running a chi-squared test


models.anova=anova(model_updated, stepwise, test="Chisq")

#save model parameters in a .csv file

tidy_stepwise<- tidy(stepwise)

write.csv(tidy_stepwise,"Model 1.csv",row.names=FALSE)

#Save odds ratios into a file

odd_ratios_stepwise<-data.frame(c(round(exp(stepwise$coefficients),4)))

lower.ci <- round(odd_ratios_stepwise*exp(-1.96*tidy_stepwise$std.error),4)
upper.ci<- round(odd_ratios_stepwise*exp(1.96*tidy_stepwise$std.error),4)

odd_ratios_stepwise <-cbind(odd_ratios_stepwise,lower.ci, upper.ci)
colnames(odd_ratios_stepwise)  <- c("OR", "Lower 95% CI", "Upper 95% CI")

write.csv(odd_ratios_stepwise,"Table of Odds ratio (Classifier 1).csv",row.names=TRUE)

# Evaluate the model using the training set

fitted_probs <- predict(model_updated,newdata=train,type='response')
fitted_results <- ifelse(fitted_probs > 0.5,"W","L")

fitted_results <- factor(fitted_results,levels = c("L","W"), ordered = FALSE)



misClasificError_train <- mean(fitted_results != train$FXResult)


confusionMatrix(fitted_results, train$FXResult, positive="W")


# Creating performance object 

#setting visual parameters
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)

par(mfrow=c(1,2)) #Illustrate 2 plots side by side

# calculating the values for ROC curve 

perf.obj_train <- prediction(predictions=fitted_probs, labels=train$FXResult)

# Get data for ROC curve
roc.obj_train <- performance(perf.obj_train, measure="tpr", x.measure="fpr")
plot(roc.obj_train,
     main="Classifier 2 (train) - ROC Curve",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="blue")
abline(0,1,col="grey")

# Get AUC value from ROC curve

auc <- performance(perf.obj_train, measure = "auc")
auc <- auc@y.values[[1]]

# adding ROC AUC to the center of the plot

maxauc<-max(round(auc, digits = 2))

maxauct <- paste(c("AUC = "),maxauc,sep="")
legend(0.37,0.34, c(maxauct),border="white",cex=1.33,box.col = "white")


#save model parameters in a .csv file

tidy_model_updated<- tidy(model_updated)

write.csv(tidy_model_updated,"Model 2.csv",row.names=FALSE)

#Save odds ratios into a file


odd_ratios_model_updated<-data.frame(c(round(exp(model_updated$coefficients),4)))

lower.ci <- round(odd_ratios_model_updated*exp(-1.96*tidy_model_updated$std.error),4)
upper.ci<- round(odd_ratios_model_updated*exp(1.96*tidy_model_updated$std.error),4)

odd_ratios_model_updated <-cbind(odd_ratios_model_updated,lower.ci, upper.ci)
colnames(odd_ratios_model_updated)  <- c("OR", "Lower 95% CI", "Upper 95% CI")

write.csv(odd_ratios_model_updated,"Table of Odds ratio (Classifier 2).csv",row.names=TRUE)


#Evaluate the model using the test set

#Predict likelihood of outcome = "W" and convert them to the predicted outcome 

predicted_probs <- predict(model_updated,newdata=test,type='response')
predicted_results <- ifelse(predicted_probs > 0.5,"W","L")

predicted_results <- factor(predicted_results,levels = c("L","W"), ordered = FALSE)


table(test$FXResult, predicted_results)

misClasificError_pred <- mean(predicted_results != test$FXResult)
print(paste('Accuracy of logistic regression model (test dataset):',1-misClasificError_pred))

confusionMatrix(predicted_results, test$FXResult, positive="W")

#Create a ROC curve

perf.obj_test <- prediction(predictions=predicted_probs, labels=test$FXResult)

# Get data for ROC curve
roc.obj_test <- performance(perf.obj_test, measure="tpr", x.measure="fpr")
plot(roc.obj_test,
     main="Classifier 2 (test) - ROC Curve",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="blue")
abline(0,1,col="grey")

# Get AUC value from ROC curve

auc_test <- performance(perf.obj_test, measure = "auc")

# now converting S4 class to vector
auc_test <- unlist(slot(auc_test, "y.values"))

# adding ROC AUC to the center of the plot

maxauc_test<-max(round(auc_test, digits = 2))

maxauct_test <- paste(c("AUC = "),maxauc_test,sep="")
legend(0.37,0.34, maxauct_test,border="white",cex=1.33,box.col = "white")

saveRDS(model_updated, file = "lg2.RDS")


########-----End of Script#####