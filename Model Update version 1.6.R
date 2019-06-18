
#Model Update and Prediction version 1.6 

#Name: Sokratis-Dimitrios Chronopoulos
#Student ID: 201755818

# Input load. Please do not change #
`dataset` = read.csv('C:/Users/soc_x/REditorWrapper_2329782c-7da8-484a-97cc-127b342b25be/input_df_fb48a2b1-015b-41e2-9bff-826ac10eece5.csv', check.names = FALSE, encoding = "UTF-8", blank.lines.skip = FALSE);
# Original Script. Please update your script content here and once completed copy below section back to the original editing window #

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



write.csv(global_table,"C:/Users/soc_x/Documents/Strathclyde Business School/Semester B/Dissertation/Global Table.csv",row.names=FALSE) #Export the dataframe to a .csv file

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


#Load the created model from previous training

classifier<-readRDS(file = "C:/Users/soc_x/Documents/Strathclyde Business School/Semester B/Dissertation/lg1.RDS")

#Update the model using the variables that exist in both already created model and the new dataset (after cleaning and preprocessing)

deleted_columns <-names(classifier$coefficients[which(!names(classifier$model) %in% names(g_other))]) #eliminated variables
deleted_columns


updated_formula <- as.formula(paste(".~. -", paste(deleted_columns, collapse="-"))) #create the updated formula removing the elimanated variables from the model


model_updated <- update(classifier, updated_formula, data=model.frame(classifier)) #update the model with the updated formula


# Evaluate the model using the predicting set

pred_probs <- predict(model_updated,newdata=g_other,type='response') #the probability of a observation belongs to "W"
pred_results <- ifelse(pred_probs > 0.5,"W","L") #apply the threshold to obtain the predicted class

pred_results <- factor(pred_results,levels = c("L","W"), ordered = FALSE)


table(g_other$FXResult, pred_results) #confusion matrix

misClasificError_g_other <- mean(pred_results != g_other$FXResult)
print(paste('Accuracy of logistic regression model (g_other dataset):',1-misClasificError_g_other))

# Creating performance object (sources:http://scaryscientist.blogspot.com/2016/03/roc-receiver-operating-characteristics.html, https://github.com/chupvl/R_scripts/blob/master/rocr_wLegend.R accessed: 14/03/2019)

library("ROCR")

#setting visual parameters

par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)

par(mfrow=c(1,2)) # displays two plots side by side


perf.obj_g_other <- prediction(predictions=pred_probs,labels=g_other$FXResult)

# Get data for ROC curve
roc.obj_g_other <- performance(perf.obj_g_other, measure="tpr", x.measure="fpr")
plot(roc.obj_g_other,
     main="Classifier 1 (Prediction) - ROC Curve",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="blue")
abline(0,1,col="grey")

# Get AUC value from ROC curve

auc <- performance(perf.obj_g_other, measure = "auc")
auc <- auc@y.values[[1]]

# adding ROC AUC to the center of the plot

maxauc<-max(round(auc, digits = 2))

maxauct <- paste(c("AUC = "),maxauc,sep="")
legend(0.37,0.34, c(maxauct),border="white",cex=1.33,box.col = "white")

classifier2<-readRDS(file = "C:/Users/soc_x/Documents/Strathclyde Business School/Semester B/Dissertation/lg2.RDS")


#Update the model using the variables that exist in both already created model and the new dataset (after cleaning and preprocessing)

deleted_columns <-names(classifier2$coefficients[which(!names(classifier2$model) %in% names(g_other))]) #eliminated variables


updated_formula <- as.formula(paste(".~. -", paste(deleted_columns, collapse="-"))) #create the updated formula removing the elimanated variables from the model


model_updated <- update(classifier2, updated_formula, data=model.frame(classifier2)) #update the model with the updated formula


# Evaluate the model using the predicting set

pred_probs <- predict(model_updated,newdata=g_other,type='response') #the probability of a observation belongs to "W"
pred_results <- ifelse(pred_probs > 0.5,"W","L") #apply the threshold to obtain the predicted class

pred_results <- factor(pred_results,levels = c("L","W"), ordered = FALSE)


table(g_other$FXResult, pred_results) #confusion matrix

misClasificError_g_other <- mean(pred_results != g_other$FXResult)
print(paste('Accuracy of logistic regression model (g_other dataset):',1-misClasificError_g_other))

# Creating performance object (sources:http://scaryscientist.blogspot.com/2016/03/roc-receiver-operating-characteristics.html, https://github.com/chupvl/R_scripts/blob/master/rocr_wLegend.R accessed: 14/03/2019)

library("ROCR")

#setting visual parameters

par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)

par(mfrow=c(1,2)) # displays two plots side by side


perf.obj_g_other <- prediction(predictions=pred_probs,labels=g_other$FXResult)

# Get data for ROC curve
roc.obj_g_other <- performance(perf.obj_g_other, measure="tpr", x.measure="fpr")
plot(roc.obj_g_other,
     main="Classifier 2 (Prediction) - ROC Curve",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="blue")
abline(0,1,col="grey")

# Get AUC value from ROC curve

auc <- performance(perf.obj_g_other, measure = "auc")
auc <- auc@y.values[[1]]

# adding ROC AUC to the center of the plot

maxauc<-max(round(auc, digits = 2))

maxauct <- paste(c("AUC = "),maxauc,sep="")
legend(0.37,0.34, c(maxauct),border="white",cex=1.33,box.col = "white")
