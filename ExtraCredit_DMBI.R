library (tidyverse) 
library (ggfortify)
library (qdapTools)
library (tidyr)
library (corrplot)
library (ggpubr)
library(ggplot2)
library(PerformanceAnalytics)
library(factoextra)
library(fpc)
install.packages("DataExplorer")
library(DataExplorer)

install.packages("dplyr")
install.packages("tidyr")
install.packages("GGally")
install.packages("Lahman")

#Source in the file as CSV
setwd("C:/Users/Anukriti Sharma/Downloads")
train <- read.csv("churn-bigml-80.csv", stringsAsFactors = FALSE)
tel_test  <- read.csv("churn-bigml-20.csv", stringsAsFactors = FALSE)

#total dataset
total <- rbind(train,tel_test)

#EDA
plot_str(total)

plot_missing(total)

duplicated(total)

plot_density(total)

plot_correlation(total, type = 'continuous','Review.Date')

plot_bar(total)

create_report(total)

#Get the idea of how data is structured
head(total)
str(total)

#Visualize the current distribution of the dependent variable
churn_yes <- total[total$Churn == 1,]
nrow(churn_yes)

churn_no <- total[total$Churn == 0,]
nrow(churn_no)

text(barplot(table(total_f$Churn),main = 'Bar plot of Churn variable in source data'),0,
     table(total_f$Churn),cex = 1,pos = 3)

#Oversampling
churn_no <- total[total$Churn=='0',]
nrow(churn_no)
churn_total_no <- churn_no[sample(nrow(churn_no),200),]
churn_total_no
churn_yes <- total[total$Churn=='1',]
nrow(churn_yes)
churn_total_yes <- churn_yes[sample(nrow(churn_yes),200),]
churn_total_yes

total_f<-rbind(churn_total_no,churn_total_yes)

#Check for any correlated variables
library(corrplot)
cor <- cor(total_f[sapply(total_f,is.numeric)])
corrplot(cor)

total_f <- subset(total_f,select = -c(Total.day.minutes,Total.eve.minutes,Total.night.minutes,Total.intl.minutes))


total_f$Account.Length <-NULL
total_f$VMail.Message <-NULL
total_f$Day.Mins <-NULL
total_f$Eve.Mins <-NULL
total_f$Night.Mins <-NULL
total_f$Intl.Mins <-NULL
total_f$CustServ.Calls <-NULL
total_f$Int.l.Plan <-NULL
total_f$Day.Calls <-NULL
total_f$Eve.Calls <-NULL
total_f$Night.Calls <-NULL
total_f$Intl.Calls <-NULL
total_f$Area.Code <-NULL
total_f$State <-NULL
total_f$Phone <-NULL

#Scaling the dataset
total_f <- scale(total_f)
total_f
varnames <- names(total_f)
signif_table <- data.frame(variable_name = character(), regression_coefficient = numeric(), odds_ratio = numeric(), pvalue = numeric(), coefficient_direction = character(), stringsAsFactors = FALSE)
i = 1 
for (var in varnames[!varnames %in% c("Churn")]) { 
  Y <- total_f$Churn 
  X <- unlist(total_f[, var]) 
  reg <- glm(Y ~ X, family = binomial) 
  signif_table[i, 1] = var 
  signif_table[i, 2] = abs(summary(reg)$coefficients[2, 1]) 
  signif_table[i, 3] = exp(summary(reg)$coefficients[2, 1])
  signif_table[i, 4] = summary(reg)$coefficients[2, 4] 
  signif_table[i, 5] = ifelse(summary(reg)$coefficients[2, 1] > 0, "Positive", "Negative") 
  i = i + 1 
}

signif_table <- signif_table %>% arrange(pvalue) 

signif_table


#Split the source data into 80:20 ratio as mentioned in the assignment
# To ensure a uniform start point for generating the random nos. we use set.seed function

total_f
require(caTools)
set.seed(123)
partition <- sample.split(total_f,SplitRatio = 0.80)
partition

# Step 7 : Use this partition for alloting the rows to Training and Testing datasets

train <- subset(total_f,partition == TRUE)
train
test <- subset(total_f,partition == FALSE)
test
str(train)

nrow(train)
nrow(test)

#Run the Logistics Regression model over the training set with Churn as the dependent variable
install.packages("caTools")    # For Logistic regression
install.packages("ROCR")       # For ROC curve to evaluate model

# Loading package
library(caTools)
library(ROCR) 

log_model <- glm(as.factor(Churn) ~.,train,family = "binomial")
log_model
summary(log_model)
#BIC
null_mod <- glm(as.factor(Churn) ~1,train,family = "binomial")
model_2_BIC<- step(null_mod, scope = list(lower = null_mod, upper = log_model), direction = 'both')
summary(model_2_BIC)

install.packages("car")
library(car)
vif(model_2_BIC)
final <- glm(Churn ~ International.plan+Customer.service.calls+Voice.mail.plan+
               +Total.eve.charge+Total.day.charge +Area.code+Total.intl.calls,
             train,family = "binomial")

pred_final <- predict(final,train,type = "response")
summary(pred_final)
validation$prob <- pred_final

cutoff_churn <- factor(ifelse(pred_final >=0.5, "Yes", "No"))
actual_churn <- factor(ifelse(train$Churn==1,"Yes","No"))
conf_final <- caret::confusionMatrix(cutoff_churn, actual_churn, positive = "Yes")
accuracy <- conf_final$overall[1]
sensitivity <- conf_final$byClass[1]
specificity <- conf_final$byClass[2]
accuracy
sensitivity
specificity


options(repr.plot.width =8, repr.plot.height =6)
summary(pred_final)
s = seq(0.01,0.80,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),
     type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend("bottom",col=c(2,"darkgreen",4,"darkred"),text.font =3,inset = 0.02,
       box.lty=0,cex = 0.8, 
       lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
abline(v = 0.32, col="red", lwd=1, lty=2)
axis(1, at = seq(0.1, 1, by = 0.1))
#  Predict the Churn values by deploying this model ovet the test data

install.packages("ROCR")
library(ROCR)


result<-predict(final,test,type = "response")
result1<-predict(final,train,type = "response")
roc_pred <- prediction(result,test$Churn)
roc_pred1 <- prediction(result1,train$Churn)
roc_perf <- performance(roc_pred,"tpr","fpr")
roc_perf1 <- performance(roc_pred1,"tpr","fpr")
plot(roc_perf,colorize=TRUE,print.cutoffs.at=seq(0.05,by=0.05))
plot(roc_perf1,colorize=TRUE,print.cutoffs.at=seq(0.05,by=0.05))


#Miss classification Rate
pcut <- mean(total$Churn)
pcut
#Test Data
test_1 <- (result>pcut)*1 

MR_OutSample <- mean(test$Churn!=test_1)
MR_OutSample

#Train Data
test_2 <- (result1>pcut)*1 

MR_OutSample2 <- mean(train$Churn!=test_2)
MR_OutSample2

# Step 11 : Create a confusion matrix

result_test <- predict(log_model,train,type = "response")
table(Observed = train$Churn,Predicted = result_test > 0.25)


#Accuracy
unlist(slot(performance(roc_pred, "auc"), "y.values"))#test data
unlist(slot(performance(roc_pred1, "auc"), "y.values"))#training data



