# nz4gg 
# sys6018-competition-titanic


# Reference:
# https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
# In this assignment, I referred to the functions and codes in this website. 

setwd("~/Downloads/sys6018-competition-titanic-master")
test = read.csv('test.csv',na.strings=c(""))
train = read.csv('train.csv',na.strings=c(""))

# find missing values 
summary(train)
# there are some missing values for variable "Age".
# too many missing values in variable "Cabin"
# there are some missing values in Age and 2 missing values in Embarked 

# filling the missing value in Age with median 
train$Age[is.na(train$Age)] <- median(train$Age,na.rm=T)

# take a subset with certain columns
# delete passengeId, Ticket and Name as they do not contribute to predicting survival 
train <- train[-c(1,4,9,11)]
str(train)
contrasts(train$Sex)
contrasts(train$Embarked)
# fit the data with logistic model 
model <- glm(Survived ~.,family=binomial(link='logit'),data=train)
summary(model)
anova(model, test= 'Chisq')
# Parch Fare Embarked are variables that are not statistically significant 
# remove from the model 

model2 <-glm(Survived ~.-Parch-Fare-Embarked,family=binomial(link='logit'),data=train)
summary(model2) # keep model2 as final model with all predicting variables significant 

#  Pclass      -1.172704   0.120092  -9.765  < 2e-16 ***
#  Sexmale     -2.732242   0.194143 -14.073  < 2e-16 ***
#  Age         -0.039778   0.007779  -5.114 3.16e-07 ***
#  SibSp       -0.353378   0.103276  -3.422 0.000622 ***

######################################
# Predict 
str(test)
# fill the missing value Age 
test$Age[is.na(test$Age)] <- median(test$Age,na.rm=T)
test = test[-c(3,8,10)]
results <- predict(model2,newdata= test,type='response')
results <- ifelse(results > 0.5,1,0)  # if fitted values >0.5 assigned 1, otherwise 0 

table = data.frame(results,test$PassengerId)
write.table(table,file="nz4gg.csv",sep = ',', row.names = F)




