library(dplyr)
install.packages("glmnet")
library(glmnet)
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)



data1 <- read.csv2("bank2.csv")
summary(data1)

# ----------- Tree --------------------
data2 <- data1
summary(data2)
View(data1)

data2$job <- as.factor(data2$job)
data2$marital <- as.factor(data2$marital)
data2$education <- as.factor(data2$education)

data2$default <- as.factor(data2$default)
data2$housing <- as.factor(data2$housing)
data2$loan <- as.factor(data2$loan)

data2$contact <- as.factor(data2$contact)
data2$month <- as.factor(data2$month)
data2$poutcome <- as.factor(data2$poutcome)

head(data2)

index2 <- sample(nrow(data2),nrow(data2)*0.75)
tree.train = data2[index2,]
tree.test = data2[-index2,]

set.seed(1)
rf_model <- randomForest(formula = data2$y ~., 
                         data = data2)

print(rf_model)
#Plot OOB
err <- rf_model$err.rate
head(err)
oob_err <- err[500, "OOB"]
print(oob_err)
plot(rf_model)
legend(x = "right", 
       legend = colnames(err),
       fill = 1:ncol(err))


# Execute the tuning process
set.seed(1)              
res <- tuneRF(x = subset(tree.train, select = -y),
              y = tree.train$y,
              ntreeTry = 500)

mtry_opt <- res[,"mtry"][which.min(res[,"OOBError"])]
print(mtry_opt)
#Hence model is tuned wrt mtry paramenters




