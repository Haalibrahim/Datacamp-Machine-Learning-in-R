#Exercise Name: Visulaizing Classification trees


# Examine the loan_model object
loan_model

# Load the rpart.plot package
library(rpart.plot)

# Plot the loan_model with default settings
rpart.plot(loan_model)

# Plot the loan_model with customized settings
rpart.plot(loan_model, type = 3, box.palette = c("red", "green"), fallen.leaves = TRUE)




##Exercise Name: Building and Evaluating a larger tree ##

# The 'rpart' package is loaded into the workspace
# The loans_train and loans_test datasets have been created
str(loans_train)
# Grow a tree using all of the available applicant data
loan_model <- rpart(outcome ~., data = loans_train, method = "class", control = rpart.control(cp = 0))

# Make predictions on the test dataset
loans_test$pred <- predict(loan_model, loans_test, type="class") 

# Examine the confusion matrix
table(loans_test$pred, loans_test$outcome)

# Compute the accuracy on the test dataset
mean(loans_test$outcome == loans_test$pred)



##Exercise Name: Preventing outgrown trees ##

# The 'rpart' package is loaded into the workspace

# Grow a tree with maxdepth of 6
loan_model <- rpart(outcome~., data=loans_train, control=rpart.control(maxdepth=6, cp=0), method="class")

# Compute the accuracy of the simpler tree
loans_test$pred <- predict(loan_model, loans_test, type="class")
mean(loans_test$pred==loans_test$outcome)

# Grow a tree with minsplit of 500
loan_model2 <- rpart(outcome~., data=loans_train, control=rpart.control(minsplit=500, cp=0),  method="class")

# Compute the accuracy of the simpler tree
loans_test$pred2 <-  predict(loan_model2, loans_test, type="class")
mean(loans_test$pred2==loans_test$outcome)


##Exercise Name: Creating a nicely pruned tree ##


# The 'rpart' package is loaded into the workspace

# Grow an overly complex tree
loan_model <- rpart(outcome ~ ., data = loans_train, method = "class", control = rpart.control(cp = 0))

# Examine the complexity plot
plotcp(loan_model)

# Prune the tree
loan_model_pruned <- prune(loan_model, cp = 0.0014)

# Compute the accuracy of the pruned tree
loans_test$pred <- predict(loan_model_pruned, loans_test, type = "class")
mean(loans_test$pred == loans_test$outcome)


## Exercise Name: Building a random forest model ##

# Load the randomForest package
library(randomForest)

# Build a random forest model
loan_model <- randomForest(outcome ~ ., data = loans_train)

# Compute the accuracy of the random forest
loans_test$pred <- predict(loan_model, loans_test)
mean(loans_test$pred == loans_test$outcome)