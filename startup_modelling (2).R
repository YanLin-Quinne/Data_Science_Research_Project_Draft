##### Library Imports #####
library(readxl)
library(tidyverse)
library(corrplot)
library(ggplot2)
library(caret)
library(lessR)
library(pROC)
library(mice)
library(VIM)
library(viridis)      
library(psych)
library(tree)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(scales)
library(glmnet)
library(leaps)
library(corrplot)


##### Import Data and Make Suitable for Analysis #####

# Import Data
startup_df = data.frame(read_excel("~/Documents/MASTERS/Dissertation/diss_modelling/processed_df.xlsx"))

# Check For Duplicate Names and drop column "Company.Name"

startup_df = startup_df[!duplicated(startup_df$Company.Name),]
startup_df = startup_df[,-2]

# Set New Column Names for ease of coding
new_colnames = c("company_status", "industry_type", "total_raised",
                 "employee_count", "country", "region", "sub_region", "first_financing_size",
                 "first_financing_valuation", "number_of_competitors", "top100_description_count",
                 "description_sentiment","top100_keyword_count", "number_of_industries", "number_of_verticals",
                 "employee_growth", "company_age","years_before_first_financing", "linkedin_follower_count")
colnames(startup_df) = new_colnames

# Change target variable to 0,1
startup_df$company_status[startup_df$company_status=="Bankrupt"] = 0
startup_df$company_status[startup_df$company_status=="Operating"] = 1

# Change to correct type
startup_df$company_status = as.factor(startup_df$company_status)
startup_df$industry_type = as.factor(startup_df$industry_type)
startup_df$country = as.factor(startup_df$country)
startup_df$region = as.factor(startup_df$region)
startup_df$sub_region = as.factor(startup_df$sub_region)

# Rename levels of industry for ease of reading
levels(startup_df$industry_type) = c("B2B", "B2C", "Energy", "Financial",
                                "Healthcare", "IT", "Materials")

str(startup_df)

##### EDA #####

### Missingness ###

sum(complete.cases(startup_df)) # only 174 rows not missing some data point
aggr(startup_df, col=c("cornflowerblue", "red"), numbers=TRUE, sortVars=TRUE, labels=names(startup_df),
     cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

startup_df = startup_df[,-9] # Remove first financing valuation as 69% missing

sum(complete.cases(startup_df)) # 361
aggr(startup_df, col=c("cornflowerblue", "red"), bars = F, sortVars=F, prop = T, labels = T,
     cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

imputed_df = mice(startup_df, m=1, maxit = 1, method = "mean")
densityplot(imputed_df)
startup_df = complete(imputed_df, 1)
sum(complete.cases(startup_df))

### Categorical Variables ###

## Country Barplot
country_bp = barplot(table(startup_df$country), col = viridis(19), ylim = c(0,500), las = 2, cex.names = 0.68,
                     main = "Number of Observations in 'Country' Factor Levels", cex.main = 1.5,
                     xlab = "",ylab = "Observation Count", cex.lab = 1.2)
text(country_bp, c(table(startup_df$country)), table(startup_df$country), cex=1.2, pos=3)

# Remove less than 20
countries_keep = levels(startup_df$country)[table(startup_df$country) > 20]
startup_df = startup_df[startup_df$country %in% countries_keep,]
startup_df$country = factor(startup_df$country)
startup_df$sub_region = factor(startup_df$sub_region)

# New bp for country
country_bp = barplot(table(startup_df$country), col = viridis(5), ylim = c(0,500),cex.names = 2.0,
                     main = "Number of Observations in 'Country' Factor Levels - After Removing Factor Levels with less than 20 Observations", cex.main = 1.5,
                     xlab = "",ylab = "Observation Count", cex.lab = 1.2)
text(country_bp, c(table(startup_df$country)), table(startup_df$country), cex=1.2, pos=3)

## Sub Region Barplot
sub_region_bp = barplot(table(startup_df$sub_region), col = viridis(3), ylim = c(0,500), cex.names = 2.0,
                        main = "Number of Observations in 'Sub Region' Factor Levels", cex.main = 1.5,
                        xlab = "Sub Region",ylab = "Observation Count", cex.lab = 1.5)
text(sub_region_bp, c(table(startup_df$sub_region)), table(startup_df$sub_region), cex=1.5, pos=3)

# Remove sub region
startup_df = startup_df[,-7]

## Industry BP

industry_bp = barplot(table(startup_df$industry), col = c("cornflowerblue","darkgreen"),ylim = c(0,500),
                            main = "Number of Observations in 'Industry' Factor Levels", cex.main = 1.25,
                            xlab = "Industry",ylab = "Observation Count", cex.lab = 1, cex.names = 1.0)
text(company_status_bp, c(table(startup_df$company_status)), table(startup_df$company_status), cex=1.5, pos=3)

## Company Status Barplot
company_status_bp = barplot(table(startup_df$company_status), col = viridis(2),ylim = c(0,500),
                            main = "Number of Observations in 'Company Status' Factor Levels", cex.main = 1.5,
                            xlab = "Company Status",ylab = "Observation Count", cex.lab = 1.5,
                            names.arg = c("Bankrupt", "Exited"), cex.names = 2.0)
text(company_status_bp, c(table(startup_df$company_status)), table(startup_df$company_status), cex=1.5, pos=3)


## Region Barplot
region_bp = barplot(table(startup_df$region), col = c("cornflowerblue","darkgreen"),ylim = c(0,550),
                    main = "Number of Observations in 'Region' Factor Levels", cex.main = 1.5,cex.lab = 1.5,
                    xlab = "Region",ylab = "Observation Count",cex.names = 2.0)
text(region_bp, c(table(startup_df$region)), table(startup_df$region), cex=1.5, pos=3)

### Numerical Variables ###

# Numerical Variable subset
numeric_columns = unlist(lapply(startup_df, is.numeric))
numeric_variables = startup_df[ ,numeric_columns]

# corrplot
cor_matrix = cor(numeric_variables, method = "pearson")
corrplot(cor_matrix, method="circle", type = "upper")

#remove employee_growth due to covarience with employee count
startup_df = startup_df[,-14]

pairs(numeric_variables, main = "Scatter Plot Matrix for Numeric Variables",
             method = "pearson", 
             hist.col = "cornflowerblue",
             density = T, 
             ellipses = F,
             pch = 16)

### Total Raised with respect to target
boxplot(startup_df$total_raised[startup_df$company_status == 0], ylim = c(0,2500),
        startup_df$total_raised[startup_df$company_status == 1],
        names = c("Bankrupt", "Exited"), col = viridis(2),
        xlab = "Company Status", ylab = "Total Raised",
        main = "Distribution of 'total_raised' with respect to 'company_status'")

### Employee Count with respect to target
boxplot(startup_df$employee_count[startup_df$company_status == 0], ylim = c(0,2050),
        startup_df$employee_count[startup_df$company_status == 1],
        names = c("Bankrupt", "Exited"), col = viridis(2),
        xlab = "Company Status", ylab = "Employee Count",
        main = "Distribution of 'employee_count' with respect to 'company_status'")

### Linked In follower count count with respect to target

boxplot(startup_df$linkedin_follower_count[startup_df$company_status == 0], ylim = c(0,75000),
        startup_df$linkedin_follower_count[startup_df$company_status == 1],
        names = c("Bankrupt", "Exited"), col = viridis(2),
        xlab = "Company Status", ylab = "LinkedIn Follower Count",
        main = "Distribution of 'linkedin_follower_count' with respect to 'company_status'")

# Employee and linked in follower count BEFORE IMPUTATION 
# Note: This requires re running the data retrieval at the beginning of the script

boxplot(startup_df$employee_count[startup_df$company_status == 0], ylim = c(0,2050),
        startup_df$employee_count[startup_df$company_status == 1],
        names = c("Bankrupt", "Exited"), col = viridis(2),
        xlab = "Company Status", ylab = "Employee Count",
        main = "Distribution of 'employee_count' with respect to 'company_status' (before imputation)")

### Linked In follower count count with respect to target

boxplot(startup_df$linkedin_follower_count[startup_df$company_status == 0], ylim = c(0,75000),
        startup_df$linkedin_follower_count[startup_df$company_status == 1],
        names = c("Bankrupt", "Exited"), col = viridis(2),
        xlab = "Company Status", ylab = "LinkedIn Follower Count",
        main = "Distribution of 'linkedin_follower_count' with respect to 'company_status' (before imputation)")

# Remove total raised, employee count, and linked in

startup_df = startup_df[,-c(3,4,16)]

##### Data Partition #####

set.seed(6) # 4 is good

index = createDataPartition(startup_df$company_status, p = 0.8, list=FALSE, times = 1)
train = startup_df[index,]
test = startup_df[-index,]

##### Logistic Regression #####

# Create Full Model and Variable Selection Model

null_logit_model = glm(company_status~1, data=train, family = "binomial")
full_logit_model = glm(company_status~., data=train, family = "binomial")
summary(full_logit_model)

step(null_logit_model, scope=formula(full_logit_model), direction="forward") 

forward_step_logit_model = glm(formula = company_status ~ top100_description_count + country + 
                                 top100_keyword_count + number_of_competitors + description_sentiment + 
                                 first_financing_size + number_of_verticals + years_before_first_financing + 
                                 number_of_industries, family = "binomial", data = train)

summary(forward_step_logit_model)

##### CART #####

### Standard Cart
unpruned_tree_model = tree(company_status ~., data = train, method = "class")
plot(unpruned_tree_model)
text(unpruned_tree_model, pretty = 0)
title("Unpruned Classification Tree", line = +3)
summary(unpruned_tree_model)

### Pruned Tree

pruned_tree = cv.tree(unpruned_tree_model, FUN=prune.tree)
plot(pruned_tree)
title("Deviance as Size of Pruned Tree Increases", line = +3)

pruned_tree_model = prune.tree(unpruned_tree_model, best = 10)
summary(pruned_tree_model)
plot(pruned_tree_model)
text(pruned_tree_model, pretty = 0)
title("Pruned Classification Tree", line = +3)

### Bagged Model

# Fine Tune Number of Bagged Trees

classification_accuracy_vec = c()

for (i in 1:100){
  
  bagged_model = randomForest(company_status ~ ., train, ntree=(i*5), importance =TRUE)
  
  predicted_labels = predict(bagged_model, test, type = "response")
  cm_bag = table(predicted_labels, test$company_status)
  
  accuracy = (cm_bag[1,1] + cm_bag[1,1]) / nrow(test)
  
  classification_accuracy_vec[i] = accuracy
}

# Performance Does not Increase Past 240 Trees
tree_number = seq(from = 5, to = 500, by = 5)
colours = c(rep("black", 100))
colours[(which.max(classification_accuracy_vec))] = "green" # 85 Trees

plot(tree_number, classification_accuracy_vec, main = "Classification Accuracy as Number of Trees in Bagging Classification Tree Increases", 
     xlab = "Number of Trees", ylab = "Classification Accuracy", pch = 16,
     col = colours) 

bagged_model = randomForest(company_status ~ ., train, ntree=240, importance =TRUE)
importance(bagged_model)

### Random Forests

classification_accuracy_vec = double(13)

for (i in 1:13){
  
  rf_model = randomForest(company_status ~ ., train, mtry = i, ntree=240, importance =TRUE)

  predicted_labels = predict(rf_model, test, type = "response")
  
  cm_rf = table(predicted_labels, test$company_status)
  
  accuracy = (cm_rf[1,1] + cm_rf[1,1]) / nrow(test)
  
  classification_accuracy_vec[i] = accuracy
}

colours = c(rep("black", 13))
colours[which.max(classification_accuracy_vec)] = "green"

plot(classification_accuracy_vec,main = "Classification Accuracy as Number of  Variables Randomly Sampled at Each Node Increases", 
     xlab = "Number of Variables Avaiable at Each Node", ylab = "Classification Accuracy", pch = 16,
     col = colours) # Optimum is 8 

rf_model = randomForest(company_status ~ ., train, mtry = 5, ntree=240, importance =TRUE)
summary(rf_model)
names(rf_model)

### Boosted Tree

# Find best interaction Depth - best is 4, seed - 1

accuracy_depth = double(10)

for (i in 1:10){
  
  boost_model =gbm(company_status~.,data=train, distribution="multinomial",
                   n.trees =240, interaction.depth =i)
  
  predicted_probabilites = predict(boost_model, test, type = "response")
  predicted_labels = ifelse((predicted_probabilites[,1,] > predicted_probabilites[,2,])  > 0.5 , 0, 1)
  
  cm_rf = table(predicted_labels, test$company_status)
  accuracy = (cm_rf[1,1] + cm_rf[1,1]) / nrow(test)
  
  accuracy_depth[i] = accuracy
  
}

colours = c(rep("black", 10))
colours[which.max(accuracy_depth)] = "green"

plot(accuracy_depth, main = "Accuracy as Depth of Trees Increases", 
     xlab = "Depth", ylab = "Accuracy", pch = 16,
     col = colours)

# Optimise Shrinkage Parameter

shrink_index = seq(from = 0.0025, to = 0.25, by = 0.0025)
accuracy_shrinkage = c()

for (i in 1:100){
  
  boost_model =gbm(company_status~.,data=train, distribution="multinomial",
                   n.trees =240, interaction.depth = 5, shrinkage = (i/400))
  
  predicted_probabilites = predict(boost_model, test, type = "response")
  predicted_labels = ifelse((predicted_probabilites[,1,] > predicted_probabilites[,2,])  > 0.5 , 0, 1)
  
  cm_rf = table(predicted_labels, test$company_status)
  accuracy = (cm_rf[1,1] + cm_rf[1,1]) / nrow(test)
  
  accuracy_shrinkage[i] = accuracy

}

colours = c(rep("black", 500))
colours[which.max(accuracy_shrinkage)] = "green"

plot(shrink_index, accuracy_shrinkage, main = "Accuracy as Shrinkage Parameter Increases",
     xlab = "Lambda (Shrinkage Parameter)",ylab = "Accuracy", pch = 16,
     col = alpha(colours,0.8))

boost_model =gbm(company_status~.,data=train, distribution="multinomial",
                 n.trees =240, interaction.depth = 5, shrinkage = 0.0725)

summary(boost_model)

##### All Model Comparison #####

repetitions = 100 # How many times to run comparison

# Empty vectors to store respective model accuracies
accuracy_full_logit = c()
accuracy_forward_selection_logit = c()
accuracy_tree = c()
accuracy_prune = c() 
accuracy_bag = c()
accuracy_rf = c()
accuracy_boost = c()

precision_full_logit = c()
precision_forward_selection_logit = c()
precision_tree = c()
precision_prune = c()
precision_bag = c()
precision_rf = c()
precision_boost = c()

recall_full_logit = c()
recall_forward_selection_logit = c()
recall_tree = c()
recall_prune = c()
recall_bag = c()
recall_rf = c()
recall_boost = c()

specificity_full_logit = c()
specificity_forward_selection_logit = c()
specificity_tree = c()
specificity_prune = c()
specificity_bag = c()
specificity_rf = c()
specificity_boost = c()

for (i in 1:repetitions){
  
  # Split Data
  index = createDataPartition(startup_df$company_status, p = 0.8, list=FALSE, times = 1)
  train = startup_df[index,]
  test = startup_df[-index,]
  
  # Train Models
  
  full_logit_model = glm(company_status~., data=train, family = "binomial")
  
  forward_step_logit_model = glm(formula = company_status ~ top100_description_count + country + 
                                   top100_keyword_count + number_of_competitors + description_sentiment + 
                                   first_financing_size + number_of_verticals + years_before_first_financing + 
                                   number_of_industries, family = "binomial", data = train)
  
  tree_model = tree(company_status ~., data = train)
  
  pruned_tree_model = prune.tree(tree_model, best = 10)
  
  bagged_model = randomForest(company_status ~ ., train, ntree=240, importance =TRUE)
  
  rf_model = randomForest(company_status ~ ., train, mtry = 5, ntree=240, importance =TRUE)
  
  boost_model =gbm(company_status~.,data=train, distribution="multinomial",
                   n.trees =240, interaction.depth =5, shrinkage = 0.0725)
  
  # Predict with models on test set
  full_logit_probabilites = predict(full_logit_model, test, type = "response")
  predicted_labels = ifelse(full_logit_probabilites > 0.5, 1, 0)
  cm_full_logit = confusionMatrix(factor(predicted_labels), factor(test$company_status))
  cm_full_logit$table[1,1]

  forward_selection_logit_probabilites = predict(forward_step_logit_model, test, type = "response")
  predicted_labels = ifelse(forward_selection_logit_probabilites > 0.5, 1, 0)
  cm_forward_selection_logit = confusionMatrix(factor(predicted_labels), factor(test$company_status))
  
  tree_model_preds = predict(tree_model, test, type = "class")
  cm_tree = confusionMatrix(factor(tree_model_preds), factor(test$company_status))
  
  pruned_tree_model_preds = predict(pruned_tree_model, test, type = "class")
  cm_pruned = confusionMatrix(factor(pruned_tree_model_preds), (test$company_status))
  
  bagged_predicted_labels = predict(bagged_model, test, type = "response")
  cm_bagged = confusionMatrix(factor(bagged_predicted_labels), factor(test$company_status))
  
  rf_predicted_labels = predict(rf_model, test, type = "response")
  cm_rf = confusionMatrix(factor(rf_predicted_labels), factor(test$company_status))
  
  boost_predicted_probabilites = predict(boost_model, test, type = "response")
  boost_predicted_labels = ifelse((boost_predicted_probabilites[,1,] > boost_predicted_probabilites[,2,])  > 0.5 , 0, 1)
  cm_boost = confusionMatrix(factor(boost_predicted_labels), factor(test$company_status))

  # Compute and Store Accuracy Metrics
  
  accuracy_full_logit[i] = (cm_full_logit$table[1,1] + cm_full_logit$table[2,2]) / nrow(test)
  accuracy_forward_selection_logit[i] = (cm_forward_selection_logit$table[1,1] + cm_forward_selection_logit$table[2,2]) / nrow(test)
  accuracy_tree[i] = (cm_tree$table[1,1] + cm_tree$table[2,2]) / nrow(test)
  accuracy_prune[i] = (cm_pruned$table[1,1] + cm_pruned$table[2,2]) / nrow(test)
  accuracy_bag[i] = (cm_bagged$table[1,1] + cm_bagged$table[2,2]) / nrow(test)
  accuracy_rf[i] = (cm_rf$table[1,1] + cm_rf$table[2,2]) / nrow(test)
  accuracy_boost[i] = (cm_boost$table[1,1] + cm_boost$table[2,2]) / nrow(test)
  
  precision_full_logit[i] = cm_full_logit$byClass["Pos Pred Value"]
  precision_forward_selection_logit[i] = cm_forward_selection_logit$byClass["Pos Pred Value"]
  precision_tree[i] = cm_tree$byClass["Pos Pred Value"]
  precision_prune[i] = cm_pruned$byClass["Pos Pred Value"]
  precision_bag[i] = cm_bagged$byClass["Pos Pred Value"]
  precision_rf[i] = cm_rf$byClass["Pos Pred Value"]
  precision_boost[i] = cm_boost$byClass["Pos Pred Value"]
  
  recall_full_logit[i] = cm_full_logit$byClass["Sensitivity"]
  recall_forward_selection_logit[i] = cm_forward_selection_logit$byClass["Sensitivity"]
  recall_tree[i] = cm_tree$byClass["Sensitivity"]
  recall_prune[i] = cm_pruned$byClass["Sensitivity"]
  recall_bag[i] = cm_bagged$byClass["Sensitivity"]
  recall_rf[i] = cm_rf$byClass["Sensitivity"]
  recall_boost[i] = cm_boost$byClass["Sensitivity"]
  
  specificity_full_logit[i] = cm_full_logit$byClass["Specificity"]
  specificity_forward_selection_logit[i] = cm_forward_selection_logit$byClass["Specificity"]
  specificity_tree[i] = cm_tree$byClass["Specificity"]
  specificity_prune[i] = cm_pruned$byClass["Specificity"]
  specificity_bag[i] = cm_bagged$byClass["Specificity"]
  specificity_rf[i] = cm_rf$byClass["Specificity"]
  specificity_boost[i] = cm_boost$byClass["Specificity"]
}

# Compare
boxplot(accuracy_full_logit,accuracy_forward_selection_logit, accuracy_tree, accuracy_prune, accuracy_bag, accuracy_rf, accuracy_boost,
        names = c('Full Logit',
                  'Forward Selection Logit',
                  'Unpruned Tree',
                  'Pruned Tree',
                  'Bagged Tree', 
                  'Random Forest Tree',
                  "Boosted Tree"),
        main = "Test Accuracy Distribution for All Models",
        xlab = "Model Type", ylab = 'Test Accuracy',
        col = viridis(7))

mean(accuracy_full_logit)
mean(accuracy_forward_selection_logit)
mean(accuracy_tree) 
mean(accuracy_prune) 
mean(accuracy_bag) 
mean(accuracy_rf) 
mean(accuracy_boost) 

mean(precision_full_logit)
mean(precision_forward_selection_logit)
mean(precision_tree)
mean(precision_prune)
mean(precision_bag)
mean(precision_rf)
mean(precision_boost)

mean(recall_full_logit)
mean(recall_forward_selection_logit)
mean(recall_tree)
mean(recall_prune)
mean(recall_bag)
mean(recall_rf)
mean(recall_boost)

mean(specificity_full_logit)
mean(specificity_forward_selection_logit)
mean(specificity_tree)
mean(specificity_prune)
mean(specificity_bag)
mean(specificity_rf)
mean(specificity_boost)




##### Discussion #####

### Description
mean_description_bp = barplot(
  c(mean(startup_df$top100_description_count[startup_df$company_status == 0]),
    mean(startup_df$top100_description_count[startup_df$company_status == 1])),
  main = "Mean Value of 'top100_description_count' with Respect to Target Variable",
  ylim = c(0,15), col = viridis(2), legend.text = c("Bankrupt", "Exited"),
  ylab = "mean 'top100_description_count'", xlab = "Company Status",
  args.legend = list(x = "top",inset = c(- 0.15, 0)))
text(mean_description_bp, 
     c(mean(startup_df$top100_description_count[startup_df$company_status == 0]),
       mean(startup_df$top100_description_count[startup_df$company_status == 1])),
     c(round(mean(startup_df$top100_description_count[startup_df$company_status == 0]),2),
       round(mean(startup_df$top100_description_count[startup_df$company_status == 1]),2)),
     cex=1.2, pos=3)

### Country
country_bp = barplot(table(startup_df$company_status, startup_df$country), col = viridis(2), ylim = c(0,300), beside = TRUE,
                     legend.text = c("Bankrupt", "Exited"),
                     args.legend = list(x = "top",inset = c(- 0.15, 0)),
                     main = "Distribution of Country with respect to Company Status", cex.main = 1.5,
                     xlab = "Country",ylab = "Number of Obsevations Count", cex.lab = 1.2)
text(country_bp, 
     c(table(startup_df$company_status, startup_df$country)),
     table(startup_df$company_status, startup_df$country),
     cex=1.2, pos=3)

### Competitors
competitors_bp = barplot(
  c(mean(startup_df$number_of_competitors[startup_df$company_status == 0]),
    mean(startup_df$number_of_competitors[startup_df$company_status == 1])),
  main = "Mean Value of 'number_of_competitors' with Respect to Target Variable",
  ylim = c(0,40), col = viridis(2), legend.text = c("Bankrupt", "Exited"),
  ylab = "Mean 'number_of_competitors'", xlab = "Company Status",
  args.legend = list(x = "top",inset = c(- 0.15, 0)))
text(competitors_bp, 
     c(mean(startup_df$number_of_competitors[startup_df$company_status == 0]),
       mean(startup_df$number_of_competitors[startup_df$company_status == 1])),
     c(round(mean(startup_df$number_of_competitors[startup_df$company_status == 0]),2),
       round(mean(startup_df$number_of_competitors[startup_df$company_status == 1]),2)),
     cex=1.2, pos=3)

### Keyword
mean_keyword_bp = barplot(
  c(mean(startup_df$top100_keyword_count[startup_df$company_status == 0]),
    mean(startup_df$top100_keyword_count[startup_df$company_status == 1])),
  main = "Mean Value of 'top100_keyword_count' with Respect to Target Variable",
  ylim = c(0,15), col = viridis(2), legend.text = c("Bankrupt", "Exited"),
  ylab = "mean 'top100_keyword_count'", xlab = "Company Status",
  args.legend = list(x = "top",inset = c(- 0.15, 0)))
text(mean_description_bp, 
     c(mean(startup_df$top100_keyword_count[startup_df$company_status == 0]),
       mean(startup_df$top100_keyword_count[startup_df$company_status == 1])),
     c(round(mean(startup_df$top100_keyword_count[startup_df$company_status == 0]),2),
       round(mean(startup_df$top100_keyword_count[startup_df$company_status == 1]),2)),
     cex=1.2, pos=3)

### Industry
industry_type_bp = barplot(table(startup_df$company_status, startup_df$industry_type), beside = TRUE, 
                           ylim = c(0,300), col = viridis(2),
                           legend.text = c("Bankrupt", "Exited"),
                           xlab = "Industry Type", ylab = "Number of Observations",
                           main = "Distribution of Industry Type with respect to Company Status")
text(industry_type_bp, c(table(startup_df$company_status, startup_df$industry_type)),
     table(startup_df$company_status, startup_df$industry_type),
     cex=1.2, pos=3)

### Keyword
first_financing_bp = barplot(
  c(mean(startup_df$first_financing_size[startup_df$company_status == 0]),
    mean(startup_df$first_financing_size[startup_df$company_status == 1])),
  main = "Mean Value of 'first_financing_size' with Respect to Target Variable",
  ylim = c(0,30), col = viridis(2), legend.text = c("Bankrupt", "Exited"),
  ylab = "mean 'first_financing_size'", xlab = "Company Status",
  args.legend = list(x = "top",inset = c(- 0.15, 0)))
text(mean_description_bp, 
     c(mean(startup_df$first_financing_size[startup_df$company_status == 0]),
       mean(startup_df$first_financing_size[startup_df$company_status == 1])),
     c(round(mean(startup_df$first_financing_size[startup_df$company_status == 0]),2),
       round(mean(startup_df$first_financing_size[startup_df$company_status == 1]),2)),
     cex=1.2, pos=3)

### Region

region_bp = barplot(table(startup_df$company_status, startup_df$region), beside = TRUE, 
                           ylim = c(0,400), col = viridis(2),
                           legend.text = c("Bankrupt", "Exited"),
                           args.legend = list(x = "top",inset = c(- 0.15, 0)),
                           xlab = "Region", ylab = "Number of Observations",
                           main = "Distribution of Region with respect to Company Status")
text(industry_type_bp, c(table(startup_df$company_status, startup_df$region)),
     table(startup_df$company_status, startup_df$region),
     cex=1.2, pos=3)

### Company Age

age_bp = barplot(
  c(mean(startup_df$company_age[startup_df$company_status == 0]),
    mean(startup_df$company_age[startup_df$company_status == 1])),
  main = "Mean Value of 'company_age' with Respect to Target Variable",
  ylim = c(0,30), col = viridis(2), legend.text = c("Bankrupt", "Exited"),
  ylab = "mean 'company_age'", xlab = "Company Status",
  args.legend = list(x = "top",inset = c(- 0.15, 0)))
text(mean_description_bp, 
     c(mean(startup_df$company_age[startup_df$company_status == 0]),
       mean(startup_df$company_age[startup_df$company_status == 1])),
     c(round(mean(startup_df$company_age[startup_df$company_status == 0]),2),
       round(mean(startup_df$company_age[startup_df$company_status == 1]),2)),
     cex=1.2, pos=3)



