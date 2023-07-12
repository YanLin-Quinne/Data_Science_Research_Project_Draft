#version5.0_0711
#loading the data
df_bat <- read.csv("~/Desktop/final_dataframe.csv")
df_bat

#overview of data
library(skimr)
skim(df_bat)

dim(df_bat) #123  21
names(df_bat)
str(df_bat)

##### EDA #####
# drop column  'forced_to_bat'
#This variable does not need to be considered 
#because it is the exact opposite of choose to bat, with a correlation of -1
df_bat <- df_bat[ , !(names(df_bat) %in% c( "forced_to_bat"))]

### Missingness ###
#Check missing values
image(is.na(df_bat))
colSums(is.na(df_bat))
#avg_win_rate,avg_score,avg_wickets_out missing values:8
#This is normal, as there is no historical data for the first games of 8 teams

library(naniar)
gg_miss_var(df_bat,show_pct = TRUE)
library(visdat)
vis_dat(df_bat)
vis_miss(df_bat)
dev.off()
library(mice)
options(width = 100) 
md.pattern(df_bat)

#delete na rows
df_bat <- na.omit(df_bat) 
dim(df_bat) #115rows 20variables

#verify that there are no more NAs in the dataset.
colSums(is.na(df_bat))

# Change target variable to 0,1
df_bat$Result[df_bat$Result=="loose"] = 0
df_bat$Result[df_bat$Result=="win"] = 1
table(df_bat$Result)
df_bat$Result = as.integer(df_bat$Result)
#0  1 
#68 55 balance

# Change to correct type
df_bat$team = as.factor(df_bat$team)
df_bat$opponent = as.factor(df_bat$opponent)
df_bat$venue = as.factor(df_bat$venue)
df_bat$city = as.factor(df_bat$city)
df_bat$winner = as.factor(df_bat$winner)
df_bat$toss_winner = as.factor(df_bat$toss_winner)

str(df_bat)


###Explore in groups###
library(ggplot2)
library(dplyr)
library(corrplot)
library(viridis)  
library(RColorBrewer)

color_palette <- brewer.pal(2, "Set2")

#target variable
table(df_bat$Result) #63 52 

#Group1: binary categorical variables
# last match result
table(df_bat$last_match_result)
last_match_result_bp = barplot(table(df_bat$last_match_result), col = color_palette , ylim = c(0,100),
                               main = "Number of Observations in 'last match result' Factor Levels", cex.main = 1.5,
                               names.arg = c( "loose", "win"),
                               xlab = "Last Match Result",ylab = "Observation Count", cex.lab = 1.5, cex.names = 2.0)
text(last_match_result_bp, c(table(df_bat$last_match_result)), table(df_bat$last_match_result), cex=1.5, pos=3)

last_match_result_freq <- table(df_bat$last_match_result)
lbls <- paste(names(last_match_result_freq), "\n", round(gender_freq/sum(gender_freq)*100, 1), "%")
pie(gender_freq, main = "Pie Chart of Last Match Result", col = color_palette, labels = lbls)

# Gender
gender_freq <- table(df_bat$gender)
lbls <- paste(names(gender_freq), "\n", round(gender_freq/sum(gender_freq)*100, 1), "%")
pie(gender_freq, main = "Pie Chart of Gender", col = color_palette, labels = lbls)

# Season
season_freq <- table(df_bat$season)
lbls <- paste(names(season_freq), "\n", round(season_freq/sum(season_freq)*100, 1), "%")
pie(season_freq, main = "Pie Chart of Season", col = color_palette, labels = lbls)

# Home Advantage
home_advantage_freq <- table(df_bat$home_advantage)
lbls <- paste(names(home_advantage_freq), "\n", round(home_advantage_freq/sum(home_advantage_freq)*100, 1), "%")
pie(home_advantage_freq, main = "Pie Chart of Home Advantage", col = color_palette, labels = lbls)

# Choose to bat
choose_to_bat_freq <- table(df_bat$choose_to_bat)
lbls <- paste(names(choose_to_bat_freq), "\n", round(choose_to_bat_freq/sum(choose_to_bat_freq)*100, 1), "%")
pie(choose_to_bat_freq, main = "Pie Chart of choose_to_bat", col = color_palette, labels = lbls)

binary_vars <- c("last_match_result", "season", "gender", "home_advantage", "choose_to_bat")
for(var in binary_vars){
  p <- ggplot(df_bat, aes_string(var)) + 
    geom_bar(fill="steelblue") +
    labs(title = paste("Distribution of", var), x = var, y = "Count") +
    theme_minimal()
  print(p)
}


#Group2: Multicategorical Variables
#winner
table(df_bat$winner)
winner_bp = barplot(table(df_bat$winner), col = viridis(8), ylim = c(0,30), las = 2, cex.names = 0.68,
                    main = "Number of Observations in 'winner' Factor Levels", cex.main = 1.5,
                    xlab = "",ylab = "Observation Count", cex.lab = 1.2)
text(winner_bp, c(table(df_bat$winner)), table(df_bat$winner), cex=1.2, pos=3)
#There is a slight imbalance, with 'Welsh Fire' as the team with the least

#toss_winner
table(df_bat$toss_winner)
toss_winner_bp = barplot(table(df_bat$toss_winner), col = viridis(8), ylim = c(0,30),las = 2, cex.names = 0.8,
                         main = "Number of Observations in 'toss winner' Factor Levels", cex.main = 1,
                         xlab = "",ylab = "Observation Count", cex.lab = 1.2)
text(toss_winner_bp, c(table(df_bat$toss_winner)), table(df_bat$toss_winner), cex=1.2, pos=3)
#balance

#compare: winner, toss_winner
color_func <- colorRampPalette(c("lightblue", "darkblue"))
cross_table <- with(df_bat, table(toss_winner, winner))
cross_table
op <- par(no.readonly = TRUE)
par(cex.axis = 0.5)
heatmap(cross_table, col = color_func(10), Rowv = NA, Colv = NA, 
        xlab = "Winner", ylab = "Toss Winner",
        main = "Cross Table of 'toss_winner' and 'winner'")
par(op)
#There are four cases where the colour is very dark, representing a strong association
#toss winner: Welsh Fire, winner:Birmingham Phoenix
#toss winner: Welsh Fire, winner:Manchester Originals
#toss winner: Welsh Fire, winner:Welsh Fire
#toss winner: Manchester Originals, winner:Trent Rockets

#team, opponent
table(df_bat$team)
team_bp = barplot(table(df_bat$team), col = viridis(8), ylim = c(0,30),las = 2, cex.names = 0.8,
                  main = "Number of Observations in 'team' Factor Levels", cex.main = 1,
                  xlab = "",ylab = "Observation Count", cex.lab = 1.2)
text(team_bp, c(table(df_bat$team)), table(df_bat$team), cex=1.2, pos=3) 

table(df_bat$opponent)
opponent_bp = barplot(table(df_bat$opponent), col = viridis(8), ylim = c(0,30),las = 2, cex.names = 0.8,
                      main = "Number of Observations in 'opponent' Factor Levels", cex.main = 1,
                      xlab = "",ylab = "Observation Count", cex.lab = 1.2)
text(opponent_bp, c(table(df_bat$opponent)), table(df_bat$opponent), cex=1.2, pos=3)

#compare with 'opponent' and 'toss_winner'
ggplot(df_bat, aes(x=opponent, y=toss_winner)) + 
  geom_count(aes(size=..n..), show.legend = FALSE) + 
  scale_size_continuous(range = c(1, 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Opponent", y = "Toss Winner", 
       title = "Relationship between 'Opponent' and 'Toss Winner'") 
#These two variables are highly correlated

# venue, city
table(df_bat$venue)
venue_bp = barplot(table(df_bat$venue), col = viridis(8), ylim = c(0,30),las = 2, cex.names = 0.8,
                   main = "Number of Observations in 'venue' Factor Levels", cex.main = 1,
                   xlab = "",ylab = "Observation Count", cex.lab = 1.2)
text(venue_bp, c(table(df_bat$venue)), table(df_bat$venue), cex=1.2, pos=3) 

table(df_bat$city)
city_bp = barplot(table(df_bat$city), col = viridis(7), ylim = c(0,40),las = 2, cex.names = 0.8,
                  main = "Number of Observations in 'city' Factor Levels", cex.main = 1,
                  xlab = "",ylab = "Observation Count", cex.lab = 1.2)
text(city_bp, c(table(df_bat$city)), table(df_bat$city), cex=1.2, pos=3) 
#Unbalanced, London is twice as big as other cities

#compare with city and venue
cross_table <- table(df_bat$city, df_bat$venue)
print(cross_table)
cross_df <- as.data.frame.table(cross_table)
ggplot(cross_df, aes(x=Var1, y=Var2, fill=Freq)) + 
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "City", y = "Venue", fill = "Frequency", 
       title = "Cross Table of 'City' and 'Venue'") 
#These two variables are highly correlated

multi_vars <- c("winner", "toss_winner", "team", "opponent", "venue", "city")
for(var in multi_vars){
  p <- ggplot(df_bat, aes_string(var)) + 
    geom_bar(fill="steelblue") +
    labs(title = paste("Distribution of", var), x = var, y = "Count") +
    theme_minimal() +
    coord_flip()  # flipping coordinates for better visualization
  print(p)
}



#Group3: numeric varaibles
#'match_number' 'total_score' 'total_wickets_player_out' 
#''month' 'day' 'avg_win_rate' 'avg_score' 'avg_wickets_out'¶
boxplot(df_bat$total_score ~ df_bat$team)
boxplot(df_bat$total_wickets_player_out ~ df_bat$team)

num_vars <- c("match_number", 
              "total_score", "total_wickets_player_out", 
              "month", "day", 
              "avg_win_rate", "avg_score", "avg_wickets_out")
for(var in num_vars){
  p <- ggplot(df_bat, aes_string(var)) +
    geom_histogram(binwidth=10, fill="steelblue", color="black") +
    labs(title = paste("Distribution of", var), x = var, y = "Count") +
    theme_minimal()
  print(p)
}

# corrplot
numeric_columns=c("match_number","total_score","total_wickets_player_out","month","day",
                  "avg_win_rate","avg_score","avg_wickets_out")
numeric_variables = df_bat[ ,numeric_columns]
cor_matrix = cor(numeric_variables, method = "pearson")
corrplot(cor_matrix, method="circle", type = "upper")

pairs(numeric_variables, main = "Scatter Plot Matrix for Numeric Variables",
      method = "pearson", 
      hist.col = "cornflowerblue",
      density = T, col='lightblue',
      ellipses = F,
      pch = 16)
#month, match_numer: strong relationship
#avg_score, avg_wickets_out: strong relationship

#label encode
library(forcats)
# Save the factor version of the columns
df_bat$team_factor <- forcats::as_factor(df_bat$team)
df_bat$opponent_factor <- forcats::as_factor(df_bat$opponent)
df_bat$venue_factor <- forcats::as_factor(df_bat$venue)
df_bat$city_factor <- forcats::as_factor(df_bat$city)

# Save levels from the training set
team_levels <- levels(df_bat$team_factor)
opponent_levels <- levels(df_bat$opponent_factor)
venue_levels <- levels(df_bat$venue_factor)
city_levels <- levels(df_bat$city_factor)

# Convert the factors to integers
df_bat$team <- as.integer(df_bat$team_factor)
df_bat$opponent <- as.integer(df_bat$opponent_factor)
df_bat$venue <- as.integer(df_bat$venue_factor)
df_bat$city <- as.integer(df_bat$city_factor)


df_bat$winner <- as.integer(forcats::as_factor(df_bat$winner))
df_bat$toss_winner <- as.integer(forcats::as_factor(df_bat$toss_winner))

#binary encode
df_bat$last_match_result <- ifelse(df_bat$last_match_result == 'win', 1, 0)
df_bat$gender <- ifelse(df_bat$gender == 'female', 0, 1)
df_bat$home_advantage <- ifelse(df_bat$home_advantage == 'yes', 1, 0)
df_bat$choose_to_bat <- ifelse(df_bat$choose_to_bat == 'yes', 1, 0)


str(df_bat)

#Descriptive statistics 
desc_stats <- data.frame(
  Min = apply(df_bat, 2, min), # minimum
  Q1 = apply(df_bat, 2, function(x) quantile(x, 0.25)), # 1st quartile (Q1)
  Med = apply(df_bat, 2, median), # median
  Mean = apply(df_bat, 2, mean), # mean
  Q3 = apply(df_bat, 2, function(x) quantile(x, 0.75)), # 3rd quartile (Q3)
  Max = apply(df_bat, 2, max), # Maximum
  SD = apply(df_bat, 2, sd) # Standard deviation
)
desc_stats <- round(desc_stats, 2)
head(desc_stats)
#Descriptive statistics is similar to summary function
summary(df_bat)

# Calculate the correlation matrix
cor_matrix <- cor(df_bat)
print(cor_matrix)

# Calculate the covariance matrix
cov_matrix <- cov(df_bat)
print(cov_matrix)



##### Data Partition #####
set.seed(180) 
train=sample(1:nrow(df_bat),floor(nrow(df_bat)*0.8))
data_train=df_bat[train,] #92
data_test=df_bat[-train,] #23

data_train$Result <- as.factor(data_train$Result)
data_test$Result <- as.factor(data_test$Result)

##### Logistic Regression #####
#The significance of a variable in a logistic regression model can be assessed by examining the p-value. 

#model-1
# Create Full Model and Variable Selection Model
null_logit_model = glm(Result~1, data=data_train, family = "binomial")
full_logit_model = glm(Result~., data=data_train, family = "binomial")
summary(full_logit_model)

#important: total_score
#The p-value for the total_score variable is less than 0.05, 
#which indicates that it is statistically significant in terms of prediction Result. 
#this means that the team's total score has a significant impact on the outcome of the game (win/lose). 

#The coefficient total_score is 0.04832.

#maybe important: match_number, season, month, and last_match_result 
#have p-values close to 0.05, 

#model-2
step(null_logit_model, scope=formula(full_logit_model), direction="forward") 
forward_step_logit_model = glm(formula = Result ~ total_score + day + gender + last_match_result + 
                                 choose_to_bat + toss_winner, family = "binomial", data = data_train)
#AIC: 102.9                               
summary(forward_step_logit_model)
#important variable: total_score, gender 
#maybe important: day, last_match_result (p-value close to 0.05)

predict_test <- predict(forward_step_logit_model, newdata = data_test, type = "response")
predicted_class <- ifelse(predict_test > 0.5, 1, 0)
predicted_class <- factor(predicted_class, levels = c(0,1))
data_test$Result <- factor(data_test$Result, levels = c(0,1))

library(caret)
confusionMatrix(predicted_class, data_test$Result)
# Accuracy : 56.52%, Sensitivity : 0.7500     

#####Naive Bayesian #####
library(e1071)

#model-3
nb_model <- naiveBayes(Result ~ ., data = data_train)
print(nb_model)

predict_test_nb <- predict(nb_model, newdata = data_test)
predicted_nb <- factor(predict_test_nb, levels = c(0,1))
confusionMatrix(predicted_nb, data_test$Result)
#Accuracy : 52.17%  , Sensitivity :0.6250  


##### CART #####
### Standard Cart-model-4
library(tree)
unpruned_tree_model = tree(Result ~., data = data_train, method = "class")
plot(unpruned_tree_model)
text(unpruned_tree_model, pretty = 0)
title("Unpruned Classification Tree", line = +3)
summary(unpruned_tree_model) 
#Misclassification error rate: 0.1087
#important varaible: 
#"total_score" "day"  "toss_winner" "avg_score"  "team" 
#"avg_wickets_out" "total_wickets_player_out" "match_number"     

### Pruned Tree-model-5
pruned_tree = cv.tree(unpruned_tree_model, FUN=prune.tree)
plot(pruned_tree)
title("Deviance as Size of Pruned Tree Increases", line = +3)

pruned_tree_model = prune.tree(unpruned_tree_model, best = 4)
summary(pruned_tree_model)
#important variable: "total_score" "day" "toss_winner"

plot(pruned_tree_model)
text(pruned_tree_model, pretty = 0)
title("Pruned Classification Tree", line = +3)


### Bagged Model-model-6
# Fine Tune Number of Bagged Trees
library(randomForest)

classification_accuracy_vec = c()
for (i in 1:200){
  bagged_model = randomForest(Result ~ ., data_train, ntree=i*5, importance =TRUE)
  predicted_labels = predict(bagged_model, data_test, type = "response")
  cm_bag = table(predicted_labels, data_test$Result)
  accuracy = (cm_bag[1,1] + cm_bag[2,2]) / nrow(data_test)
  classification_accuracy_vec[i] = accuracy
}

tree_number = seq(from = 5, to = 1000, by = 5)
colours = c(rep("black", 100))
colours[(which.max(classification_accuracy_vec))] = "green" 

plot(tree_number, classification_accuracy_vec, 
     main = "Classification Accuracy as Number of Trees in Bagging Classification Tree Increases", 
     xlab = "Number of Trees", ylab = "Classification Accuracy", pch = 16,
     col = colours) 

optimal_tree_number = tree_number[which.max(classification_accuracy_vec)] 
#optimal_tree_number: 65

bagged_model = randomForest(Result ~ ., data_train, ntree=optimal_tree_number, importance =TRUE)
importance(bagged_model) 
#"total_score", "opponent", "winner"  are the most important features 
#as they have higher values for both MeanDecreaseAccuracy and MeanDecreaseGini.



### Random Forests -model-7
num_predictors <- ncol(data_train) - 1  
classification_accuracy_vec = double(num_predictors)

for (i in 1:num_predictors){
  rf_model = randomForest(Result ~ ., data_train, mtry = i, ntree=65, importance =TRUE)
  predicted_labels = predict(rf_model, data_test, type = "response")
  cm_rf = table(predicted_labels, data_test$Result)
  accuracy = (cm_rf[1,1] + cm_rf[2,2]) / nrow(data_test)
  classification_accuracy_vec[i] = accuracy
}

colours = c(rep("black", 19))
colours[which.max(classification_accuracy_vec)] = "green"

plot(classification_accuracy_vec,
     main = "Classification Accuracy as Number of Variables Randomly Sampled at Each Node Increases", 
     xlab = "Number of Variables Avaiable at Each Node", ylab = "Classification Accuracy", pch = 16,
     col = colours) 
# Optimum is 7

rf_model = randomForest(Result ~ ., data_train, mtry = 7, ntree=65, importance =TRUE)
summary(rf_model)
names(rf_model)
importance(rf_model)
#total_score winner: the most important features 




### Boosting Tree-model-8
library(gbm)

# Find best interaction Depth - best is 4, seed - 1
accuracy_depth = double(10)
for (i in 1:10){
  boost_model =gbm(Result~.,data=data_train, distribution="multinomial",
                   n.trees =65, interaction.depth =i)
  predicted_probabilites = predict(boost_model, data_test, type = "response")
  predicted_labels = ifelse((predicted_probabilites[,1,] > predicted_probabilites[,2,])  > 0.5 , 0, 1)
  cm_rf = table(predicted_labels, data_test$Result)
  accuracy = (cm_rf[1,1] + cm_rf[2,2]) / nrow(data_test)
  accuracy_depth[i] = accuracy
}

#which.max(accuracy_depth):3
colours = c(rep("black", 10))
colours[which.max(accuracy_depth)] = "green"

plot(accuracy_depth, main = "Accuracy as Depth of Trees Increases", 
     xlab = "Depth", ylab = "Accuracy", pch = 16,
     col = colours)


# Optimise Shrinkage Parameter
shrink_index = seq(from = 0.0025, to = 0.25, by = 0.0025)
accuracy_shrinkage = c()
for (i in 1:100){
  boost_model =gbm(Result~.,data=data_train, distribution="multinomial",
                   n.trees =65, interaction.depth = 3, shrinkage = (i/400))
  predicted_probabilites = predict(boost_model, data_test, type = "response")
  predicted_labels = ifelse((predicted_probabilites[,1,] > predicted_probabilites[,2,])  > 0.5 , 0, 1)
  cm_rf = table(predicted_labels, data_test$Result)
  accuracy = (cm_rf[1,1] + cm_rf[2,2]) / nrow(data_test)
  accuracy_shrinkage[i] = accuracy
}
#which.max(accuracy_shrinkage)
colours = c(rep("black", 500))
colours[which.max(accuracy_shrinkage)] = "green"

plot(shrink_index, accuracy_shrinkage, main = "Accuracy as Shrinkage Parameter Increases",
     xlab = "Lambda (Shrinkage Parameter)",ylab = "Accuracy", pch = 16,
     col = rgb(0, 0, 0, alpha = 0.8))

optimal_depth = which.max(accuracy_depth) 
optimal_depth
optimal_shrinkage = shrink_index[which.max(accuracy_shrinkage)] 
optimal_shrinkage

boost_model =gbm(Result~.,data=data_train, distribution="multinomial",
                 n.trees =65, interaction.depth = optimal_depth, shrinkage = optimal_shrinkage)
summary(boost_model)
#total_score,day, opponent are the most important


  
### SVM Model - model-9

#Find the optimal cost parameter
cost_values <- c(0.01, 0.1, 1, 10, 100)
svm_accuracy <- vector(length = length(cost_values))

for (i in 1:length(cost_values)) {
  svm_model <- svm(Result ~ ., data = data_train, kernel = "radial", cost = cost_values[i])
  svm_pred <- predict(svm_model, newdata = data_test)
  svm_accuracy[i] <- sum(svm_pred == data_test$Result) / nrow(data_test)
}

optimal_cost <- cost_values[which.max(svm_accuracy)]
svm_model <- svm(Result ~ ., data = data_train, kernel = "radial", cost = optimal_cost)
svm_model
summary(svm_model)

svm_pred <- predict(svm_model, newdata = data_test)
confusion_matrix <- confusionMatrix(svm_pred, data_test$Result)
print(confusion_matrix)
# Accuracy : 0.6087，Sensitivity : 0.7500

accuracy <- sum(svm_pred == data_test$Result) / nrow(data_test)
precision <- confusion_matrix$byClass["Pos Pred Value"] # 0.4615385 
recall <- confusion_matrix$byClass["Sensitivity"]
f1_score <- confusion_matrix$byClass["F1"] #0.5714286 



### KNN Model - model-10
library(class)
k_values <- seq(1, 20, by = 2)
knn_accuracy <- vector(length = length(k_values))

for (i in 1:length(k_values)) {
  knn_model <- knn(train = data_train[, -1], test = data_test[, -1],
                   cl = data_train$Result, k = k_values[i])
  knn_accuracy[i] <- sum(knn_model == data_test$Result) / nrow(data_test)
}

optimal_k <- k_values[which.max(knn_accuracy)]
knn_model <- knn(train = data_train[, -1], test = data_test[, -1],
                 cl = data_train$Result, k = optimal_k)
summary(knn_model)

# KNN Feature Importance
knn_feature_importance <- numeric(ncol(data_train) - 1)
for (i in 1:(ncol(data_train) - 1)) {
  permuted_data <- data_test
  permuted_data[, i] <- sample(permuted_data[, i])
  
  permuted_knn_model <- knn(train = data_train[, -1], test = permuted_data[, -1],
                            cl = data_train$Result, k = optimal_k)
  
  knn_feature_importance[i] <- mean(permuted_knn_model != knn_model)
}
knn_feature_importance
#total_score,total_wickets_player_out,avg_wickets_out,home_advantage are important

knn_cm <- table(Actual = data_test$Result, Predicted = knn_model)
print(knn_cm)

knn_accuracy <- sum(diag(knn_cm)) / sum(knn_cm) #0.6086957
knn_recall <- knn_cm[2, 2] / sum(knn_cm[2, ]) #0.6
knn_precision <- knn_cm[2, 2] / sum(knn_cm[, 2]) #0.75
knn_f1 <- 2 * knn_precision * knn_recall / (knn_precision + knn_recall) #0.6666667



##### All Model Comparison #####
repetitions = 100 # How many times to run comparison
# Empty vectors to store respective model accuracies
accuracy_full_logit = c()
accuracy_forward_selection_logit = c()
accuracy_nb_model = c()
accuracy_tree = c()
accuracy_prune = c() 
accuracy_bag = c()
accuracy_rf = c()
accuracy_boost = c()
accuracy_svm = c()
accuracy_knn = c()

precision_full_logit = c()
precision_forward_selection_logit = c()
precision_nb_model = c()
precision_tree = c()
precision_prune = c()
precision_bag = c()
precision_rf = c()
precision_boost = c()
precision_svm = c()
precision_knn = c()

recall_full_logit = c()
recall_forward_selection_logit = c()
recall_nb_model = c()
recall_tree = c()
recall_prune = c()
recall_bag = c()
recall_rf = c()
recall_boost = c()
recall_svm = c()
recall_knn = c()

specificity_full_logit = c()
specificity_forward_selection_logit = c()
specificity_nb_model = c()
specificity_tree = c()
specificity_prune = c()
specificity_bag = c()
specificity_rf = c()
specificity_boost = c()
specificity_svm = c()
specificity_knn = c()

for (i in 1:repetitions){
  # Split Data
  train=sample(1:nrow(df_bat),floor(nrow(df_bat)*0.8))
  data_train=df_bat[train,] 
  data_test=df_bat[-train,] 
  
  data_train$Result <- as.factor(data_train$Result)
  data_test$Result <- as.factor(data_test$Result)

  # Train Models
  full_logit_model = glm(Result~., data=data_train, family = "binomial")
  
  forward_step_logit_model = glm(formula = Result ~ total_score + day + gender + last_match_result + 
                                   choose_to_bat + toss_winner, family = "binomial", data = data_train)
  
  nb_model = naiveBayes(Result ~ ., data = data_train)
  
  tree_model = tree(Result ~., data = data_train)
  
  pruned_tree_model = prune.tree(tree_model, best = 4)
  
  bagged_model = randomForest(Result ~ ., data_train, ntree=65, importance =TRUE)
  
  rf_model = randomForest(Result~ ., data_train, mtry = 7, ntree=65, importance =TRUE)
  
  boost_model =gbm(Result~.,data=data_train, distribution="multinomial",
                   n.trees =65, interaction.depth =optimal_depth , shrinkage = optimal_shrinkage)
  
  svm_model <- svm(Result ~ ., data = data_train, kernel = "radial", cost = optimal_cost)
  
  
  knn_model <- knn(train = data_train[, -1], test = data_test[, -1],
                   cl = data_train$Result, k = optimal_k)
  
  
  # Predict with models on test set
  full_logit_probabilites = predict(full_logit_model, data_test, type = "response")
  predicted_labels = ifelse(full_logit_probabilites > 0.5, 1, 0)
  cm_full_logit = confusionMatrix(factor(predicted_labels), factor(data_test$Result))
  cm_full_logit$table[1,1]
  
  forward_selection_logit_probabilites = predict(forward_step_logit_model, data_test, type = "response")
  predicted_labels = ifelse(forward_selection_logit_probabilites > 0.5, 1, 0)
  cm_forward_selection_logit = confusionMatrix(factor(predicted_labels), factor(data_test$Result))
  
  nb_model_preds = predict(nb_model, data_test, type = "class")
  cm_nb = confusionMatrix(factor(predicted_labels), factor(data_test$Result))
  
  tree_model_preds = predict(tree_model, data_test, type = "class")
  cm_tree = confusionMatrix(factor(tree_model_preds), factor(data_test$Result))
  
  pruned_tree_model_preds = predict(pruned_tree_model, data_test, type = "class")
  cm_pruned = confusionMatrix(factor(pruned_tree_model_preds), (data_test$Result))
  
  bagged_predicted_labels = predict(bagged_model, data_test, type = "response")
  cm_bagged = confusionMatrix(factor(bagged_predicted_labels), factor(data_test$Result))
  
  rf_predicted_labels = predict(rf_model, data_test, type = "response")
  cm_rf = confusionMatrix(factor(rf_predicted_labels), factor(data_test$Result))
  
  boost_predicted_probabilites = predict(boost_model, data_test, type = "response")
  boost_predicted_labels = ifelse((boost_predicted_probabilites[,1,] > boost_predicted_probabilites[,2,])  > 0.5 , 0, 1)
  cm_boost = confusionMatrix(factor(boost_predicted_labels), factor(data_test$Result))
  
  svm_pred <- predict(svm_model, newdata = data_test)
  cm_svm <- confusionMatrix(factor(svm_pred), factor(data_test$Result))
  
  cm_knn <- confusionMatrix(factor(knn_model), factor(data_test$Result))
  
  
  
  # Compute and Store Accuracy Metrics
  
  accuracy_full_logit[i] = (cm_full_logit$table[1,1] + cm_full_logit$table[2,2]) / nrow(data_test) 
  accuracy_forward_selection_logit[i] = (cm_forward_selection_logit$table[1,1] + cm_forward_selection_logit$table[2,2]) / nrow(data_test) 
  accuracy_nb_model[i] = (cm_nb$table[1,1] + cm_nb$table[2,2]) / nrow(data_test) 
  accuracy_tree[i] = (cm_tree$table[1,1] + cm_tree$table[2,2]) / nrow(data_test) 
  accuracy_prune[i] = (cm_pruned$table[1,1] + cm_pruned$table[2,2]) /nrow(data_test) 
  accuracy_bag[i] = (cm_bagged$table[1,1] + cm_bagged$table[2,2]) / nrow(data_test) 
  accuracy_rf[i] = (cm_rf$table[1,1] + cm_rf$table[2,2]) / nrow(data_test) 
  accuracy_boost[i] = (cm_boost$table[1,1] + cm_boost$table[2,2]) / nrow(data_test) 
  accuracy_svm[i] = (cm_svm$table[1,1] + cm_svm$table[2,2]) / nrow(data_test) 
  accuracy_knn[i] = (cm_knn$table[1,1] + cm_knn$table[2,2]) / nrow(data_test) 
  
  precision_full_logit[i] = cm_full_logit$byClass["Pos Pred Value"]
  precision_forward_selection_logit[i] = cm_forward_selection_logit$byClass["Pos Pred Value"]
  precision_nb_model[i] = cm_nb$byClass["Pos Pred Value"]
  precision_tree[i] = cm_tree$byClass["Pos Pred Value"]
  precision_prune[i] = cm_pruned$byClass["Pos Pred Value"]
  precision_bag[i] = cm_bagged$byClass["Pos Pred Value"]
  precision_rf[i] = cm_rf$byClass["Pos Pred Value"]
  precision_boost[i] = cm_boost$byClass["Pos Pred Value"]
  precision_svm[i] = cm_svm$byClass["Pos Pred Value"]
  precision_knn[i] = cm_knn$byClass["Pos Pred Value"]
  
  recall_full_logit[i] = cm_full_logit$byClass["Sensitivity"]
  recall_forward_selection_logit[i] = cm_forward_selection_logit$byClass["Sensitivity"]
  recall_nb_model[i] = cm_nb$byClass["Sensitivity"]
  recall_tree[i] = cm_tree$byClass["Sensitivity"]
  recall_prune[i] = cm_pruned$byClass["Sensitivity"]
  recall_bag[i] = cm_bagged$byClass["Sensitivity"]
  recall_rf[i] = cm_rf$byClass["Sensitivity"]
  recall_boost[i] = cm_boost$byClass["Sensitivity"]
  recall_svm[i] = cm_svm$byClass["Sensitivity"]
  recall_knn[i] = cm_knn$byClass["Sensitivity"]
  
  specificity_full_logit[i] = cm_full_logit$byClass["Specificity"]
  specificity_forward_selection_logit[i] = cm_forward_selection_logit$byClass["Specificity"]
  specificity_nb_model[i] = cm_nb$byClass["Specificity"]
  specificity_tree[i] = cm_tree$byClass["Specificity"]
  specificity_prune[i] = cm_pruned$byClass["Specificity"]
  specificity_bag[i] = cm_bagged$byClass["Specificity"]
  specificity_rf[i] = cm_rf$byClass["Specificity"]
  specificity_boost[i] = cm_boost$byClass["Specificity"]
  specificity_svm[i] = cm_svm$byClass["Specificity"]
  specificity_knn[i] = cm_knn$byClass["Specificity"]
}

# Compare
par(las = 3) 
boxplot(accuracy_full_logit,
        accuracy_forward_selection_logit, 
        accuracy_nb_model,
        accuracy_tree, 
        accuracy_prune, 
        accuracy_bag, 
        accuracy_rf, 
        accuracy_boost,
        accuracy_svm, 
        accuracy_knn,
        names = c('Full Logit',
                  'Forward Selection Logit',
                  'Naive Bayesian',
                  'Unpruned Tree',
                  'Pruned Tree',
                  'Bagged Tree', 
                  'Random Forest Tree',
                  "Boosting Tree",
                  "Support Vector Machine",
                  "K-Nearest Neighbors" ),
        main = "Test Accuracy Distribution for All Models",
        xlab = "Model Type", ylab = 'Test Accuracy',
        col = viridis(10))





mean(accuracy_full_logit)
mean(accuracy_forward_selection_logit)
mean(accuracy_nb_model)
mean(accuracy_tree) 
mean(accuracy_prune) 
mean(accuracy_bag) 
mean(accuracy_rf) 
mean(accuracy_boost) 
mean(accuracy_svm) #max:0.6895652
mean(accuracy_knn)

mean(precision_full_logit)
mean(precision_forward_selection_logit)
mean(precision_nb_model)
mean(precision_tree)
mean(precision_prune)
mean(precision_bag)
mean(precision_rf)
mean(precision_boost)
mean(precision_svm) #max:0.7081313
mean(precision_knn)

mean(recall_full_logit) 
mean(recall_forward_selection_logit) #max:0.7525716
mean(recall_nb_model) #max:0.7525716
mean(recall_tree)
mean(recall_prune)
mean(recall_bag)
mean(recall_rf)
mean(recall_boost)
mean(recall_svm)
mean(recall_knn)

mean(specificity_full_logit)
mean(specificity_forward_selection_logit)
mean(specificity_nb_model)
mean(specificity_tree)
mean(specificity_prune)
mean(specificity_bag)
mean(specificity_rf)
mean(specificity_boost)
mean(specificity_svm) #max:0.633744
mean(specificity_knn)





#### from the begining of the match
# Define the encoding
team_encoding <- setNames(1:length(team_levels), team_levels)
opponent_encoding <- setNames(1:length(opponent_levels), opponent_levels)
venue_encoding <- setNames(1:length(venue_levels), venue_levels)
city_encoding <- setNames(1:length(city_levels), city_levels)

# Apply the encoding to the training set
df_bat$team <- team_encoding[as.character(df_bat$team)]
df_bat$opponent <- opponent_encoding[as.character(df_bat$opponent)]
df_bat$venue <- venue_encoding[as.character(df_bat$venue)]
df_bat$city <- city_encoding[as.character(df_bat$city)]

# Load the new prediction set
df_2023 <- read.csv("~/Desktop/df_2023.csv")

# Apply the encoding to the prediction set
df_2023$team <- team_encoding[as.character(df_2023$team)]
df_2023$opponent <- opponent_encoding[as.character(df_2023$opponent)]
df_2023$venue <- venue_encoding[as.character(df_2023$venue)]
df_2023$city <- city_encoding[as.character(df_2023$city)]

# Convert binary variables to numeric
df_2023$gender <- ifelse(df_2023$gender == 'female', 0, 1)
df_2023$home_advantage <- ifelse(df_2023$home_advantage == 'yes', 1, 0)




