library(skimr)
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
library(factoextra)
library(FactoMineR)
library(tidyverse)
library(ggplot2)
library( cluster)
library(RColorBrewer)

#loading the data
df_bat <- read.csv("~/Desktop/final_dataframe.csv")
df_bat

df <- read.csv("~/Desktop/full_dataframe.csv")
df

#overview of data
skim(df_bat)
names(df_bat)
str(df_bat)

# drop column 'Result_num' 'toss_decision' 'choose_to_field' 'forced_to_field' 
#These three variables do not need to be considered as they have only one value
df_bat <- df_bat[ , !(names(df_bat) %in% c("Result_num", "toss_decision", "choose_to_field", "forced_to_field"))]

#Check missing values
colSums(is.na(df_bat))
#avg_win_rate,avg_score,avg_wickets_out missing values:8
#This is normal, as there is no historical data for the first games of 8 teams

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

##### EDA #####

### Missingness ###
image(is.na(df_bat))
colSums(is.na(df_bat))
library(naniar)
gg_miss_var(df_bat,show_pct = TRUE)
library(visdat)
vis_dat(df_bat)
vis_miss(df_bat)
dev.off()
library(mice)
options(width = 100) 
md.pattern(df_bat)

df_bat$avg_win_rate <- replace(df_bat$avg_win_rate, is.na(df_bat$avg_win_rate), 0)
df_bat$avg_score <- replace(df_bat$avg_score, is.na(df_bat$avg_score), 0)
df_bat$avg_wickets_out <- replace(df_bat$avg_wickets_out, is.na(df_bat$avg_wickets_out), 0)

### Categorical Variables ###
#group1: 'winner' 'toss_winner' 'team' 'opponent' 'venue' 'city'
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


#group2: binary variable
#'gender' 'season' 'home_advantage' 'choice_to_bat''forced_to_bat' ‘last_match_result’
table(df_bat$last_match_result)
last_match_result_bp = barplot(table(df_bat$last_match_result), col = viridis(3),ylim = c(0,100),
                            main = "Number of Observations in 'last match result' Factor Levels", cex.main = 1.5,
                            names.arg = c("none", "loose", "win"),
                            xlab = "Last Match Result",ylab = "Observation Count", cex.lab = 1.5, cex.names = 2.0)
text(last_match_result_bp, c(table(df_bat$last_match_result)), table(df_bat$last_match_result), cex=1.5, pos=3)

color_palette <- brewer.pal(2, "Set2")
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

# Forced to bat
forced_to_bat_freq <- table(df_bat$forced_to_bat)
lbls <- paste(names(forced_to_bat_freq), "\n", round(forced_to_bat_freq/sum(forced_to_bat_freq)*100, 1), "%")
pie(forced_to_bat_freq, main = "Pie Chart of forced_to_bat", col = color_palette, labels = lbls)

ggplot(df_bat, aes(home_advantage, total_score)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot of total_score by home_advantage",
       x = "home_advantage",
       y = "total_score") +
  theme_minimal()

t.test(total_score ~ home_advantage, data = df_bat)

boxplot(df_bat$total_score ~ df_bat$team)
boxplot(df_bat$total_wickets_player_out ~ df_bat$team)



### Numerical Variables ###
#"match_number"  "total_score" "total_wickets_player_out" "month" "day" "avg_win_rate" "avg_score" "avg_wickets_out"
numeric_columns=c("match_number","total_score","total_wickets_player_out","month","day",
                  "avg_win_rate","avg_score","avg_wickets_out")
numeric_variables = df_bat[ ,numeric_columns]

# corrplot
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
df_bat$team <- as.integer(factor(df_bat$team))
df_bat$opponent <- as.integer(factor(df_bat$opponent))
df_bat$winner <- as.integer(factor(df_bat$winner))
df_bat$toss_winner <- as.integer(factor(df_bat$toss_winner))
df_bat$venue <- as.integer(factor(df_bat$venue))
df_bat$city <- as.integer(factor(df_bat$city))

#binary encode
df_bat$last_match_result <- ifelse(df_bat$last_match_result == 'win', 1, 0)
df_bat$gender <- ifelse(df_bat$gender == 'female', 0, 1)
df_bat$home_advantage <- ifelse(df_bat$home_advantage == 'yes', 1, 0)
df_bat$choose_to_bat <- ifelse(df_bat$choose_to_bat == 'yes', 1, 0)
df_bat$forced_to_bat <- ifelse(df_bat$forced_to_bat  == 'yes', 1, 0)

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
data_train=df_bat[train,] #98
data_test=df_bat[-train,] #25

data_train$Result <- as.factor(data_train$Result)
data_test$Result <- as.factor(data_test$Result)

##### Logistic Regression #####
# Create Full Model and Variable Selection Model
null_logit_model = glm(Result~1, data=data_train, family = "binomial")
full_logit_model = glm(Result~., data=data_train, family = "binomial")
summary(full_logit_model)
#important variable: gender,total_score, total_wickets_player_out,avg_score, last_match_result  

step(null_logit_model, scope=formula(full_logit_model), direction="forward") 

forward_step_logit_model = glm(formula = Result ~ total_wickets_player_out + total_score  + 
                                 gender +  last_match_result +forced_to_bat, family = "binomial", data =data_train)

summary(forward_step_logit_model)
#important variable: total_score,last_match_result, gender 

predict_test <- predict(forward_step_logit_model, newdata = data_test, type = "response")
predicted_class <- ifelse(predict_test > 0.5, 1, 0)
predicted_class <- factor(predicted_class, levels = c(0,1))
data_test$Result <- factor(data_test$Result, levels = c(0,1))

confusionMatrix(predicted_class, data_test$Result)
# Accuracy : 0.56，Sensitivity : 0.6364

##### Plain Bayesian #####
library(e1071)
nb_model <- naiveBayes(Result ~ ., data = data_train)
predicted_nb <- predict(nb_model, newdata = data_test)
confusionMatrix(predicted_nb, data_test$Result)
#Accuracy : 0.52, Sensitivity : 0.5455

##### CART #####
### Standard Cart
unpruned_tree_model = tree(Result ~., data = data_train, method = "class")
plot(unpruned_tree_model)
text(unpruned_tree_model, pretty = 0)
title("Unpruned Classification Tree", line = +3)
summary(unpruned_tree_model) #Misclassification error rate: 0.1224
#important varaible: "total_wickets_player_out" "total_score"              "winner"                  
# "avg_wickets_out"          "match_number"             "avg_win_rate"            
# "avg_score" 


### Pruned Tree
pruned_tree = cv.tree(unpruned_tree_model, FUN=prune.tree)
plot(pruned_tree)
title("Deviance as Size of Pruned Tree Increases", line = +3)

pruned_tree_model = prune.tree(unpruned_tree_model, best = 3)
summary(pruned_tree_model)
#important variable: "total_wickets_player_out" "avg_win_rate" 

plot(pruned_tree_model)
text(pruned_tree_model, pretty = 0)
title("Pruned Classification Tree", line = +3)


### Bagged Model
# Fine Tune Number of Bagged Trees
classification_accuracy_vec = c()

for (i in 1:100){
  bagged_model = randomForest(Result ~ ., data_train, ntree=i*5, importance =TRUE)
  predicted_labels = predict(bagged_model, data_test, type = "response")
  cm_bag = table(predicted_labels, data_test$Result)
  accuracy = (cm_bag[1,1] + cm_bag[2,2]) / nrow(data_test)
  classification_accuracy_vec[i] = accuracy
}

tree_number = seq(from = 5, to = 500, by = 5)
colours = c(rep("black", 100))
colours[(which.max(classification_accuracy_vec))] = "green" 

plot(tree_number, classification_accuracy_vec, 
     main = "Classification Accuracy as Number of Trees in Bagging Classification Tree Increases", 
     xlab = "Number of Trees", ylab = "Classification Accuracy", pch = 16,
     col = colours) 

optimal_tree_number = tree_number[which.max(classification_accuracy_vec)] 
#160
bagged_model = randomForest(Result ~ ., data_train, ntree=optimal_tree_number, importance =TRUE)
importance(bagged_model) 
#total_wickets_player_out, total_score and winner are the most important features 
#as they have high values in the MeanDecreaseAccuracy and MeanDecreaseGini columns


### Random Forests
### Random Forests
classification_accuracy_vec = double(20)
for (i in 1:20){
  rf_model = randomForest(Result ~ ., data_train, mtry = i, ntree=160, importance =TRUE)
  predicted_labels = predict(rf_model, data_test, type = "response")
  cm_rf = table(predicted_labels, data_test$Result)
  accuracy = (cm_rf[1,1] + cm_rf[2,2]) / nrow(data_test)
  classification_accuracy_vec[i] = accuracy
}

colours = c(rep("black", 20))
colours[which.max(classification_accuracy_vec)] = "green"

plot(classification_accuracy_vec,
     main = "Classification Accuracy as Number of Variables Randomly Sampled at Each Node Increases", 
     xlab = "Number of Variables Avaiable at Each Node", ylab = "Classification Accuracy", pch = 16,
     col = colours) 
# Optimum is 10

rf_model = randomForest(Result ~ ., data_train, mtry = 10, ntree=160, importance =TRUE)
summary(rf_model)
names(rf_model)
importance(rf_model)
#total_wickets_player_out, total_score and winner, avg_win_rate
#are the most important features 

### Boosted Tree
# Find best interaction Depth - best is 4, seed - 1
accuracy_depth = double(10)
for (i in 1:10){
  boost_model =gbm(Result~.,data=data_train, distribution="multinomial",
                   n.trees =160, interaction.depth =i)
  predicted_probabilites = predict(boost_model, data_test, type = "response")
  predicted_labels = ifelse((predicted_probabilites[,1,] > predicted_probabilites[,2,])  > 0.5 , 0, 1)
  cm_rf = table(predicted_labels, data_test$Result)
  accuracy = (cm_rf[1,1] + cm_rf[2,2]) / nrow(data_test)
  accuracy_depth[i] = accuracy
}
#which.max(accuracy_depth):2
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
                   n.trees =160, interaction.depth = 2, shrinkage = (i/400))
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
     col = alpha(colours,0.8))

optimal_depth = which.max(accuracy_depth)
optimal_shrinkage = shrink_index[which.max(accuracy_shrinkage)]

boost_model =gbm(Result~.,data=data_train, distribution="multinomial",
                 n.trees =160, interaction.depth = optimal_depth, shrinkage = optimal_shrinkage)
summary(boost_model)
#total_score,total_wickets_player_out,avg_wickets_out,avg_score, avg_win_rate, day, team
#are the most important

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

precision_full_logit = c()
precision_forward_selection_logit = c()
precision_nb_model = c()
precision_tree = c()
precision_prune = c()
precision_bag = c()
precision_rf = c()
precision_boost = c()

recall_full_logit = c()
recall_forward_selection_logit = c()
recall_nb_model = c()
recall_tree = c()
recall_prune = c()
recall_bag = c()
recall_rf = c()
recall_boost = c()

specificity_full_logit = c()
specificity_forward_selection_logit = c()
specificity_nb_model = c()
specificity_tree = c()
specificity_prune = c()
specificity_bag = c()
specificity_rf = c()
specificity_boost = c()

for (i in 1:repetitions){
  # Split Data
  train=sample(1:nrow(df_bat),floor(nrow(df_bat)*0.8))
  data_train=df_bat[train,] #98
  data_test=df_bat[-train,] 
  
  data_train$Result <- as.factor(data_train$Result)
  data_test$Result <- as.factor(data_test$Result)

  # Train Models
  full_logit_model = glm(Result~., data=data_train, family = "binomial")
  
  forward_step_logit_model = glm(formula = Result ~ total_wickets_player_out + total_score  + 
                                   gender +  last_match_result +forced_to_bat, 
                                 family = "binomial", 
                                 data =data_train)
  
  nb_model = naiveBayes(Result ~ ., data = data_train)
  
  tree_model = tree(Result ~., data = data_train)
  
  pruned_tree_model = prune.tree(tree_model, best = 3)
  
  bagged_model = randomForest(Result ~ ., data_train, ntree=160, importance =TRUE)
  
  rf_model = randomForest(Result~ ., data_train, mtry = 10, ntree=160, importance =TRUE)
  
  boost_model =gbm(Result~.,data=data_train, distribution="multinomial",
                   n.trees =160, interaction.depth =2, shrinkage = 0.0125)
  
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
  
  # Compute and Store Accuracy Metrics
  
  accuracy_full_logit[i] = (cm_full_logit$table[1,1] + cm_full_logit$table[2,2]) / nrow(data_test) 
  accuracy_forward_selection_logit[i] = (cm_forward_selection_logit$table[1,1] + cm_forward_selection_logit$table[2,2]) / nrow(data_test) 
  accuracy_nb_model[i] = (cm_nb$table[1,1] + cm_nb$table[2,2]) / nrow(data_test) 
  accuracy_tree[i] = (cm_tree$table[1,1] + cm_tree$table[2,2]) / nrow(data_test) 
  accuracy_prune[i] = (cm_pruned$table[1,1] + cm_pruned$table[2,2]) /nrow(data_test) 
  accuracy_bag[i] = (cm_bagged$table[1,1] + cm_bagged$table[2,2]) / nrow(data_test) 
  accuracy_rf[i] = (cm_rf$table[1,1] + cm_rf$table[2,2]) / nrow(data_test) 
  accuracy_boost[i] = (cm_boost$table[1,1] + cm_boost$table[2,2]) / nrow(data_test) 
  
  precision_full_logit[i] = cm_full_logit$byClass["Pos Pred Value"]
  precision_forward_selection_logit[i] = cm_forward_selection_logit$byClass["Pos Pred Value"]
  precision_nb_model[i] = cm_nb$byClass["Pos Pred Value"]
  precision_tree[i] = cm_tree$byClass["Pos Pred Value"]
  precision_prune[i] = cm_pruned$byClass["Pos Pred Value"]
  precision_bag[i] = cm_bagged$byClass["Pos Pred Value"]
  precision_rf[i] = cm_rf$byClass["Pos Pred Value"]
  precision_boost[i] = cm_boost$byClass["Pos Pred Value"]
  
  recall_full_logit[i] = cm_full_logit$byClass["Sensitivity"]
  recall_forward_selection_logit[i] = cm_forward_selection_logit$byClass["Sensitivity"]
  recall_nb_model[i] = cm_nb$byClass["Sensitivity"]
  recall_tree[i] = cm_tree$byClass["Sensitivity"]
  recall_prune[i] = cm_pruned$byClass["Sensitivity"]
  recall_bag[i] = cm_bagged$byClass["Sensitivity"]
  recall_rf[i] = cm_rf$byClass["Sensitivity"]
  recall_boost[i] = cm_boost$byClass["Sensitivity"]
  
  specificity_full_logit[i] = cm_full_logit$byClass["Specificity"]
  specificity_forward_selection_logit[i] = cm_forward_selection_logit$byClass["Specificity"]
  specificity_nb_model[i] = cm_nb$byClass["Specificity"]
  specificity_tree[i] = cm_tree$byClass["Specificity"]
  specificity_prune[i] = cm_pruned$byClass["Specificity"]
  specificity_bag[i] = cm_bagged$byClass["Specificity"]
  specificity_rf[i] = cm_rf$byClass["Specificity"]
  specificity_boost[i] = cm_boost$byClass["Specificity"]
}

# Compare
boxplot(accuracy_full_logit,accuracy_forward_selection_logit, accuracy_nb_model,
        accuracy_tree, accuracy_prune, accuracy_bag, accuracy_rf, accuracy_boost,
        names = c('Full Logit',
                  'Forward Selection Logit',
                  'Plain Bayesian',
                  'Unpruned Tree',
                  'Pruned Tree',
                  'Bagged Tree', 
                  'Random Forest Tree',
                  "Boosted Tree"),
        main = "Test Accuracy Distribution for All Models",
        xlab = "Model Type", ylab = 'Test Accuracy',
        col = viridis(8))


mean(accuracy_full_logit)
mean(accuracy_forward_selection_logit)
mean(accuracy_nb_model)
mean(accuracy_tree) 
mean(accuracy_prune) 
mean(accuracy_bag) 
mean(accuracy_rf) 
mean(accuracy_boost) 

mean(precision_full_logit)
mean(precision_forward_selection_logit)
mean(precision_nb_model)
mean(precision_tree)
mean(precision_prune)
mean(precision_bag)
mean(precision_rf)
mean(precision_boost)

mean(recall_full_logit)
mean(recall_forward_selection_logit)
mean(recall_nb_model)
mean(recall_tree)
mean(recall_prune)
mean(recall_bag)
mean(recall_rf)
mean(recall_boost)

mean(specificity_full_logit)
mean(specificity_forward_selection_logit)
mean(specificity_nb_model)
mean(specificity_tree)
mean(specificity_prune)
mean(specificity_bag)
mean(specificity_rf)
mean(specificity_boost)



#### PCA ###
bat_df <- df_bat[,-1] #delete 'Result'
str(bat_df)
dim(bat_df)

#Standardised the dataset
apply(bat_df, 2, sd)
apply(bat_df, 2, mean) #Needs to be standardised
#They are vastly different, so there is need for scaling.

df.scaled <- scale(bat_df) #crucial pre-processing step
#Not normally distributed so need to scale

#PCA: The singular value decomposition in the principal component analysisis performed below
#while prcomp() uses singular value decomposition (svd), which can be more stable in some cases.
pr.out<-prcomp(bat_df,scale=TRUE)
summary(pr.out) # PC9: 0.80058

pr.out$center #Means and standard deviations of variables before standardisation.
pr.out$scale #Means and standard deviations of variables after standardisation.

#eigenvalues
pr.out$sdev^2
#PC loadings  vector
pr.out$rotation
#PC score
pr.out$x

summary(pr.out)
head(pr.out$x) #6 PC score vectors

#The Standard deviation of the principal components is the square root of the corresponding eigenvalues.
sd_all <- summary(pr.out)$importance[1,] #Standard deviation
sd_all^2/sum(sd_all^2) #pr.out$sdev

#scree plot
library(factoextra)
fviz_screeplot(pr.out, addlabels = TRUE) 
#The rule of thumb recommends retaining the component that explains 80-90% of the variability in the original data set

pr.out$sdev #Standard deviation of each principal component
pr.var=pr.out$sdev^2
pr.var  #Variance explained by each principal component
pve=pr.var/sum(pr.var)
pve 
par(mfrow=c(1,2))
plot(pve,xlab='Principal Component',ylab='Proportion of Variance Explained',type='b',
     main='Proportion of variance explained')
plot(cumsum(pve),xlab='Principal Component',
     ylab='Cumulative Proportion of Variance Explained',type='b',
     main='Cummulative proportion of variance explained')
dev.off()
plot(summary(pr.out)$importance[2,], 
     type="b", xlab="PCs", ylab="Variability explained")
#The summary shows that 9 components are enough to keep 80% of the variability in data. 
#Also choosing 9 principal components seems reasonable based on the so-called elbow method.

#biplot
fviz_pca_biplot(pr.out,addlabels = TRUE)

#Correlation between variables and PCs
var <- get_pca_var(pr.out)
var$cor  #Correlation between variables and PC
cor(bat_df,pr.out$x[,1:2]) #Variable Correlation Chart
fviz_pca_var(pr.out) 
fviz_pca_var(pr.out, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800"),
             repel = TRUE)

#Cos2 between variables and PCs
var$cos2 #quality of representation (var$cor)^2
cos2 <- (cor(bat_df, pr.out$x[,1:2]))^2
cos2
fviz_cos2(pr.out,choice='var',axes=1) 
fviz_cos2(pr.out,choice='var',axes=2)

##Contribution between variables and PCs
var$contrib 
#any variable whose height is up to 16.66 and 
#above is considered to have contributed significantly to the component.
fviz_contrib(pr.out, choice = "var", axes = 1, top = 10) #Variable contribution to PC1
fviz_contrib(pr.out, choice = "var", axes = 2, top = 10) #Variable contribution to PC2


#### Cluster analysis ###
df.scaled <- scale(bat_df)
#Choose the best k in the k-means algorithm, using the sum of the sums of squares within the clusters
fviz_nbclust(df.scaled, kmeans, method = "wss")+
  geom_vline( xintercept  =  3 , linetype  =  2 )
#total within-cluster sum of square (wss)
fviz_nbclust(df.scaled,kmeans, method = "silhouette")+
  labs(title = "K-means")

# Compute k-means with k = 2
set.seed(123)
k2 <- kmeans(df.scaled, 2, nstart = 25)
names(k2)

k2$centers 
k2$iter 
k2$size #115   8
k2$withinss #2057.6895   83.0065
k2$tot.withinss #2140.696
k2$totss #2440
k2$betweenss #299.304

#Percentage of variance explained by the cluster means = (between_ss/total_ss)*100
# k2$betweenss=k2$totss -k2$tot.withinss
# 0.1226656= (k2$totss-k2$tot.withinss)/k2$totss= k2$betweenss/k2$totss

cols=c('red','darkgreen')
plot(df.scaled,col=cols[k2$cluster],main='K-means clustering with 2 clusters',xlab='',ylab='')
points(k2$centers,pch=19,cex=2,col=cols) #k=2 is not good enough, there is overlap

#Find the mean of 6 variables based on the newly created cluster = k2$centers #2 centroids under 6 variables (6 dimensions)
aggregate(scale(bat_df), by=list(cluster=k2$cluster), mean)

# Visualise kmeans clustering
fviz_cluster(k2, df.scaled, ellipse.type = "norm")

# Visualize the k-means clustering results using PCA
pca_scores <- as.data.frame(pr.out$x[, 1:2])
pca_scores$cluster <- k2$cluster
pca_cluster_plot <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = as.factor(cluster))) +
  geom_point() +
  labs(title = "K-means Clustering Results using PCA",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Cluster") +
  theme_minimal()
print(pca_cluster_plot)
#Looking at the results of k-means on the first 2 principal components is very intuitive

true_labels = df_bat[,1]
combined_data <- cbind(pca_scores, true_labels)
cluster_plot <- ggplot(combined_data, aes(x = PC1, y = PC2, color = as.factor(cluster), shape = as.factor(true_labels))) +
  geom_point() +
  labs(title = "K-means Clustering Results with True Labels",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Cluster",
       shape = "True Label") +
  theme_minimal()
print(cluster_plot)






