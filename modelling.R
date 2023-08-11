##### Half-time Modelling #####
#Setting the working directory
setwd("/Users/quinne/Desktop/dissertation_dataset")
getwd()

library(dplyr)
library(tidyr)
library(ggplot2)
library(skimr)
library(corrplot)
library(viridis)  
library(RColorBrewer)
library(gridExtra)
library(naniar)
library(visdat)
library(mice)
library(vcd)
library(lattice)
library(factoextra)
library(FactoMineR)
library(tidyverse)
library(cluster)
library(pROC)

##### Import Data and Make Suitable for Analysis #####
# Import Data
df <- read.csv("~/Desktop/dissertation_dataset/double_dataframe.csv")
df_bat <- df[df$toss_decision == "bat", ]
head(df_bat)

###Drop columns that don't make sense for half-time model
df_bat <- df_bat[, -(which(names(df) %in% c("toss_decision", 
                                            "choose_to_field", 
                                            "forced_to_field")))]
### New variable: 'days_from_start'
### converting time variables like months and days to the first day from the start of each season
df_bat$year <- df_bat$season
df_bat$date <- as.Date(with(df_bat, paste(year, month, day, sep="-")), "%Y-%m-%d")

get_start_date <- function(season, gender) {
  if (season == 2021 && gender == "female") return(as.Date("2021-07-21"))
  if (season == 2021 && gender == "male") return(as.Date("2021-07-22"))
  if (season == 2022 && gender == "female") return(as.Date("2022-08-11"))
  if (season == 2022 && gender == "male") return(as.Date("2022-08-03"))
}

df_bat$days_from_start <- mapply(function(season, gender, date) {
  start_date <- get_start_date(season, gender)
  as.numeric(difftime(date, start_date, units = "days"))
}, df_bat$season, df_bat$gender, df_bat$date)

df_bat$year <- NULL
df_bat$month <- NULL
df_bat$day <- NULL
df_bat$date <- NULL

# Overview of data structure
skim(df_bat) 
dim(df_bat) #123 22
names(df_bat)
str(df_bat)
summary(df_bat)

class(df_bat[2,20])
is.factor(df_bat$Result)

# Change to correct type
df_bat$Result <- as.factor(df_bat$Result)
table(df_bat$Result) 

# Binary categorical variable: group1
df_bat$gender = as.factor(df_bat$gender)
df_bat$home_advantage = as.factor(df_bat$home_advantage)

df_bat$last_match_result <- trimws(df_bat$last_match_result)
df_bat$last_match_result <- dplyr::na_if(df_bat$last_match_result, "") #na:8
df_bat$last_match_result = as.factor(df_bat$last_match_result)
#levels(df_bat$last_match_result) <- c(0,1)
table(df_bat$last_match_result) 

df_bat$choose_to_bat = as.factor(df_bat$choose_to_bat)
df_bat$forced_to_bat = as.factor(df_bat$forced_to_bat)

# Multicategorical variable: group2
df_bat$team = as.factor(df_bat$team)
df_bat$opponent = as.factor(df_bat$opponent)
df_bat$toss_winner = as.factor(df_bat$toss_winner)
df_bat$winner = as.factor(df_bat$winner)

df_bat$venue = as.factor(df_bat$venue)
df_bat$city = as.factor(df_bat$city)

str(df_bat)
skim(df_bat)  #123 22


##### EDA #####
### Missingness ###
image(is.na(df_bat))
colSums(is.na(df_bat))
gg_miss_var(df_bat,show_pct = TRUE) #avg_win_rate, avg_wickets_out, avg_score
vis_dat(df_bat)
vis_miss(df_bat)
dev.off()
options(width = 100) 
md.pattern(df_bat)

#Check for missing values:
sum(is.na(df_bat)) 
#32 missing values- find where missing values are:
which(is.na(df_bat))
#Check by columns:
colMissingData <- sapply(df_bat, anyNA)


#Dealing with missing values：8/123 almost 6.5% of bat data
df_bat <- na.omit(df_bat) 
#2021 the first four matches no history performance variables
dim(df_bat) #115 22
str(df_bat)
table(is.na(df_bat))

### EDA: Variables Visualisation ###
color_palette <- colorRampPalette(c("lightblue", "darkblue"))

### Target Variable ###
result_bp = barplot(table(df_bat$Result), col = color_palette(2) , ylim = c(0,100),
                    main = "Number of Observations in 'Result' Factor Levels", cex.main = 1.5,
                    names.arg = c( "lose", "win"),
                    xlab = "Result",ylab = "Observation Count", cex.lab = 1.5, cex.names = 2.0)
text(result_bp, c(table(df_bat$Result)), table(df_bat$Result), cex=1.5, pos=3)

result_freq <- table(df_bat$Result)
lbls <- paste(names(result_freq), "\n", round(result_freq/sum(result_freq)*100, 1), "%")
pie(result_freq, main = "Pie Chart of Result", col =color_palette(2), labels = lbls)
#55% lose vs 45% win 
#balance


### Categorical Variables ###
###Group1: binary categorical variables

# last match result
last_result_freq <- table(df_bat$last_match_result)
lbls <- paste(names(last_result_freq), "\n", round(last_result_freq/sum(last_result_freq)*100, 1), "%")
pie(last_result_freq, main = "Pie Chart of Last Match Result", col =color_palette(2), labels = lbls)
#balance

# Gender
gender_freq <- table(df_bat$gender)
lbls <- paste(names(gender_freq), "\n", round(gender_freq/sum(gender_freq)*100, 1), "%")
pie(gender_freq, main = "Pie Chart of Gender", col = color_palette(2), labels = lbls)
#balance

# Season
season_freq <- table(df_bat$season)
lbls <- paste(names(season_freq), "\n", round(season_freq/sum(season_freq)*100, 1), "%")
pie(season_freq, main = "Pie Chart of Season", col = color_palette(2), labels = lbls)
#balance

# Home Advantage
home_advantage_freq <- table(df_bat$home_advantage)
lbls <- paste(names(home_advantage_freq), "\n", round(home_advantage_freq/sum(home_advantage_freq)*100, 1), "%")
pie(home_advantage_freq, main = "Pie Chart of Home Advantage", col = color_palette(2), labels = lbls)
#no 61% yes 39% 
#not balance, describes the data structure

# Choose to bat
choose_to_bat_freq <- table(df_bat$choose_to_bat)
lbls <- paste(names(choose_to_bat_freq), "\n", round(choose_to_bat_freq/sum(choose_to_bat_freq)*100, 1), "%")
pie(choose_to_bat_freq, main = "Pie Chart of choose_to_bat", col = color_palette(2), labels = lbls)
#no 72% yes 28%
#not balance, describes the data structure

#Forced to bat
forced_to_bat_freq <- table(df_bat$forced_to_bat)
lbls <- paste(names(forced_to_bat_freq), "\n", round(forced_to_bat_freq/sum(forced_to_bat_freq)*100, 1), "%")
pie(forced_to_bat_freq, main = "Pie Chart of forced_to_bat", col = color_palette(2), labels = lbls)
#yes 72%, no 28%
#not balance, describes the data structure


### Does choose_to_bat & forced_to_bat completely opposite?
df_bat$choose_to_bat_numeric <- ifelse(df_bat$choose_to_bat == "yes", 1, 0)
df_bat$forced_to_bat_numeric <- ifelse(df_bat$forced_to_bat == "yes", 1, 0)

correlation_test <- cor.test(df_bat$choose_to_bat_numeric, df_bat$forced_to_bat_numeric)
print(correlation_test) #-1
#Two variables are completely opposite
#only one needs to be kept to avoid multicollinearity

#drop meaningless variables
df_bat <- df_bat[, -which(names(df_bat) %in% c("choose_to_bat_numeric", "forced_to_bat_numeric", "forced_to_bat"))]

binary_vars <- c("last_match_result", "season", "gender", "home_advantage", "choose_to_bat")
plots <- list()
for(var in binary_vars){
  p <- ggplot(df_bat, aes_string(var)) + 
    geom_bar(fill=color_palette(2)) +
    labs(title = paste("Distribution of", var), x = var, y = "Count") +
    theme_minimal()
  plots[[var]] <- p
}
do.call("grid.arrange", c(plots, ncol = 2))

###imbalance variables with respect to target
doubledecker(Result ~ home_advantage, data=df_bat, gp =gpar(fill=color_palette(2)))
doubledecker(Result ~ choose_to_bat, data=df_bat, gp =gpar(fill=color_palette(2)))

###last_match_result& home_advantage:p<0.05
doubledecker(last_match_result ~ home_advantage, data=df_bat,gp =gpar(fill=color_palette(2)))
doubledecker(Result ~ home_advantage+last_match_result, data=df_bat, gp =gpar(fill=color_palette(2)))
#last result no + no home advantage
#more likely to lose in this match


###Group2: Multi-categorical Variables
#Team, Opponent, Winner, Toss winner
team_colors <- brewer.pal(8, "Set2")
plot_team<-ggplot(data=df_bat, aes(x=team, fill=team)) + 
  geom_bar() + 
  scale_fill_manual(values=team_colors) +
  labs(title="Barplot of Team") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) +
  coord_cartesian(ylim=c(0, 25))
plot_opponent<-ggplot(data=df_bat, aes(x=opponent, fill=opponent)) + 
  geom_bar() + 
  scale_fill_manual(values=team_colors) +
  labs(title="Barplot of Opponent") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) +
  coord_cartesian(ylim=c(0, 25))
plot_winner<-ggplot(data=df_bat, aes(x=winner, fill=winner)) + 
  geom_bar() + 
  scale_fill_manual(values=team_colors) +
  labs(title="Barplot of Winner") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) +
  coord_cartesian(ylim=c(0, 25))
plot_toss_winner<-ggplot(data=df_bat, aes(x=toss_winner, fill=toss_winner)) + 
  geom_bar() + 
  scale_fill_manual(values=team_colors) +
  labs(title="Barplot of Toss Winner") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) +
  coord_cartesian(ylim=c(0, 25))

grobs <- list(ggplotGrob(plot_team), ggplotGrob(plot_opponent),
              ggplotGrob(plot_winner),ggplotGrob(plot_toss_winner))
grid.arrange(grobs = grobs, ncol = 2)

multi_vars <- c("winner", "toss_winner", "team", "opponent")
for(var in multi_vars){
  p <- ggplot(df_bat, aes_string(var)) + 
    geom_bar(fill=team_colors) +
    labs(title = paste("Distribution of", var), x = var, y = "Count") +
    theme_minimal() +
    coord_flip()  
  print(p)
}

###'team''opponent''winner''toss_winner' respect to target
doubledecker(Result ~ team, data=df_bat, gp =gpar(fill=color_palette(2)))
doubledecker(Result ~ opponent, data=df_bat, gp =gpar(fill=color_palette(2)))
doubledecker(Result ~ toss_winner, data=df_bat, gp =gpar(fill=color_palette(2)))
doubledecker(Result ~ winner, data=df_bat, gp =gpar(fill=color_palette(2)))


##### Is Toss Winner also the Winner? #####
df_bat$toss_equals_winner <- ifelse(df_bat$toss_winner == df_bat$winner, "Yes", "No")
toss_winner_table <- table(df_bat$toss_equals_winner)

pie(toss_winner_table, col = c("lightblue", "darkblue"), 
    main = "Is Toss Winner also the Match Winner?",
    labels = paste(names(toss_winner_table), "\n", toss_winner_table, " matches"))

df_bat$toss_equals_winner <- NULL


##### Is Toss Winner also the Opponent? #####
df_bat$toss_equals_opponent <- ifelse(df_bat$toss_winner == df_bat$opponent, "Yes", "No")
toss_opponent_table <- table(df_bat$toss_equals_opponent)

pie(toss_opponent_table, col = c("lightblue", "darkblue"), 
    main = "Is Toss Winner also the Opponent?",
    labels = paste(names(toss_opponent_table), "\n", toss_opponent_table, " matches"))

df_bat$toss_equals_opponent <- NULL
#opponent& toss winner: highly correlated


##### How to win Finals??  #####
participation_count <- df_bat %>%
  select(team, opponent) %>%
  unlist() %>%
  table() %>%
  as.data.frame() %>%
  rename_with(.cols = everything(), .fn = ~ c("team_name", "participation_count"))
win_count <- df_bat %>%
  count(winner) %>%
  rename(team_name = winner, win_count = n)
team_stats <- left_join(participation_count, win_count, by = "team_name") %>%
  replace_na(list(win_count = 0)) %>%
  mutate(loss_count = participation_count - win_count) %>%
  pivot_longer(cols = c(win_count, loss_count), names_to = "result", values_to = "count")

ggplot(team_stats, aes(x = team_name, y = count, fill = result)) +
  geom_bar(stat = "identity") +
  labs(title = "Participation and Wins by Team",
       x = "Team",
       y = "Count",
       fill = "Result") +
  scale_fill_manual(values = c("lightblue", "darkblue")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


#City, Venue
city_colors <- brewer.pal(7, "Set2")
names(city_colors) <- unique(df_bat$city) 

df_bat$venue_city <- paste(df_bat$venue, df_bat$city)
df_bat$venue_city = as.factor(df_bat$venue_city)
table(df_bat$venue_city)
plot_venue <- ggplot(data = df_bat, aes(x = venue_city, fill = city)) + 
  geom_bar() + 
  scale_fill_manual(values = city_colors) +
  labs(title = "Barplot of Venue/City") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) +
  coord_cartesian(ylim = c(0, 20))

table(df_bat$city)
plot_city <- ggplot(data = df_bat, aes(x = city, fill = city)) + 
  geom_bar() + 
  scale_fill_manual(values = city_colors) +
  labs(title = "Barplot of City") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) +
  coord_cartesian(ylim = c(0, 35))

grobs <- list(ggplotGrob(plot_venue), ggplotGrob(plot_city))
grid.arrange(grobs = grobs, ncol = 2)

###compare with city and venue
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

##### strategy: toss winner& choose to bat #####
df_bat$team_won_toss_and_batting <- ifelse((df_bat$toss_winner == df_bat$team & df_bat$choose_to_bat == 'yes') | 
                                             (df_bat$toss_winner == df_bat$opponent & df_bat$choose_to_bat == 'no'), 
                                           'yes', 'no')
df_bat$team_won_toss_and_batting
#The toss winner always chooses to bat. 
#If the toss winner is the TEAM, then they choose to bat first, 'choose_to_bat'=yes
#and if the toss winner is the OPPONENT, they choose to bat later. 'choose_to_bat'=no
#This is a common strategy because the batting team has a greater advantage.


###Because a new variable has been added that includes city and venue information, 
#keep venue_city and drop 'city' and 'venue'
df_bat <- df_bat[, -(which(names(df_bat) %in% c("venue", 
                                            "city", 
                                            "team_won_toss_and_batting",
                                            'toss_and_match_winner')))]


### Numerical Variables ###
###Group3: numeric varaibles
# Numerical Variable subset
numeric_columns = unlist(lapply(df_bat, is.numeric))
numeric_variables = df_bat[ ,numeric_columns]
names(numeric_variables)
#"match_number"    "season"          "team_score"      "team_wickets"    "win_by_runs"    
#"win_by_wickets"  "avg_win_rate"    "avg_score"       "avg_wickets_out" "days_from_start"

# corrplot
cor_matrix = cor(numeric_variables, method = "pearson")
corrplot(cor_matrix, method="circle", type = "upper")
#corrplot(cor_matrix, method="circle", type = "upper", addCoef.col = "black")
##### days from start & match number: 0.95 #####

color_mapping = ifelse(df_bat$Result == "win", "darkblue", "lightblue")
pairs(numeric_variables, main = "Scatter Plot Matrix for Numeric Variables",
      method = "pearson", 
      hist.col = "cornflowerblue",
      density = T, col = color_mapping,
      ellipses = F,
      pch = 16)

### days from start & match number: 0.95  ###
xyplot(days_from_start ~ match_number | Result, data = df_bat,pch=16,
       xlab = "match number",
       ylab = "days from start in every season",
       main = "Relationship between days from start and match number by Result")

### Frequency of Halftime Scores ###
ggplot(df_bat, aes(x = team_score)) +
  geom_histogram(aes(y = ..density..), binwidth = 10, fill = "cornflowerblue") +
  geom_density(alpha = 0.5, fill = "blue") +
  labs(x = "Team Score",
       y = "Frequency",
       title = "Frequency of Team Scores") +
  theme_minimal()

### Frequency of Halftime Wickets ###
ggplot(df_bat, aes(x = team_wickets)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "cornflowerblue") +
  geom_density(color = "red") +
  labs(title = "Distribution of Team Wickets",
       x = "Team Wickets",
       y = "Density") +
  xlim(c(2, 10)) +
  theme_minimal()

##### Look-ahead Bias ####
#The information contained in the data is a direct result of winning a cricket match.

#But here the halftime model is trained based on past historical information 
#(which has already happened), 
#so there is no need to consider look-ahead bias

##### Encoding #####
levels(df_bat$Result) <- c(0, 1)
typeof(df_bat$Result)
table(df_bat$Result) 

levels(df_bat$last_match_result) <- c(0, 1)
levels(df_bat$gender) <- c(0, 1)
levels(df_bat$home_advantage) <- c(0, 1)
levels(df_bat$choose_to_bat) <- c(0, 1)

### label encoding ###
levels(df_bat$team)
levels(df_bat$venue_city)

teams <- sort(unique(c(df_bat$team, df_bat$opponent, df_bat$winner, df_bat$toss_winner)))

df_bat$team_code <- match(df_bat$team, teams)
df_bat$opponent_code <- match(df_bat$opponent, teams)
df_bat$winner_code <- match(df_bat$winner, teams)
df_bat$toss_winner_code <- match(df_bat$toss_winner, teams)

team_to_city <- c(
  "Birmingham Phoenix" = "Edgbaston, Birmingham Birmingham",
  "London Spirit" = "Lord's, London London", 
  "Manchester Originals" = "Old Trafford, Manchester Manchester", 
  "Northern Superchargers" = "Headingley, Leeds Leeds", 
  "Oval Invincibles" = "Kennington Oval, London London",
  "Southern Brave" = "The Rose Bowl, Southampton Southampton", 
  "Trent Rockets" = "Trent Bridge, Nottingham Nottingham", 
  "Welsh Fire" = "Sophia Gardens, Cardiff Cardiff"
)

df_bat$venue_city_code <- ifelse(df_bat$venue_city == team_to_city[df_bat$team],
                                 df_bat$team_code, df_bat$opponent_code)


###Drop columns
df_bat <- subset(df_bat, select = -c(team, opponent, winner, toss_winner, venue_city))

new_order <- c('Result',"match_number", 'season','gender',
               'team_code','opponent_code', 
               'winner_code','toss_winner_code',
               "team_score","team_wickets","win_by_runs","win_by_wickets",
               'days_from_start','choose_to_bat',
               'home_advantage','venue_city_code',
               "avg_win_rate", "avg_score","avg_wickets_out", "last_match_result")

df_bat <- df_bat[, new_order]

str(df_bat)
names(df_bat)

write.csv(df_bat,"halftime_df_bat.csv", row.names = F)
### End EDA & halftime features ###

#Descriptive statistics 
summary(df_bat)

df_bat$Result <- as.numeric(df_bat$Result) -1
df_bat$gender <- as.numeric(df_bat$gender) -1
df_bat$home_advantage <- as.numeric(df_bat$home_advantage) -1
df_bat$choose_to_bat <- as.numeric(df_bat$choose_to_bat) -1
df_bat$last_match_result <- as.numeric(df_bat$last_match_result) -1


#Break into win and lose classes to examine variance of both classes
Windf_bat <- df_bat[df_bat$Result==1,]
Losedf_bat <- df_bat[df_bat$Result==0,]

#Covariance
(cov(Windf_bat))
(cov(Losedf_bat))

#Export
write.csv(cov(Windf_bat),"cov_Win.csv", row.names = F) 
write.csv(cov(Losedf_bat),"cov_Lose.csv", row.names = F)

cor_matrix <- cor(df_bat)
print(cor_matrix)

##Correlation and covariance matrices of df_bat
(corbat<-cor(df_bat))
(covbat<-cov(df_bat))

write.csv(corbat,"df_bat_Correlation.csv", row.names = F) 
write.csv(covbat,"df_bat_Covariance.csv", row.names = F)

##### PCA & Clustering #####

#'match number' and 'days from start' share redundant information
#using PCA removes this redundancy, thus reducing the dimensionality of the data

d2 <- df_bat[, names(numeric_variables)]

#Standardised the dataset
apply(d2, 2, sd)
apply(d2, 2, mean) #Needs to be standardised
#They are vastly different, so there is need for scaling.

pr.out<-prcomp(d2,scale=TRUE)
summary(pr.out)

pr.out$center #Means and standard deviations of variables before standardisation.
pr.out$scale #Means and standard deviations of variables after standardisation.

#eigenvalues
pr.out$sdev^2
#PC loadings  vector
pr.out$rotation
#PC score
pr.out$x

#The Standard deviation of the principal components is the square root of the corresponding eigenvalues.
sd_all <- summary(pr.out)$importance[1,] #Standard deviation
sd_all^2/sum(sd_all^2) #pr.out$sdev

#scree plot
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
#The summary shows that 5 components are enough to keep 81.6% of the variability in data. 
#10 components keep 82.17% of the variability in data.
#Also choosing 5 principal components seems reasonable based on the so-called elbow method.

#biplot
fviz_pca_biplot(pr.out,addlabels = TRUE)

scores <- pr.out$x[, 1:5]
loadings <- pr.out$rotation[, 1:5]

#Correlation between variables and PCs
var <- get_pca_var(pr.out)
var$cor  #Correlation between variables and PC
cor(d2,pr.out$x[,1:5]) #Variable Correlation Chart
fviz_pca_var(pr.out) 
fviz_pca_var(pr.out, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

#Cos2 between variables and PCs
var$cos2 #quality of representation (var$cor)^2
cos2 <- (cor(d2, pr.out$x[,1:5]))^2
cos2
fviz_cos2(pr.out,choice='var',axes=1) 
fviz_cos2(pr.out,choice='var',axes=2)
fviz_cos2(pr.out,choice='var',axes=3)
fviz_cos2(pr.out,choice='var',axes=4)
fviz_cos2(pr.out,choice='var',axes=5)


##Contribution between variables and PCs
var$contrib 
#any variable whose height is up to 16.66 and 
#above is considered to have contributed significantly to the component.

fviz_contrib(pr.out, choice = "var", axes = 1, top = 10) #Variable contribution to PC1
#team_score, win_by_wickets, win_by_runs, team_wickets
fviz_contrib(pr.out, choice = "var", axes = 2, top = 10) #Variable contribution to PC2
#match_number, days_from_start
fviz_contrib(pr.out, choice = "var", axes = 3, top = 10) #Variable contribution to PC3
#avg_win_rate, avg_wickets_out
fviz_contrib(pr.out, choice = "var", axes = 4, top = 10) #Variable contribution to PC4
#avg_score, team_wickets
fviz_contrib(pr.out, choice = "var", axes = 5, top = 10) #Variable contribution to PC5
#season

### Using the first two principal components to visualise the effect of dimensionality reduction and true labelling #####
pca_scores <- as.data.frame(pr.out$x[, 1:2])
pca_scores$Result <- df_bat$Result
ggplot(pca_scores, aes(x = PC1, y = PC2, color = as.factor(Result))) +
  geom_point() +
  labs(x = "Principal Component 1", y = "Principal Component 2", color = "Result") +
  theme_minimal() +
  ggtitle("PCA Plot with True Labels")

pca_scores <- as.data.frame(pr.out$x)
pca_scores$Result <- as.factor(df_bat$Result)
fviz_pca_biplot(pr.out, 
                label = "var", 
                habillage = pca_scores$Result, 
                palette = c("#00AFBB", "#FC4E07"), 
                addEllipses = TRUE) 


##### Cluster analysis with pca data #####
pca_scores <- as.data.frame(pr.out$x[, 1:5])

# Combine PCA scores with label-encoded categorical variables
combined_data <- cbind(pca_scores, df_bat[, c("gender", "team_code", "opponent_code","winner_code", 
                                              "toss_winner_code", "choose_to_bat", "home_advantage", 
                                              "venue_city_code", "last_match_result")])
# Standardize the combined data
df.scaled <- scale(combined_data)


#1.k-means 
#Choose the best k in the k-means algorithm, using the sum of the sums of squares within the clusters
fviz_nbclust(df.scaled, kmeans, method = "wss")+
  geom_vline( xintercept  =  6 , linetype  =  2 )
#total within-cluster sum of square (wss): 2
fviz_nbclust(df.scaled, kmeans, method = "silhouette")+
  labs(title = "K-means")

# Compute k-means with k = 2
set.seed(123)
k2 <- kmeans(df.scaled, 2, nstart = 25)
k2
names(k2)
#Try 25 different random initial cluster assignments and choose the best result corresponding to the one with the least variation within the clusters
#between_SS / total_SS = 14.03%，Percentage of variance explained by the mean of the clusters
k2$centers #2 centroids 
k2$iter #1 iteration only
k2$size #63 52
k2$withinss #736.5483 635.4522
k2$tot.withinss #1372
k2$totss #1596
k2$betweenss #223.9996
#Percentage of variance explained by the cluster means = (between_ss/total_ss)*100
# k2$betweenss=k2$totss -k2$tot.withinss
# 14.03 %= (k2$totss-k2$tot.withinss)/k2$totss= k2$betweenss/k2$totss

cols=c('red','darkgreen')
plot(df.scaled,col=cols[k2$cluster],main='K-means clustering with 2 clusters',xlab='',ylab='')
points(k2$centers,pch=19,cex=2,col=cols) #k=2 is not good enough, there is overlap

#Find the mean of 6 variables based on the newly created cluster = k2$centers #2 centroids under 6 variables (6 dimensions)
aggregate(df.scaled, by=list(cluster=k2$cluster), mean)

# Visualise kmeans clustering
fviz_cluster(k2, df.scaled, ellipse.type = "norm")

# Evaluate the clustering result:k-means
table(k2$cluster, df_bat$Result) #54.78%


#2.Hierarchical clustering
## Find the best number of clusters using the elbow method
fviz_nbclust(df.scaled, FUNcluster = function(x, k) { list(cluster = cutree(hclust(dist(x)), k = k)) }, 
             method = "silhouette") +labs(title = "Hierarchical")
#fviz_nbclust(df.scaled, FUNcluster = hcut, method = "silhouette")+ labs(title = "Hierarchical")
#k_optimal=2

# Compute distances and hierarchical clustering
dd <- dist(df.scaled, method = "euclidean")
hc <- hclust(dd, method = "complete")
hc
hcut<-cutree(hc,k=2)
hcut
table(hcut) #cluster1:59,cluster2:56
rownames(df.scaled)[hcut == 1]

#We can visualise the object by using a dendrogram. 
#This will enable us to determine the point to cut the tree, 
#resulting in the number of clusters.
dendros <- as.dendrogram(hc)
plot(dendros, main = "Combination data - Complete linkage",
     ylab = "Height")
abline(h=0.5, lty = 2, col="red")
abline(h=1, lty = 2, col="blue")
Hs <- hc$height[(length(hc$height)-4):length(hc$height)]
abline(h=Hs, col=3, lty=2)

fviz_cluster(list(data=df.scaled, cluster=cutree(hc, 2)), ellipse.type = "norm")
fviz_dend(hc, k = 2, # Cut in two groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB"),
          color_labels_by_k = TRUE, # color labels by groups
          ggtheme = theme_gray() # Change theme
)

hcut <- cutree(hc, k = 2)
table(hcut) #Check the number of observations in each of the two clusters
aggregate(d2, by=list(cluster=hcut), mean) 
#Can be used to calculate the mean of each variable by clusters using raw data

hc_silhouette_score <- mean(silhouette(hcut, dist(df.scaled))[, "sil_width"])
hc_silhouette_score #0.1041427

m <- c("average","single","complete")
names(m) <- c("average","single","complete")
# function to compute coefficient
ac <- function(x){
  agnes(df.scaled, method = x)$ac
}
map_dbl(m,ac) #complete: 0.6515457 

library(igraph)
fviz_dend(hc, k = 2, k_colors = "jco", type = "phylogenic", repel = TRUE)

# Evaluate the clustering result
table(hcut, df_bat$Result) #53.04%


#The two types of clustering done after normalising the continuous variables of the dataset 
#by dimensionality reduction and merging them with the categorical variables 
#did not work well and did not yield much information.





##### Modelling on 100% of the dataset #####
#rather than splitting the dataset into training and test sets
df_bat$Result <- as.factor(df_bat$Result)
str(df_bat)
### match number & days from start:0.95
df_bat <- df_bat[, -which(names(df_bat) %in% c('match_number', 'win_by_runs','win_by_wickets'))]


#error metrics -- Confusion Matrix
err_metric=function(CM)
{ TN =CM[1,1]
  TP =CM[2,2]
  FP =CM[1,2]
  FN =CM[2,1]
  precision =(TP)/(TP+FP)
  recall_score =(FP)/(FP+TN)
  f1_score=2*((precision*recall_score)/(precision+recall_score))
  accuracy_model  =(TP+TN)/(TP+TN+FP+FN)
  False_positive_rate =(FP)/(FP+TN)
  False_negative_rate =(FN)/(FN+TP)
  print(paste("Precision value of the model: ",precision))
  print(paste("Accuracy of the model: ",accuracy_model))
  print(paste("Recall value of the model: ",recall_score))
  print(paste("False Positive rate of the model: ",False_positive_rate))
  print(paste("False Negative rate of the model: ",False_negative_rate))
  print(paste("f1 score of the model: ",f1_score))}


### Reference material: Introduction to Statistics for Data Science ###
n=nrow(df_bat) #115
p=ncol(df_bat)-1 #16

### model1----full logistic model (binomial regression)
binomial_model <- glm(Result ~ ., data = df_bat, family = "binomial")
summary(binomial_model)
#team_score gender avg_wickets_out
prob_win_full_binomial <- predict(binomial_model, type = "response")
prob_win_full_binomial

phat = predict(binomial_model, df_bat, type="response")
yhat = as.numeric(ifelse(phat > 0.5, 1, 0)) 
1-mean(yhat==df_bat$Result) 
#Train error(binomial regression):0.2347826 < 50%

CM= table(Observed=df_bat$Result, Predicted=yhat)
print(CM)
err_metric(CM)
#Train Accuracy of the model:   0.765217391304348

#If we simply flipped a coin to decide whether to predict a win or a loss, the training error would be 50%. 
#It appears that we did a little better than that. 
#However, the training error uses the same data to train and test the model and therefore gives an optimistic assessment of the performance of the classifier.

### model2----forward binomial regression stepwise selection with AIC criterion
null_logit_model = glm(Result~1, data=df_bat, family = "binomial")
forward_step_model <- step(null_logit_model, scope=formula(binomial_model), direction="forward") 
#Result ~ team_score, gender, avg_wickets_out

df_subset <- df_bat[,c('Result','team_score','gender','avg_wickets_out')]
forward_step_logit_model <- glm(formula = Result ~ team_score + gender + avg_wickets_out,
                                family = "binomial", data = df_subset)

summary(forward_step_logit_model)
prob_win_forward_binomial <- predict(forward_step_logit_model, type = "response")
prob_win_forward_binomial

phat = predict(forward_step_logit_model, df_subset, type="response")
yhat = as.numeric(ifelse(phat > 0.5, 1, 0)) 
1-mean(yhat==df_bat$Result) 
#Train error(forward binomial stepwise): 0.2608696

CM= table(Observed=df_bat$Result, Predicted=yhat)
print(CM)
err_metric(CM)
#Train Accuracy of the model(forward stepwise):  0.739130434782609


### 10-fold cross-validation ###
#k-fold: Set the seed (say, at 5) to make the analysis reproducible 
set.seed(5)
## Sample the fold-assignment index
nfolds = 10
fold_index = sample(nfolds, n, replace=TRUE)
#A random sample of 115 numbers from 0-10 with release 
head(fold_index)

logistic_reg_fold_error = function(X, y, test_data) {
  Xy = data.frame(X, y=y)
  if(ncol(Xy)>1) tmp_fit = glm(y ~ ., data=Xy[!test_data,], family="binomial") 
  else tmp_fit = glm(y ~ 1, data=Xy[!test_data,,drop=FALSE], family="binomial") 
  phat = predict(tmp_fit, Xy[test_data,,drop=FALSE], type="response")
  yhat = ifelse(phat > 0.5, 1, 0)
  yobs = y[test_data]
  test_error = 1 - mean(yobs == yhat)
  return(test_error)
}

general_cv = function(X, y, fold_ind, fold_error_function) {
  p = ncol(X)
  Xy = cbind(X, y=y)
  nfolds = max(fold_ind)
  if(!all.equal(sort(unique(fold_ind)), 1:nfolds)) stop("Invalid fold partition.") 
  fold_errors = numeric(nfolds)
  for(fold in 1:nfolds) {
    fold_errors[fold] = fold_error_function(X, y, fold_ind==fold) 
  }
  fold_sizes = numeric(nfolds)
  for(fold in 1:nfolds) fold_sizes[fold] = length(which(fold_ind==fold)) 
  test_error = weighted.mean(fold_errors, w=fold_sizes) 
  return(test_error)
}

test_error_full_logit = general_cv(df_bat[,2:p+1], df_bat[,1], fold_index, logistic_reg_fold_error)
#test error(full logit):0.3565217

test_error_forward = general_cv(df_subset[,2:4], df_subset[,1], fold_index, logistic_reg_fold_error)
#test error(forward stepwise):0.2956522


### roc analysis for full-logit & forward stepwise
roc_full <- roc(df_bat$Result, prob_win_full_binomial)
roc_forward <- roc(df_bat$Result, prob_win_forward_binomial)

plot(roc_full, main="ROC Curve")
lines(roc_forward, col="red")

auc_full <- auc(roc_full) #0.8443
auc_forward <- auc(roc_forward) #0.7985

print(paste("AUC for  binomial regression full model:", auc_full))
print(paste("AUC for  binomial regression stepwise model:", auc_forward))


### model3----LDA with stepwise model
library(MASS)

lda_forward_model = lda(Result ~ team_score + gender + avg_wickets_out, data=df_subset)
lda_forward_prdict = predict(lda_forward_model, df_subset)
lda_forward_win_pro = lda_forward_prdict$class
lda_forward_win_pro

1 - mean(df_bat$Result == lda_forward_win_pro) 
#Training error (LDA with 3 variables): 0.2608696

lda_fold_error = function(X, y, test_data) {
  Xy = data.frame(X, y=y)
  if(ncol(Xy)>1) tmp_fit = lda(y ~ ., data=Xy[!test_data,]) 
  else tmp_fit = lda(y ~ 1, data=Xy[!test_data,,drop=FALSE]) 
  yhat = predict(tmp_fit, Xy[test_data,,drop=FALSE])$class
  yobs = y[test_data]
  test_error = 1 - mean(yobs == yhat)
  return(test_error)
}

lda_test_error_forward = general_cv(df_subset[,2:4], df_subset[,1], fold_index, lda_fold_error)
#LDA test error with forward subset: 0.3130435 (10-fold cross-validation)

### model4----QDA model with 3 variables
qda_fit = qda(Result ~ team_score + gender + avg_wickets_out, data=df_subset) 
summary(qda_fit)

qda_predict = predict(qda_fit, df_subset)
yhat_qda = qda_predict$class

confusion_qda = table(Observed=df_bat$Result, Predicted=yhat_qda) 
confusion_qda

1-mean(yhat_qda==df_bat$Result) 
#QDA training error with forward subset: 0.2347826

#10-fold test error for QDA model
qda_fold_error = function(X, y, test_data) {
  Xy = data.frame(X, y=y)
  if(ncol(Xy)>1) tmp_fit = qda(y ~ ., data=Xy[!test_data,]) 
  tmp_predict = predict(tmp_fit, Xy[test_data,])
  yhat = tmp_predict$class
  yobs = y[test_data]
  test_error = 1 - mean(yobs == yhat)
  return(test_error)
}

test_error_qda = general_cv(df_subset[,c('team_score','gender','avg_wickets_out')],
                            df_subset[,'Result'], fold_index, qda_fold_error)
#QDA test error with forward subset: 0.2782609




##### model 5----Naive Bayesian #####
library(e1071)

nb_model <- naiveBayes(Result ~ ., data = df_bat)
print(nb_model)

prob_predict_nb <- predict(nb_model, df_bat, type="raw")
print(prob_predict_nb)

predicted_nb <- factor(ifelse(prob_predict_nb[,2] > 0.5, 1, 0), levels = c(0,1))
confusion_nb = table(Observed=df_bat$Result, Predicted=predicted_nb) 
confusion_nb

1-mean(df_bat$Result==predicted_nb )
#naive Bayesian train error:0.2956522
CM= table(Observed=df_bat$Result, Predicted=predicted_nb)
print(CM)
err_metric(CM)
#Train Accuracy of the model:  0.704347826086957

naive_bayes_fold_error = function(X, y, test_data) {
  Xy = data.frame(X, y=y)
  if(ncol(Xy) > 1) tmp_fit = naiveBayes(y ~ ., data=Xy[!test_data,]) 
  else tmp_fit = naiveBayes(y ~ 1, data=Xy[!test_data,,drop=FALSE]) 
  phat = predict(tmp_fit, Xy[test_data,,drop=FALSE], type="raw")
  yhat = ifelse(phat[,2] > 0.5, 1, 0)
  yobs = y[test_data]
  test_error = 1 - mean(yobs == yhat)
  return(test_error)
}

test_error_naive_bayes = general_cv(df_bat[, -1], df_bat[, 1], fold_index, naive_bayes_fold_error)
#Test error for naive bayes: 0.4086957



##### model 6----Classification Tree #####
##### CART #####
### Standard Cart
library(tree)
unpruned_tree_model = tree(Result ~., data = df_bat, method = "class")
plot(unpruned_tree_model)
text(unpruned_tree_model, pretty = 0)
title("Unpruned Classification Tree", line = +3)
summary(unpruned_tree_model) 
#"team_score"       "avg_wickets_out"  "winner_code"      "team_wickets"     "avg_win_rate"    
# "avg_score"        "days_from_start"  "toss_winner_code"

unpruned_prob <- predict(unpruned_tree_model, df_bat, type = "vector")
unpruned_prob

# 10-fold CV Test Error for Unpruned
cv_errors_unpruned <- vector("numeric", 10)
folds <- createFolds(df_bat$Result, k = 10)
for(i in 1:10) {
  train_data <- df_bat[-folds[[i]], ]
  test_data <- df_bat[folds[[i]], ]
  model <- tree(Result ~., data = train_data, method = "class")
  predictions <- predict(model, test_data, type = "class")
  cv_errors_unpruned[i] <- mean(predictions != test_data$Result)
}
mean_test_error_unpruned <- mean(cv_errors_unpruned)
#unpruned test error: 0.416317
train_predictions_unpruned <- predict(unpruned_tree_model, df_bat, type = "class")
train_error_unpruned <- mean(train_predictions_unpruned != df_bat$Result)
#unpruned train error: 0.1304348



### Pruned Tree
pruned_tree_cv <- cv.tree(unpruned_tree_model, FUN = prune.tree, K = 10)
plot(pruned_tree_cv)
title("Deviance as Size of Pruned Tree Increases", line = +3)

pruned_tree_model = prune.tree(unpruned_tree_model, best = 5)
summary(pruned_tree_model)
#"team_score"  "avg_wickets_out"

plot(pruned_tree_model)
text(pruned_tree_model, pretty = 0)
title("Pruned Classification Tree", line = +3)

predict_prob <- predict(pruned_tree_model, df_bat, type = "vector")
predict_prob

# 10-fold CV Test Error for Pruned
cv_errors_pruned <- vector("numeric", 10)
for(i in 1:10) {
  train_data <- df_bat[-folds[[i]], ]
  test_data <- df_bat[folds[[i]], ]
  model <- prune.tree(tree(Result ~., data = train_data, method = "class"), best = 5)
  predictions <- predict(model, test_data, type = "class")
  cv_errors_pruned[i] <- mean(predictions != test_data$Result)
}
mean_test_error_pruned <- mean(cv_errors_pruned)
#pruned tree test error: 0.4420746

train_predictions_pruned <- predict(pruned_tree_model, df_bat, type = "class")
train_error_pruned <- mean(train_predictions_pruned != df_bat$Result)
#pruned tree train error: 0.2

library(randomForest)
library(caret)

##### Bagged Model #####
tree_number = seq(from = 5, to = 1000, by = 5)
classification_accuracy_vec = vector("numeric", length(tree_number))
train_errors = vector("numeric", length(tree_number))

for (i in 1:length(tree_number)){
  bagged_model = randomForest(Result ~ ., df_bat, ntree = tree_number[i], importance = TRUE)
  predicted_labels = predict(bagged_model, df_bat, type = "response")
  predicted_probs = predict(bagged_model, df_bat, type = "prob") 
  # Probability prediction
  
  accuracy = mean(predicted_labels == df_bat$Result)
  train_errors[i] = mean(predicted_labels != df_bat$Result)
  classification_accuracy_vec[i] = accuracy
}

optimal_trees = tree_number[which.max(classification_accuracy_vec)] #15

# Perform 10-fold cross-validation for test error
folds <- createFolds(df_bat$Result, k = 10)
cv_errors <- vector("numeric", 10)
for(i in 1:10) {
  train_data <- df_bat[-folds[[i]],]
  test_data <- df_bat[folds[[i]],]
  model <- randomForest(Result ~ ., train_data, ntree = optimal_trees)
  predictions <- predict(model, test_data, type = "response")
  cv_errors[i] <- mean(predictions != test_data$Result)
}

mean_test_error <- mean(cv_errors)
#bagging model test error: 0.3020979

colours = c(rep("black", length(tree_number)))
colours[which.max(classification_accuracy_vec)] = "green"
plot(tree_number, classification_accuracy_vec, main = "Classification Accuracy as Number of Trees in Bagging Classification Tree Increases", 
     xlab = "Number of Trees", ylab = "Classification Accuracy", pch = 16,
     col = colours) 

# Print results
print(paste("Optimal number of trees:", optimal_trees))
print(paste("Training error:", mean(train_errors)))
print(paste("Mean test error:", mean_test_error))
print("Predicted probabilities for training data:")
print(predicted_probs) # Print the probabilities
print(importance(bagged_model))
#There are a total of 16 variables and 1 target variable, and I set 100/16 as a threshold

#team_code, opponent_code, winner_code, team_score, team_wickets, avg_win_rate, avg_wickets_out



##### random forest #####
classification_accuracy_vec = vector("double", 16)
optimal_trees = 15

# Perform 10-fold cross-validation for test error
folds <- createFolds(df_bat$Result, k = 10)
cv_errors <- vector("numeric", 10)
for (i in 1:16){
  # 10-fold CV for each mtry
  for(j in 1:10) {
    train_data <- df_bat[-folds[[j]],]
    test_data <- df_bat[folds[[j]],]
    rf_model = randomForest(Result ~ ., train_data, mtry = i, ntree = optimal_trees)
    predictions <- predict(rf_model, test_data, type = "response")
    cv_errors[j] <- mean(predictions != test_data$Result)
  }
  
  classification_accuracy_vec[i] = 1 - mean(cv_errors) # Compute accuracy for each mtry
}

colours = c(rep("black", 16))
colours[which.max(classification_accuracy_vec)] = "green"

plot(classification_accuracy_vec,main = "Classification Accuracy as Number of Variables Randomly Sampled at Each Node Increases", 
     xlab = "Number of Variables Available at Each Node", ylab = "Classification Accuracy", pch = 16,
     col = colours)

# Building model with optimal mtry and ntree on the full training data
rf_model = randomForest(Result ~ ., df_bat, mtry = optimal_mtry, ntree = optimal_trees,
                        importance = TRUE)

# Getting training error
train_predictions <- predict(rf_model, df_bat, type = "response")
train_error <- mean(train_predictions != df_bat$Result)

# Perform 10-fold cross-validation for test error
folds <- createFolds(df_bat$Result, k = 10)
cv_errors <- vector("numeric", 10)

for(i in 1:10) {
  train_data <- df_bat[-folds[[i]],]
  test_data <- df_bat[folds[[i]],]
  rf_model <- randomForest(Result ~ ., train_data, mtry = optimal_mtry, ntree = optimal_trees)
  predictions <- predict(rf_model, test_data, type = "response")
  cv_errors[i] <- mean(predictions != test_data$Result)
}

# Mean test error from 10-fold CV
mean_test_error <- mean(cv_errors)
#Training error: 0.0173913043478261"
#Mean test error from 10-fold CV: 0.267773892773893"


predicted_probabilities <- predict(rf_model, df_bat, type = "prob")
print("Predicted probabilities:")
print(predicted_probabilities)

# Getting variable importance
importance_values <- importance(rf_model)
print("Variable Importance:")
print(importance_values)

# Plotting the variable importance
varImpPlot(rf_model, main = "Variable Importance in Random Forest")
#100/16=6.25
#team_score, avg_wickets_out 


##### boosting #####
### Boosted Tree
# Find best interaction Depth - best is 4, seed - 1
accuracy_depth = double(10)
for (i in 1:10){
  boost_model =gbm(Result~.,data=df_bat, distribution="multinomial",
                   n.trees =15, interaction.depth =i)
  predicted_probabilites = predict(boost_model, df_bat, type = "response")
  predicted_labels = ifelse((predicted_probabilites[,1,] > predicted_probabilites[,2,])  > 0.5 , 0, 1)
  cm_rf = table(predicted_labels, df_bat$Result)
  accuracy = (cm_rf[1,1] + cm_rf[2,2]) / nrow(df_bat)
  accuracy_depth[i] = accuracy
}

#which.max(accuracy_depth):4
colours = c(rep("black", 10))
colours[which.max(accuracy_depth)] = "green"

plot(accuracy_depth, main = "Accuracy as Depth of Trees Increases", 
     xlab = "Depth", ylab = "Accuracy", pch = 16,
     col = colours)

# Optimise Shrinkage Parameter
shrink_index = seq(from = 0.0025, to = 0.25, by = 0.0025)
accuracy_shrinkage = c()
for (i in 1:100){
  boost_model =gbm(Result~.,data=df_bat, distribution="multinomial",
                   n.trees =15, interaction.depth = 4, shrinkage = (i/400))
  predicted_probabilites = predict(boost_model, df_bat, type = "response")
  predicted_labels = ifelse((predicted_probabilites[,1,] > predicted_probabilites[,2,])  > 0.5 , 0, 1)
  cm_rf = table(predicted_labels, df_bat$Result)
  accuracy = (cm_rf[1,1] + cm_rf[2,2]) / nrow(df_bat)
  accuracy_shrinkage[i] = accuracy
}
#which.max(accuracy_shrinkage) 79
colours = c(rep("black", 500))
colours[which.max(accuracy_shrinkage)] = "green"

plot(shrink_index, accuracy_shrinkage, main = "Accuracy as Shrinkage Parameter Increases",
     xlab = "Lambda (Shrinkage Parameter)",ylab = "Accuracy", pch = 16,
     col = rgb(0, 0, 0, alpha = 0.8))

optimal_depth = which.max(accuracy_depth) 
optimal_depth #4
optimal_shrinkage = shrink_index[which.max(accuracy_shrinkage)] 
optimal_shrinkage #0.1975

# Training model on 100% of the dataset
boost_model <- gbm(Result ~ ., data = df_bat, distribution = "multinomial",
                   n.trees = 15, interaction.depth = optimal_depth, shrinkage = optimal_shrinkage)

# Predict on the training data
predicted_probabilities <- predict(boost_model, df_bat, n.trees = 15, type = "response")
predicted_labels <- ifelse(predicted_probabilities > 0.5, 1, 0)

# Calculate training error
final_predictions <- apply(predicted_labels[,,1], 1, which.max) - 1
cm_rf <- table(final_predictions, df_bat$Result)
train_accuracy <- sum(diag(cm_rf)) / sum(cm_rf)
train_error <- 1 - train_accuracy
print(paste("Training error:", train_error))

# Print predicted probabilities
print("Predicted probabilities:")
print(predicted_probabilities)

# Print important variables
importance <- summary(boost_model, n.trees = 15, plot = FALSE)
print("Important variables:")
print(importance[importance[, 1] > (100 / 16), ])
#avg_score team_score avg_wickets_out team_wickets days_from_start team_code winner_code

# Perform 10-fold CV for test error
folds <- createFolds(df_bat$Result, k = 10)
cv_errors <- c()

for (j in 1:10) {
  train_data <- df_bat[-folds[[j]],]
  test_data <- df_bat[folds[[j]],]
  
  cv_boost_model <- gbm(Result ~ ., data = train_data, distribution = "multinomial",
                        n.trees = 15, interaction.depth = optimal_depth, shrinkage = optimal_shrinkage)
  
  predicted_probabilities <- predict(cv_boost_model, test_data, n.trees = 15, type = "response")
  predicted_labels <- apply(predicted_probabilities, 1, which.max) - 1
  
  cm_rf <- table(predicted_labels, test_data$Result)
  accuracy <- (cm_rf[1, 1] + cm_rf[2, 2]) / nrow(test_data)
  
  cv_errors[j] <- 1 - accuracy
}

print(paste("Mean 10-fold CV test error:", mean(cv_errors)))
#boosting test error：0.338228438228438



#### add new data #####
df_bat_new <- df_bat[, c("Result", "team_code", "opponent_code", "season",
                         "gender", "days_from_start",
                         "home_advantage", "venue_city_code")]

new_data <- read.csv("~/Desktop/dissertation_dataset/df_2023.csv")
combined_data <- rbind(df_bat_new, new_data)


phat_full_logistic <- predict(binomial_model, newdata = combined_data, type = "response")
yhat_full_logistic <- as.numeric(ifelse(phat_full_logistic > 0.5, 1, 0)) 

combined_data_subset <- combined_data[, c('team_score', 'gender', 'avg_wickets_out')]
phat_forward_stepwise <- predict(forward_step_logit_model, newdata = combined_data_subset, type = "response")
yhat_forward_stepwise <- as.numeric(ifelse(phat_forward_stepwise > 0.5, 1, 0)) 
