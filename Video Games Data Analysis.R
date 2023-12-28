rm(list=ls())
library(Hmisc)
library(ggplot2)
library(tidyverse)
games <- read.csv("~/Downloads/Video_Games.csv")
View(games)
dim(games)
describe(games)
 #Which platform is more used?
games$PC <- as.integer(games$Platform == "PC")
sum(games$PC)
sum(games$PC) / nrow(games) #PC=5.8%
games$Wii <- as.integer(games$Platform == "Wii") 
games$PS4 <- as.integer(games$Platform == "PS4") 
games$X <- as.integer(games$Platform == "X360") 
sum(games$Wii) / nrow(games) #Wii=7.8%
sum(games$PS4) / nrow(games) #PS4=2.3%
sum(games$X) / nrow(games) #X360=7.5%

#Is 'Role-Playing' genre more liked than 'Action'?
action = subset(games,games$Genre == "Action")
roleplay = subset(games,games$Genre == "Role-Playing")
mean(action$Critic_Score, na.rm = TRUE) #Action critic score = 66.69175/100
mean(roleplay$Critic_Score, na.rm = TRUE) #Role-playing critic score = 72.73529/100
#Comparing Critic rating to User rating
#Are they contradicting?
action$User_Score[action$User_Score == ""] <- NA #Some user ratings are empty, filling in as NA
action$User_Score <- as.numeric(as.character(action$User_Score)) #Converting character data type to integer
class(action$User_Score)
roleplay$User_Score[roleplay$User_Score == ""] <- NA
roleplay$User_Score <- as.numeric(as.character(roleplay$User_Score))
class(roleplay$User_Score)
mean(action$User_Score, na.rm = TRUE) * 10 #Action user score = 70.53416/100
mean(roleplay$User_Score, na.rm = TRUE) * 10  #Role-playing user score = 76.18858/100
#User rating and Critic rating is more for 'Role-playing' than 'Action'
#Additional conclusion: User ratings are higher than Critic ratings
t.test(action$Critic_Score, roleplay$Critic_Score, alternative="two.sided",conf.level = 0.99)
#Very small p-value (< 2.2e-16) suggests strong evidence against the null hypothesis.
#Negative t-value (-11.079) indicates that the mean critic score for "roleplay" significantly higher than "action".
#The 99% confidence interval (-7.450275 to -4.636823) provides a range of plausible values for the true difference in mean
#It does not include zero, supporting the conclusion of a significant difference.

#Analysis of Critic Score vs. Genre
ggplot(games, aes(x = Genre, y = Critic_Score)) +
  geom_boxplot(aes(fill = Genre), outlier.shape = NA) +  # Remove outliers for better visualization
  theme_minimal() +
  labs(title = "Critic Score vs. Genre", x = "Genre", y = "Critic Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

games$Global_Sales <- as.numeric(as.character(games$Global_Sales))
ordered_data <- games[order(-games$Global_Sales), ] # Order the data by Global_Sales in descending order
top_20_games <- head(ordered_data, 20) # Select the top 20 games by global sales

# Bar plot for the top 20 games by global sales
ggplot(top_20_games, aes(x = reorder(Name, -Global_Sales), y = Global_Sales)) +
  geom_bar(stat = "identity", fill = "pink") +
  theme_minimal() +
  labs(title = "Top 20 Games by Global Sales",
       x = "Game Name",
       y = "Global Sales (in millions)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


total_global_sales <- aggregate(Global_Sales ~ Publisher, data = games, sum)
ordered_publishers <- total_global_sales[order(-total_global_sales$Global_Sales), ]
top_10_publishers <- head(ordered_publishers, 10) # Select the top 10 games by global sales

# Pie chart for the global sales distribution by Publisher
ggplot(top_10_publishers, aes(x = "", y = Global_Sales, fill = Publisher)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  theme_void() +
  labs(title = "Global Sales Distribution by Publisher (Top 10)")


# Convert sales columns to numeric, handling NAs
games$NA_Sales <- as.numeric(as.character(games$NA_Sales))
games$EU_Sales <- as.numeric(as.character(games$EU_Sales))
games$JP_Sales <- as.numeric(as.character(games$JP_Sales))
games$Other_Sales <- as.numeric(as.character(games$Other_Sales))

total_sales <- aggregate(cbind(Global_Sales, NA_Sales, EU_Sales, JP_Sales, Other_Sales) ~ Publisher, data = games, sum)
ordered_publishers <- total_sales[order(-total_sales$Global_Sales), ]
top_20_publishers <- head(ordered_publishers, 20)
melted_data <- gather(top_20_publishers, key = "Variable", value = "Value", -Publisher) # Reshape data for easier plotting
melted_data$Variable <- factor(melted_data$Variable, levels = c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales"))

# Heatmap for sales by Publisher
ggplot(melted_data, aes(x = Variable, y = Publisher, fill = Value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Comparison of Sales by Publisher (Top 20)",
       x = "Sales Type",
       y = "Publisher",
       fill = "Sales Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
