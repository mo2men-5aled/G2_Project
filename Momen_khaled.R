
# import the required libraries
library(ggplot2)

# Importing the data set
data <- read.csv("./G2_anthropometry.csv")

# Showing the first 5 values
head(data)

# Showing the last 5 values
tail(data)

# Checking for the shape of the Values
dim(data)

# Getting some stats about the data 
str(data)
summary(data)

# Convert the Age column to integer
data$age <- as.integer(data$age)

# Convert the Gender Column
data$gender <- ifelse(data$gender == 'cm', 'M', data$gender)

# complete missing values in the foot_length column
mean_foot_length <- mean(data$foot_length, na.rm = TRUE)
data$foot_length[is.na(data$foot_length)] <- mean_foot_length
data$foot_length <- round(data$foot_length, 2)

# Remove 'cm' from the height column
data$height <- gsub(' cm', '', data$height)

# Convert "height" column to numeric values
data$height <- as.numeric(data$height)

# Create a pie chart of gender distribution
gender_counts <- table(data$gender)
pie(gender_counts, labels = names(gender_counts), autopct='%1.1f%%')
title("Gender Distribution")

# Create a scatter plot of height and age
ggplot(data, aes(x = age, y = height)) +
  geom_point() +
  xlab("Age") +
  ylab("Height") +
  ggtitle("Relationship between Height and Age")

# Create a scatter plot of age and foot_length
ggplot(data, aes(x = age, y = foot_length)) +
  geom_point() +
  xlab("Age") +
  ylab("Foot Length") +
  ggtitle("Relationship between Age and Foot Length")

# Create a scatter plot of height and foot_length
ggplot(data, aes(x = height, y = foot_length)) +
  geom_point() +
  xlab("Height") +
  ylab("Foot Length") +
  ggtitle("Relationship between Height and Foot Length")

# Create a scatter plot of age and foot_length
ggplot(data, aes(x = age, y = foot_length)) +
  geom_point() +
  xlab("Age") +
  ylab("Foot Length") +
  ggtitle("Relationship between Age and Foot Length")

# Find the highest height in males and females
male_highest <- max(data$height[data$gender == 'M'])
female_highest <- max(data$height[data$gender == 'F'])
cat("Highest height in males:", male_highest, "\n")
cat("Highest height in females:", female_highest, "\n")

# Create a histogram of female heights
female_height <- data$height[data$gender == 'F']
hist(female_height, breaks = 10, xlab = "Height", ylab = "Frequency", main = "Histogram of Female Height")

# Create a histogram of male heights
male_height <- data$height[data$gender == 'M']
hist(male_height, breaks = 10, xlab = "Height", ylab = "Frequency", main = "Histogram of Male Height")


# Create a histogram of male foot_length
male_foot_length <- data$foot_length[data$gender == 'M']
hist(male_foot_length, breaks = 10, xlab = "Foot Length", ylab = "Frequency", main = "Histogram of Male Foot Length")


# Create a histogram of female foot_length
female_foot_length <- data$foot_length[data$gender == 'F']
hist(female_foot_length, breaks = 10, xlab = "Foot Length",ylab = "Frequency", main = "Histogram of Male Foot Length")