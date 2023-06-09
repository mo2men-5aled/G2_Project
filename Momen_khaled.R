library(ggplot2)

# Importing the dataset
# filepath <- file.path(getwd(), 'G4_howell.csv')
df <- read.csv("./G2_anthropometry.csv")

# Showing the first 5 values
head(df)

# Showing the last 5 values
tail(df)

# Checking for the shape of the Values
dim(df)

# Getting some stats
str(df)
summary(df)

# Convert the Age column to integer
df$age <- as.integer(df$age)

# Convert the Gender Column
df$gender <- ifelse(df$gender == 'cm', 'M', df$gender)

# Impute missing values in the foot_length column
mean_foot_length <- mean(df$foot_length, na.rm = TRUE)
df$foot_length[is.na(df$foot_length)] <- mean_foot_length
df$foot_length <- round(df$foot_length, 2)

# Remove 'cm' from the height column
df$height <- gsub(' cm', '', df$height)

# Convert "height" column to numeric values
df$height <- as.numeric(df$height)

# Create a pie chart of gender distribution
gender_counts <- table(df$gender)
pie(gender_counts, labels = names(gender_counts), autopct='%1.1f%%')
title("Gender Distribution")

# Create a scatter plot of height and age
ggplot(df, aes(x = age, y = height)) +
  geom_point() +
  xlab("Age") +
  ylab("Height") +
  ggtitle("Relationship between Height and Age")

# Find the highest height in males and females
male_highest <- max(df$height[df$gender == 'M'])
female_highest <- max(df$height[df$gender == 'F'])
cat("Highest height in males:", male_highest, "\n")
cat("Highest height in females:", female_highest, "\n")

# Create a histogram of female heights
female_height <- df$height[df$gender == 'F']
hist(female_height, breaks = 10, xlab = "Height", ylab = "Frequency", main = "Histogram of Female Height")

# Create a histogram of male heights
male_height <- df$height[df$gender == 'M']
hist(male_height, breaks = 10, xlab = "Height", ylab = "Frequency", main = "Histogram of Male Height")
