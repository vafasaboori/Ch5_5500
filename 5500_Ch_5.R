# R Script: Introduction and Examples for Discrete Probability Distributions with Visualizations

# Load necessary libraries
install.packages("ggplot2")

# Introduction to R
# RStudio for the Total Beginner Video: https://youtu.be/FIrsOBy5k58?feature=shared
cat("R is a programming language and environment for statistical computing and graphics. It is widely used for data analysis and statistical modeling.\n\n")


# Example 5.1: Random Variables

## Discrete Random Variables
# Example: Rolling a die
die_rolls <- 1:6
probabilities <- rep(1/6, 6)
cat("Discrete Random Variable Example:\n")
cat("Possible outcomes of rolling a die:", die_rolls, "\n")
cat("Probability for each outcome:", probabilities, "\n\n")

# Visualization
barplot(probabilities, 
        names.arg = die_rolls, 
        col = "lightblue", 
        main = "Probability of Rolling a Die")
# names.arg assigns the labels on the x-axis of the bar plot (1, 2, 3, 4, 5, 6)

## Continuous Random Variables
# Conceptual example for continuous variables
cat("Continuous Random Variable Example:\n")
cat("Heights of adults can be modeled as a continuous variable, but we simulate some data.\n\n")

# Simulated example for height distribution (normal distribution)
height_data <- rnorm(1000, mean = 170, sd = 10)
hist(height_data, breaks = 20, 
     col = "lightgreen", 
     main = "Simulated Height Distribution", 
     xlab = "Height (cm)")

# Example 5.2: Developing Discrete Probability Distributions

# Example: Creating a probability distribution for a discrete random variable
prob_dist <- data.frame(x = die_rolls, p = probabilities)
cat("Discrete Probability Distribution Example:\n")
print(prob_dist)
cat("\n")

# Visualization
library(ggplot2)
ggplot(prob_dist, aes(x = factor(x), y = p)) + # factor(x): categorical variable
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Discrete Probability Distribution", x = "Outcomes", y = "Probability")

# Example 5.3: Expected Value and Variance

## Expected Value and Variance for the Discrete Distribution
expected_value <- sum(prob_dist$x * prob_dist$p)
variance <- sum((prob_dist$x - expected_value)^2 * prob_dist$p)
cat("Expected Value and Variance Example:\n")
cat("Expected Value:", expected_value, "\n")
cat("Variance:", variance, "\n\n")

# Visualization: Expected value as a vertical line
ggplot(prob_dist, aes(x = factor(x), y = p)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_vline(xintercept = expected_value, linetype = "dashed", color = "red") +
  labs(title = "Expected Value and Variance", x = "Outcomes", y = "Probability")

# Example 5.4: Bivariate Distributions, Covariance

## Bivariate Empirical Discrete Probability Distribution
# Example Data: (X, Y) pairs
xy_data <- data.frame(X = c(1, 2, 3, 4), Y = c(2, 4, 6, 8), P = c(0.1, 0.2, 0.3, 0.4))
cat("Bivariate Empirical Discrete Probability Distribution Example:\n")
print(xy_data)
cat("\n")

# Visualization: Scatter plot for bivariate distribution
ggplot(xy_data, aes(x = X, y = Y, size = P)) +
  geom_point(color = "darkblue") +
  labs(title = "Bivariate Empirical Discrete Probability Distribution", x = "X", y = "Y")

# Dealership Example (Table 5.7)
# Create the data frame with x (Saratoga), y (Geneva), and z (Frequency) columns
sales_data <- data.frame(
  Saratoga = rep(0:5, times = 4),  # Repeats 0 to 5 for each Geneva value (4 times)
  Geneva = rep(0:3, each = 6),     # Repeats each Geneva value (0 to 3) for 6 Saratoga values
  Frequency = c(21, 30, 24, 9, 2, 0, 21, 36, 33, 18, 2, 1, 9, 42, 9, 12, 3, 2, 3, 9, 6, 3, 5, 3)
)

# Display the first few rows of the data frame
print(head(sales_data))

# Calculate total days (sum of frequencies)
total_days <- sum(sales_data$Frequency)

# Add a probability column (joint probability of each combination of sales)
sales_data$Probability <- sales_data$Frequency / total_days

# View the updated data frame with probabilities
print(head(sales_data))

# Bar plot for Saratoga dealership
barplot(
  tapply(sales_data$Frequency, sales_data$Saratoga, sum), 
  # tapply groups Frequency values by unique values of Saratoga and applies sum
  names.arg = unique(sales_data$Saratoga),
  col = "lightblue", main = "Marginal Distribution: Saratoga Dealership Sales", 
  xlab = "Cars Sold", ylab = "Frequency"
)

# Bar plot for Geneva dealership
barplot(
  tapply(sales_data$Frequency, sales_data$Geneva, sum), 
  names.arg = unique(sales_data$Geneva),
  col = "lightgreen", main = "Marginal Distribution: Geneva Dealership Sales", 
  xlab = "Cars Sold", ylab = "Frequency"
)

# Heatmap for joint probability distribution
ggplot(sales_data, aes(x = Saratoga, y = Geneva, fill = Probability)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Joint Probability Distribution of Car Sales", 
       x = "Saratoga Dealership", y = "Geneva Dealership", fill = "Probability") +
  theme_minimal()


# Example 5.5: Binomial Probability Distribution

## Binomial Experiment
# Example: Flipping a coin 10 times, probability of heads = 0.5
n <- 10  # number of trials
p <- 0.5  # probability of success
binom_dist <- dbinom(0:n, size = n, prob = p)
cat("Binomial Probability Distribution Example:\n")
cat("Probabilities of getting 0 to", n, "heads in 10 flips:\n")
print(binom_dist)
cat("\n")

# Visualization: Bar plot for binomial distribution
binom_data <- data.frame(Heads = 0:n, Probability = binom_dist)
ggplot(binom_data, aes(x = factor(Heads), y = Probability)) +
  geom_col(fill = "purple") +
  labs(title = "Binomial Probability Distribution", x = "Number of Heads", y = "Probability")

# Example 5.6: Poisson Probability Distribution

## Poisson Distribution for Time Intervals
# Example: Number of events in an hour with lambda = 3
lambda <- 3 # average rate (mean) of occurrences in a Poisson distribution
poisson_dist <- dpois(0:10, lambda = lambda)
cat("Poisson Probability Distribution Example (Time Intervals):\n")
cat("Probabilities of 0 to 10 events in an hour:\n")
print(poisson_dist)
cat("\n")

# Visualization: Poisson distribution for time intervals
poisson_data <- data.frame(Events = 0:10, Probability = poisson_dist)
ggplot(poisson_data, aes(x = factor(Events), y = Probability)) +
  geom_col(fill = "orange") +
  labs(title = "Poisson Distribution (Time Intervals)", x = "Number of Events", y = "Probability")

## Poisson Distribution for Length or Distance Intervals
# Example: Number of defects per meter with lambda = 2
lambda_dist <- 2
poisson_dist_length <- dpois(0:10, lambda = lambda_dist)
cat("Poisson Probability Distribution Example (Length or Distance Intervals):\n")
cat("Probabilities of 0 to 10 defects per meter:\n")
print(poisson_dist_length)
cat("\n")

# Visualization: Poisson distribution for length/distance intervals
poisson_length_data <- data.frame(Defects = 0:10, Probability = poisson_dist_length)
ggplot(poisson_length_data, aes(x = factor(Defects), y = Probability)) +
  geom_col(fill = "cyan") +
  labs(title = "Poisson Distribution (Length/Distance Intervals)", x = "Number of Defects", y = "Probability")

# Example 5.7: Hypergeometric Probability Distribution

## Hypergeometric Distribution
# Example: Drawing 5 red cards from a deck of 52 with 12 red cards
r <- 12
N <- 52
n <- 5
x <- 0:5
hypergeo_dist <- dhyper(x, # number of successes in the sample (i.e., red cards drawn).
                        m = r, # number of successes in the population (i.e., red cards in the deck).
                        n = N - r, # number of failures in the population (i.e., non-red cards in the deck).
                        k = n) # number of draws or sample size
cat("Hypergeometric Probability Distribution Example:\n")
cat("Probabilities of drawing 0 to 5 red cards in 5 draws:\n")
print(hypergeo_dist)
cat("\n")

# Visualization: Hypergeometric distribution
hypergeo_data <- data.frame(Successes = x, Probability = hypergeo_dist)
ggplot(hypergeo_data, aes(x = factor(Successes), y = Probability)) +
  geom_col(fill = "green") +
  labs(title = "Hypergeometric Distribution", x = "Number of Red Cards", y = "Probability")

