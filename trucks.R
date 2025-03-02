install.packages('factoextra')

library(tidyverse)
library(tidymodels)
library(cluster) # for clustering
library(factoextra) # for visualization
library(ggplot2)
# Load dataset
trucks <- read_csv("trucks-1.csv")

# Scatter plot of Distance vs Speeding
ggplot(trucks, aes(x = Distance, y = Speeding)) +
  geom_point(alpha = 0.5) +
  labs(title = "Relationship between Distance and Speeding",
       x = "Distance",
       y = "Speeding") +
  theme_minimal()
 # Remove Driver_ID and scale the predictor variables
trucks_cleaned <- trucks %>%
  select(Distance, Speeding) %>%
  scale() %>%
  as_tibble()

# Get the maximum scaled value of Distance
max_distance_scaled <- max(trucks_cleaned$Distance)
max_distance_scaled

set.seed(64) # Set seed for reproducibility

# Perform k-means clustering with k=2
kclust_2 <- kmeans(trucks_cleaned, centers = 2)

# Add clusters to the original dataset
trucks <- trucks %>%
  mutate(Cluster = as.factor(kclust_2$cluster))

# Plot clusters
ggplot(trucks, aes(x = Distance, y = Speeding, color = Cluster)) +
  geom_point(alpha = 0.6) +
  labs(title = "Clusters of Drivers (k=2)",
       x = "Distance",
       y = "Speeding") +
  theme_minimal()

set.seed(412)

# Compute k-means clustering for k = 1 to 8
wcss_values <- map_dbl(1:8, function(k) {
  kmeans(trucks_cleaned, centers = k, nstart = 10)$tot.withinss
})

# Create a data frame for plotting
elbow_df <- tibble(k = 1:8, WCSS = wcss_values)

# Plot elbow method
ggplot(elbow_df, aes(x = k, y = WCSS)) +
  geom_line() +
  geom_point() +
  labs(title = "Elbow Method for Optimal k",
       x = "Number of Clusters (k)",
       y = "Within-Cluster Sum of Squares") +
  theme_minimal()

ggplot(elbow_df, aes(x = k, y = WCSS)) +
  geom_line() +
  geom_point() +
  labs(title = "Optimal Number of Clusters",
       x = "Number of Clusters (k)",
       y = "WCSS") +
  theme_minimal()

# Assuming the best k found in Question 5 is k_optimal
k_optimal <- 4 # Change this based on the elbow method result

set.seed(64)
kclust_optimal <- kmeans(trucks_cleaned, centers = k_optimal)

# Add optimal clusters to the dataset
trucks <- trucks %>%
  mutate(Cluster = as.factor(kclust_optimal$cluster))

# Plot the clusters
ggplot(trucks, aes(x = Distance, y = Speeding, color = Cluster)) +
  geom_point(alpha = 0.6) +
  labs(title = paste("Clusters of Drivers (k =", k_optimal, ")"),
       x = "Distance",
       y = "Speeding") +
  theme_minimal()
