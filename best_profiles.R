dataset <- read.csv("profiles.csv")
library(dplyr)

dataset<- dataset %>%
  filter(!(followers == 0 & likes == 0 & videos_num == 0 & videos_views == 0))

# Unique list of keywords
keywords <- unique(dataset$keywords)

# Iterate over each keyword
for (keyword in keywords) {
  # Extract data for a specific keyword
  data <- dataset %>%
    filter(keywords == keyword)
  
  filter_data <- select(data, "followers", "likes", "videos_num", "videos_views")
  
  # Standardize the variables to have zero mean and unit variance
  scaled_data <- scale(filter_data)
  
  # Apply PCA
  pca_result <- prcomp(scaled_data, scale. = FALSE)
  
  # Extract scores from the first principal component
  scores <- pca_result$x[,1]
  
  # Order profiles based on scores from the first principal component
  top_30_profiles <- head(data[order(scores, decreasing = TRUE), ], 30)
  
  # Create the file name
  file_name <- paste(keyword, "_best_profiles.csv", sep = "")
  
  # Save the top 30 profiles to a CSV file
  write.csv(top_30_profiles, file = file_name, row.names = FALSE)
  
  # Print message indicating completion for the current keyword
  cat("Completed for keyword:", keyword, "\n")
}
