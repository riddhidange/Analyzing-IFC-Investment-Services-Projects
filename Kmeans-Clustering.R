library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(kableExtra)
library(factoextra)


# Read data
ifc_data <- read.csv('/Users/riddhidange/Desktop/Academics/fall2023/FA582/Project/Datasets/IFC_Investment_Services_Projects_20231023.csv')

# Data Preprocessing
columns_to_replace_na <- c("IFC.investment.for.Risk.Management.Million...USD.",
                           "IFC.investment.for.Guarantee.Million...USD.",
                           "IFC.investment.for.Loan.Million...USD.",
                           "IFC.investment.for.Equity.Million...USD.",
                           "Total.IFC.investment.as.approved.by.Board.Million...USD.")

for (col in columns_to_replace_na) {
  ifc_data[is.na(ifc_data[, col]), col] <- 0
}

date_columns <- c("Projected.Board.Date", "IFC.Approval.Date", "IFC.Signed.Date", "IFC.Invested.Date", "Date.Disclosed")

for (col in date_columns) {
  ifc_data[, col] <- mdy(ifc_data[, col])
}

# Drop the following columns
ifc_data <- ifc_data[, -which(names(ifc_data) == "As.of.Date")]
ifc_data <- ifc_data[, -which(names(ifc_data) == "WB.Country.Code")]
ifc_data <- ifc_data[, -which(names(ifc_data) == "Project.Url")]

# Check for missing values before creating Decade variable
missing_date_values <- which(is.na(ifc_data$IFC.Invested.Date))
if (length(missing_date_values) > 0) {
  cat("Warning: Missing values in IFC.Invested.Date Rows with missing values will be excluded.\n")
  ifc_data <- ifc_data[-missing_date_values, ]
}

# Extract decade from Date Disclosed
ifc_data$Decade <- floor(year(ifc_data$IFC.Invested.Date) / 10) * 10



head(ifc_data)


# Select relevant columns for clustering
selected_columns <- c("IFC.investment.for.Risk.Management.Million...USD.",
                      "IFC.investment.for.Guarantee.Million...USD.",
                      "IFC.investment.for.Loan.Million...USD.",
                      "IFC.investment.for.Equity.Million...USD.")


# pdf("cluster_plots.pdf")
# Loop through decades
for (decade in unique(ifc_data$Decade)) {
  # Filter data for the specific decade
  ifc_data_decade <- ifc_data[ifc_data$Decade == decade, selected_columns]
  
  # Handle missing values, if any
  ifc_data_decade <- na.omit(ifc_data_decade)
  
  # Check if there are enough data points for clustering
  k <- 3
  if (nrow(ifc_data_decade) < k) {
    #cat("Not enough data points for clustering with", k, "clusters in Decade", decade, "\n")
    next  # Skip to the next iteration if not enough data
  }
  
  # Normalize data if needed
  scaled_data <- scale(ifc_data_decade)
  
  # Perform k-means clustering with, say, 3 clusters
  kmeans_result <- kmeans(scaled_data, centers = k, nstart = 100)
  
  # Add cluster assignment to the original data
  ifc_data_decade$Cluster <- kmeans_result$cluster
  
  # Check the cluster centers
  cluster_centers <- kmeans_result$centers
  
  
  # EDA with Clusters
  # Boxplot for each cluster in each selected feature
  
  for (col in selected_columns) {
    # Create a ggplot boxplot
    p<-ggplot(ifc_data_decade, aes(x = as.factor(Cluster), y = .data[[col]])) +
      geom_boxplot() +
      labs(title = paste("Boxplot by Cluster - Decade", decade),
           x = "Cluster", y = col) 
    print(p)
      # ggsave(paste("boxplot_decade_", decade, "_", col, ".png"), device = "png")
  }
  
  res.km <- kmeans(scaled_data, centers = k, nstart = 100)  # re-run kmeans for plotting
  p <- fviz_cluster(res.km, data = ifc_data_decade[, -which(names(ifc_data_decade) == "Cluster")],
                    ellipse.type = "convex", ggtheme = theme_bw(),
                    main = paste("Cluster Visualization - Decade", decade),geom="point",
                    show.clust.cent = FALSE)

  print(p)
  
  # Save the cluster plot
  # dev.copy(pdf, paste("cluster_plot_decade_", decade, ".pdf"))
  
  # Display Centroids
  cat(paste("Centroids for Decade", decade, ":\n"))
  print(cluster_centers)

}


