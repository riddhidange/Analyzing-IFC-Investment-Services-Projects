library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)

ifc_data <- read.csv('/Users/riddhidange/Desktop/Academics/fall2023/FA582/Project/Datasets/IFC_Investment_Services_Projects_20231023.csv')

View(ifc_data)


# Data Preprocessing
# Let's start by understanding the structure of the dataset
str(ifc_data)


# Checking for missing values
missing_values <- ifc_data %>%
  summarise_all(~ sum(is.na(.)))

# View the missing value counts for each column
missing_values

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


head(ifc_data,10)



ifc_data$Date.Disclosed <- as.Date(ifc_data$Date.Disclosed)
ifc_data$Projected.Board.Date <- as.Date(ifc_data$Projected.Board.Date)
ifc_data$IFC.Approval.Date <- as.Date(ifc_data$IFC.Approval.Date)
ifc_data$IFC.Signed.Date <- as.Date(ifc_data$IFC.Signed.Date)
ifc_data$IFC.Invested.Date <- as.Date(ifc_data$IFC.Invested.Date)




# Summary statistics
summary(ifc_data)


# Extract year from date columns
ifc_data$Year_Disclosed <- format(ifc_data$Date.Disclosed, "%Y")
ifc_data$Year_Approval <- format(ifc_data$IFC.Approval.Date, "%Y")
ifc_data$Year_Signed <- format(ifc_data$IFC.Signed.Date, "%Y")
ifc_data$Year_Invested <- format(ifc_data$IFC.Invested.Date, "%Y")



# Drop missing values
ifc_data_cleaned <- na.omit(ifc_data)

# Extract the year if investment 
ifc_data_cleaned$Year_Invested <- format(as.Date(ifc_data_cleaned$IFC.Invested.Date), "%Y")

# PLOT 1: Exploring the Number of Projects Invested Each Year
projects_per_year <- ifc_data_cleaned %>%
  group_by(Year_Invested) %>%
  summarise(NumProjects = n())

# Plot the results
plot1<-ggplot(projects_per_year, aes(x = Year_Invested, y = NumProjects, fill = Year_Invested)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Projects Each Year",
       x = "Year",
       y = "Number of Projects") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot1)
ggsave("output_no_projects_invested_plot.png", plot1, width = 10, height = 6)



# PLOT 2: Exploring the Industries of Projects
projects_by_industry <- ifc_data_cleaned %>%
  group_by(Year_Invested, Industry) %>%
  summarise(NumProjects = n())

# Stacked Bar Plot with adjusted legend size
plot2 <- ggplot(projects_by_industry, aes(x = Year_Invested, y = NumProjects, fill = Industry)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Shifts in Focus Areas of Industries Over Time",
       x = "Year",
       y = "Number of Projects",
       fill = "Industry") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    legend.position = "right",  # Move legend to the right
    legend.title = element_text(size = 10),  # Adjust legend title size
    legend.text = element_text(size = 8),  # Adjust legend text size
    plot.title = element_text(size = 14, face = "bold")  # Adjust plot title size
  )

print(plot2)
# Save the plot with adjusted width
ggsave("output_industry_year_invested_plot.png", plot2, width = 10, height = 6)


# PLOT 3: Changes in product lines over time

filtered_data <- ifc_data_cleaned %>%
  filter(Product.Line != "")

# Group the data by Invested Date and Product.Line and count the occurrences
grouped_data_pdt <- filtered_data %>%
  group_by(Year_Invested, Product.Line) %>%
  summarise(count = n())

# Plot the changes in product lines over time
plot3<-ggplot(grouped_data_pdt, aes(x = Year_Invested, y = count, color = Product.Line)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Count", title = "Changes in Product Lines Over Time") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot3)
ggsave("output_product_lines_over_time.png", plot3, width = 10, height = 6)
head(ifc_data_cleaned)

column_names <- names(ifc_data_cleaned)
date_columns <- c("Date.Disclosed", "Projected.Board.Date", "IFC.Approval.Date", "IFC.Signed.Date", "IFC.Invested.Date")

if (all(date_columns %in% column_names)) {
  timeline_data <- ifc_data_cleaned[date_columns]
} else {
  stop("The values in `date_columns` are not valid column names in `ifc_data_cleaned`.")
}

# PLOT 4: Timeline Analysis

ifc_data_cleaned[date_columns] <- lapply(ifc_data_cleaned[date_columns], as.Date)

# Calculate the duration for each project stage
timeline_data$disclosed_to_board <- as.numeric(timeline_data$Projected.Board.Date - timeline_data$Date.Disclosed)
timeline_data$board_to_approval <- as.numeric(timeline_data$IFC.Approval.Date - timeline_data$Projected.Board.Date)
timeline_data$approval_to_signed <- as.numeric(timeline_data$IFC.Signed.Date - timeline_data$IFC.Approval.Date)
timeline_data$signed_to_invested <- as.numeric(timeline_data$IFC.Invested.Date - timeline_data$IFC.Signed.Date)

typeof(timeline_data$disclosed_to_board)

# Calculate the average duration for each project stage
average_duration <- colMeans(timeline_data[c("disclosed_to_board", "board_to_approval", "approval_to_signed", "signed_to_invested")], na.rm = TRUE)

# Create a bar plot of average duration
plot_data <- data.frame(stage = names(average_duration), duration = average_duration)
plot4<-ggplot(plot_data, aes(x = stage, y = duration)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Project Stage", y = "Average Duration", title = "Average Duration for Each Project Stage")

print(plot4)
ggsave("output_avg_duration.png", plot4, width = 10, height = 6)


# Calculate the average disclosure period for each environmental category
ifc_data_cleaned$disclosure_period_abs <- abs(as.numeric(ifc_data_cleaned$Date.Disclosed) - as.numeric(ifc_data_cleaned$Projected.Board.Date))

# Calculate the average disclosure period for each environmental category
average_disclosure_period <- ifc_data_cleaned %>%
  group_by(Environmental.Category) %>%
  summarize(mean_disclosure_period = mean(disclosure_period_abs))

# Print the average disclosure period for each environmental category
print(average_disclosure_period)


#  PLOT 5: 


filtered_data <- ifc_data_cleaned %>%
  filter(!is.na(Environmental.Category))

grouped_data <- filtered_data %>%
  group_by(Environmental.Category, Industry) %>%
  summarize(mean_disclosure_period = mean(disclosure_period_abs))

plot5<-ggplot(grouped_data, aes(x = Environmental.Category, y = mean_disclosure_period, fill = Industry)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Environmental Category", y = "Mean Disclosure Period (days)") +
  facet_wrap(~ Industry)

print(plot5)
ggsave("output_env_category_industry.png", plot5, width = 10, height = 6)

