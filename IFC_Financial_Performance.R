library(ggplot2)
library(readr)
library(tidyverse)
library(dplyr)
library(reshape2)


#PRODUCT
Product <- read_csv("/Users/riddhidange/Desktop/Academics/fall2023/FA582/Project/Datasets/IFC_Investment_By_Product_-_Annual_Summary.csv")

financial_data_melted <- melt(Product, id.vars = "Fiscal Year", variable.name = "Metric", value.name = "Value")

ggplot(financial_data_melted, aes(x = `Fiscal Year`, y = Value, color = Metric, group = Metric)) +
  geom_line() +
  geom_point() +
  labs(title = "Financial Metrics Over Fiscal Years",
       x = "Fiscal Year",
       y = "Value (Millions)",
       color = "Metric") +
  theme_minimal()


#REGION
Region <- read_csv("/Users/riddhidange/Desktop/Academics/fall2023/FA582/Project/Datasets/IFC_Investment_By_Region_-__Annual_Summary.csv")



Region_melted <-  Region %>% 
  pivot_longer(cols = -`Fiscal Year`, names_to = "Region", values_to = "Value")
ggplot(Region_melted, aes(x = `Fiscal Year`, y = Value, color = Region, group = Region)) +
  geom_line() +
  geom_point() +
  labs(title = "Financial Analysis Over Fiscal Years",
       x = "Fiscal Year",
       y = "Value",
       color = "Region") +
  theme_minimal()


#INDUSTRY
Industry <- read_csv("/Users/riddhidange/Desktop/Academics/fall2023/FA582/Project/Datasets/IFC_Investment_By_Industry_-_Annual_Summary.csv")

str(Industry)

industry_data_melted <- Industry %>% 
  pivot_longer(cols = -`Fiscal Year` , names_to = "Industry", values_to = "Value")
print(ggplot(industry_data_melted, aes(x = `Fiscal Year`, y = Value, color = Industry, group = Industry)) +
  geom_line() +
  geom_point() +
  labs(title = "Financial Analysis Over Fiscal Years by Industry",
       x = "Fiscal Year",
       y = "Value",
       color = "Industry") +
  theme_minimal())


#FINANCIAL HIGHLIGHTS

financial_data <- read_csv("/Users/riddhidange/Desktop/Academics/fall2023/FA582/Project/Datasets/IFC_Financial_Highlights.csv")

summary(financial_data)

financial_data$`Debt-to-equity ratio`<-sapply(strsplit(financial_data$`Debt-to-equity ratio`, ":"), function(x) as.numeric(x[1])/as.numeric(x[2]))

par(mfrow=c(2, 2))
plot(financial_data$`Fiscal Year`, financial_data$`Net income (loss), $ millions`, type="l", col="blue", xlab="Year", ylab="Net Income ($ millions)", main="Net Income Over Years")
plot(financial_data$`Fiscal Year`, financial_data$`Overall liquidity ratio`, type="l", col="red", xlab="Year", ylab="Liquidity Ratio", main="Liquidity Ratio Over Years")
plot(financial_data$`Fiscal Year`, financial_data$`Debt-to-equity ratio`, type="l", col="green", xlab="Year", ylab="Debt-to-Equity Ratio", main="Debt-to-Equity Ratio Over Years")
plot(financial_data$`Fiscal Year`, financial_data$`Total reserve against losses on loans to total disbursed loan portfolio`, type="l", col="purple", xlab="Year", ylab="Total Reserve", main="Total Reserve Over Years")


#INCOME STATEMENT
Income_statement <- read_csv("/Users/riddhidange/Desktop/Academics/fall2023/FA582/Project/Datasets/IFC_Summary_Consolidated_Income_Statement.csv")

str(Income_statement)


# Calculate total income
total_income <- Income_statement %>%
  filter(grepl("Income", `Line Item` )) %>%
  summarise(Total_Income = sum(`Amount (US$, Millions)`))

# Calculate total expenses
total_expenses <- Income_statement %>%
  filter(!grepl("Income", `Line Item` )) %>%
  summarise(Total_Expenses = sum(`Amount (US$, Millions)`))

# Calculate net income
net_income <- total_income$Total_Income - total_expenses$Total_Expenses

cat("Total Income:", total_income$Total_Income, "\n")
cat("Total Expenses:", total_expenses$Total_Expenses, "\n")
cat("Net Income:", net_income, "\n")

ggplot(Income_statement, aes(x = `Line Item`, y = `Amount (US$, Millions)`, fill = `Line Item` )) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Line Item", y = "Amount", title = "Income and Expenses")

