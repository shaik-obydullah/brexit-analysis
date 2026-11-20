# Necessary libraries
library(ggplot2)
library(tidyr)
library(reshape2)
library(car)

# Import the file
data <- read.csv("brexit_data.csv")

# Checking for missing values #

# To indices of rows with missing values
rows_with_missing <- which(apply(data, 1, function(row) any(is.na(row))))

# To indices of columns with missing values
cols_with_missing <- which(colSums(is.na(data)) > 0)

# To indices subset of data containing only the rows with missing values.
data_with_missing <- data[apply(data, 1, function(row) any(is.na(row))), ]

# To indices subset of data containing only the columns with missing values
data[, colSums(is.na(data)) > 0]

# Remove duplicate entries
data <- unique(data)

# Filter out non-finite values in the 'Age_Mean' column. Since the data-set has non-finite values in the 'Age_Mean' column
data <- data[is.finite(data$age_mean), ]

dir.create("plots", showWarnings = FALSE)

# Plot 1: Histogram of respondents by Age
p1 <- ggplot(data, aes(x=age_mean)) +
  geom_histogram(binwidth = 1, fill="skyblue", color="black") +
  labs(title="Distribution of Respondents by Age Mean", x="Age Mean", y="Number of Respondents") +
  theme_minimal()
ggsave("plots/1_age_distribution.png", p1, width = 8, height = 6, dpi = 150)

# Reshape the data into a longer format
brexit_data_long <- pivot_longer(data, cols = c(Percent_Remain, Percent_Leave),
                                 names_to = "Vote_Type", values_to = "Percentage")

# Plot 2: Bar chart for comparison of remain and leave votes by the UK Regions
p2 <- ggplot(brexit_data_long, aes(x = reorder(Region, Percentage), y = Percentage, fill = Vote_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Remain and Leave Votes by Region in the UK",
       x = "Region",
       y = "Percentage of Votes",
       fill = "Vote Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("red", "blue"), labels = c("Leave", "Remain")) +
  guides(fill = guide_legend(title = "Vote Type")) +
  coord_flip()
ggsave("plots/2_remain_leave_by_region.png", p2, width = 10, height = 8, dpi = 150)

# Plot 3: Scatter plot matrix of age, income, and Brexit
png("plots/3_age_income_brexit_matrix.png", width = 1200, height = 1200, res = 150)
scatterplotMatrix(~ age_mean + Earnings_Mean + Percent_Leave, data = data,
                  main = "Scatter Plot Matrix of Age, Income, and Brexit")
dev.off()

# Plot 4: Heatmap of socio-economic factors
heatmap_data <- data[, c("Percent_Leave",
                                "Economically_active_percent",
                                "Employed_of_economically_active_percent",
                                "Unemployed_Age50_74_percent",
                                "Occup_intermediate_percent",
                                "Bachelors_deg_percent",
                                "Birth_MidEast_Asia_percent")]

# Compute the correlation matrix
correlation_matrix <- cor(heatmap_data, use = "pairwise.complete.obs")

# Reshape the correlation matrix for plotting
melted_correlation <- melt(correlation_matrix)

# Heatmap
p4 <- ggplot(melted_correlation, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 10, hjust = 1)) +
  coord_fixed() +
  labs(title = "Correlation Heatmap of Brexit Data",
       x = "Variables", y = "Variables")
ggsave("plots/4_correlation_heatmap.png", p4, width = 8, height = 8, dpi = 150)

cat("All 4 plots saved to /app/plots/\n")
