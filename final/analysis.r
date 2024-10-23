# # Install the readr package if it is not already installed
#install.packages("readr")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("ggplot2")


# Load necessary library
library(readr)
library(dplyr) # Load necessary library for data manipulation
library(tidyr)
library(ggplot2)

# Read the CSV file
run_table <- read_csv("final/run_table.csv")

# Display the first few rows of the data
head(run_table)

# Select the specified columns and sum the CPU usage columns
selected_columns <- select(run_table,'__run_id', llm, language , problem, prompt, energy_usage_package, contains("cpu_usage_"), memory_usage, execution_time, machine_code_size) %>% # nolint
    mutate(cpu_usage_total = rowSums(select(., starts_with("cpu_usage_")))) # nolint
     # nolint
# Create a new dataframe with the selected columns and the summed CPU usage
new_runtable <- select(selected_columns, `__run_id`, llm, language , problem, prompt, energy_usage_package, cpu_usage_total, memory_usage, execution_time, machine_code_size) #nolint

# Display the first few rows of the new dataframe
head(new_runtable)

#summary(new_runtable)

# Read the CSV file
run_table <- read_csv("final/run_table.csv")

# Calculate descriptive statistics for energy_usage_package
stats_table <- run_table %>%
  group_by(problem, prompt, language, llm) %>%
  summarise(
    mean_energy = mean(energy_usage_package, na.rm = TRUE),
    median_energy = median(energy_usage_package, na.rm = TRUE),
    sd_energy = sd(energy_usage_package, na.rm = TRUE),
    min_energy = min(energy_usage_package, na.rm = TRUE),
    max_energy = max(energy_usage_package, na.rm = TRUE),
    .groups = 'drop'
  )
print(stats_table)
# Reshape the data to match the desired table format
# reshaped_table <- stats_table %>%
#   pivot_wider(
#     names_from = c(prompt, language, llm),
#     values_from = c(mean_energy, median_energy, sd_energy, min_energy, max_energy),
#     names_sep = "_"
#   )

# # Display the reshaped table
# print(reshaped_table)

# Create a box plot for energy_usage_package vs language, for every type of prompt and llm
ggplot(run_table, aes(x = language, y = energy_usage_package, color = language)) +
  geom_boxplot(fill = "white") +
  labs(title = "Box Plot of Energy Usage Package by Language, Prompt, and LLM", 
       x = "Language", 
       y = "Energy Usage") +
  facet_grid(prompt ~ llm) +
  scale_color_manual(values = c("cpp" = "red", "haskell" = "green")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Create QQ plots for energy_usage_package vs language, for every type of prompt and llm
ggplot(run_table, aes(sample = energy_usage_package, color = language)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot of Energy Usage Package by Language, Prompt, and LLM", 
       x = "Theoretical Quantiles", 
       y = "Sample Quantiles") +
  facet_grid(prompt ~ llm) +
  scale_color_manual(values = c("cpp" = "red", "haskell" = "green")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))