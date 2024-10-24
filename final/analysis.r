# # Install the readr package if it is not already installed
#install.packages("readr")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("cowplot")


# Load necessary library
library(readr)
library(dplyr) # Load necessary library for data manipulation
library(tidyr)
library(ggplot2)
library(cowplot)

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

# Create a new dataframe with the selected columns and make the dependent variables numeric
new_runtable <- selected_columns %>%
  select(`__run_id`, llm, language, problem, prompt, energy_usage_package, cpu_usage_total, memory_usage, execution_time, machine_code_size) %>%
  mutate(
    across(c(energy_usage_package, cpu_usage_total, memory_usage, execution_time, machine_code_size), as.numeric)
  )
# sorted_new_runtable <- new_runtable %>%
#   arrange(energy_usage_package)

head(new_runtable)

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

#Visualizing the data
# Define the columns to plot
columns_to_plot <- c("energy_usage_package", "cpu_usage_total", "memory_usage", "execution_time", "machine_code_size")

# Define human-readable titles for each column
column_titles <- list(
  "energy_usage_package" = "Energy Usage",
  "cpu_usage_total" = "CPU Usage",
  "memory_usage" = "Memory Usage",
  "execution_time" = "Execution Time",
  "machine_code_size" = "Machine Code Size"
)

# Define the unique values for prompt, language, and problem
prompts <- unique(new_runtable$prompt)
languages <- unique(new_runtable$language)
problems <- unique(new_runtable$problem)

# Create a list to store all plots
all_plots <- list()

# Define custom labels for prompt, language, and problem
custom_labels <- list(
  prompt = c("OG" = "Basic", "EE" = "Energy Efficient"),
  language = c("cpp" = "C++", "haskell" = "Haskell"),
  problem = c("PS" = "Pair Sums", "SR" = "String Replacement")
)

#Box Plot
# Iterate over each column to plot
for (column in columns_to_plot) {
  # Create the combined plot
  p <- ggplot(new_runtable, aes(x = llm, y = .data[[column]], color = llm)) +
    geom_boxplot() +
    labs(title = paste("Box Plot of", column_titles[[column]], "and LLM"),  # Human-readable title
         x = "LLMs",
         y = column_titles[[column]]) +  # Human-readable y-axis label
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_grid(prompt ~ language + problem, scales = "free_y",
               labeller = labeller(
                 prompt = as_labeller(custom_labels$prompt),
                 language = as_labeller(custom_labels$language),
                 problem = as_labeller(custom_labels$problem))) +  # Corrected labeller
    scale_color_manual(values = c("ChatGPT" = "red", "Gemini" = "green", "Claude" = "blue"),
                       labels = c("ChatGPT" = "ChatGPT", "Gemini" = "Gemini", "Claude" = "Claude")) +
    labs(color = "LLMs") +
    theme(legend.position = "bottom")
  
  # Add the plot to the list
  all_plots[[column]] <- p
}

# Combine all plots into a single plot
combined_plot <- plot_grid(plotlist = all_plots, ncol = 1)

# Save the combined plot to a file
ggsave(filename = "combined_boxplots.png", plot = combined_plot, width = 16, height = 48)


#QQ Plot
for (column in columns_to_plot) {
  # Check if the column has non-zero variance
  if (var(new_runtable[[column]], na.rm = TRUE) > 0) {
    # Create the QQ plot with stat_qq_line
    p <- ggplot(new_runtable, aes(sample = .data[[column]], color = llm)) +
      stat_qq() +
      stat_qq_line() +  # Only include this if variance is non-zero
      labs(title = paste("QQ Plot of", column_titles[[column]], "and LLM"),
           x = "Theoretical Quantiles",
           y = paste(column_titles[[column]], "Usage")) +
      facet_grid(prompt ~ language + problem, scales = "free_y",
                 labeller = labeller(
                   prompt = as_labeller(custom_labels$prompt),
                   language = as_labeller(custom_labels$language),
                   problem = as_labeller(custom_labels$problem))) +
      scale_color_manual(values = c("ChatGPT" = "red", "Gemini" = "green", "Claude" = "blue"),
                         labels = c("ChatGPT" = "ChatGPT", "Gemini" = "Gemini", "Claude" = "Claude")) +
      labs(color = "LLMs") +
      theme(legend.position = "bottom")
  } else {
    # Create the QQ plot without stat_qq_line for zero-variance data
    p <- ggplot(new_runtable, aes(sample = .data[[column]], color = llm)) +
      stat_qq() +
      labs(title = paste("QQ Plot of", column_titles[[column]], "and LLM"),
           x = "Theoretical Quantiles",
           y = paste(column_titles[[column]], "Usage")) +
      facet_grid(prompt ~ language + problem, scales = "free_y",
                 labeller = labeller(
                   prompt = as_labeller(custom_labels$prompt),
                   language = as_labeller(custom_labels$language),
                   problem = as_labeller(custom_labels$problem))) +
      scale_color_manual(values = c("ChatGPT" = "red", "Gemini" = "green", "Claude" = "blue"),
                         labels = c("ChatGPT" = "ChatGPT", "Gemini" = "Gemini", "Claude" = "Claude")) +
      labs(color = "LLMs") +
      theme(legend.position = "bottom")
  }
  
  # Add the plot to the list
  all_plots[[column]] <- p
}


# Combine all plots into a single plot
combined_plot <- plot_grid(plotlist = all_plots, ncol = 1)

# Save the combined plot to a file
ggsave(filename = "combined_qqplots.png", plot = combined_plot, width = 16, height = 48)

# Normalization function
normalize <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# Normalize the columns to a range of 0 to 1
normalized_runtable <- new_runtable %>%
  mutate(
    cpu_usage_total = normalize(cpu_usage_total),
    memory_usage = normalize(memory_usage),
    machine_code_size = normalize(machine_code_size)
)

# Calculate energy efficiency
normalized_runtable <- normalized_runtable %>%
  mutate(
    energy_efficiency = energy_usage_package * (0.6 * cpu_usage_total + 0.3 * memory_usage + 0.1 * machine_code_size) / execution_time
)

head(normalized_runtable)

# ANOVA for CPU usage
anova_cpu <- aov(cpu_usage_total ~ llm * language * problem, data = new_runtable)
summary(anova_cpu)

# ANOVA for Memory usage
anova_memory <- aov(memory_usage ~ llm * language * problem, data = new_runtable)
summary(anova_memory)

# ANOVA for Machine code size
anova_machine_code <- aov(machine_code_size ~ llm * language * problem, data = new_runtable)
summary(anova_machine_code)

# CPU usage impact on Energy Usage
anova_cpu <- aov(energy_usage_package ~ cpu_usage_total * llm * language * problem, data = normalized_runtable)
summary(anova_cpu)

# Memory usage impact on Energy Usage
anova_memory <- aov(energy_usage_package ~ memory_usage * llm * language * problem, data = normalized_runtable)
summary(anova_memory)

# Machine code size impact on Energy Usage
anova_machine_code <- aov(energy_usage_package ~ machine_code_size * llm * language * problem, data = normalized_runtable)
summary(anova_machine_code)




# Select the specified columns and sum the CPU usage columns
selected_columns <- run_table %>%
  select('__run_id', llm, language, problem, prompt, energy_usage_package, contains("cpu_usage_"), memory_usage, execution_time, machine_code_size) %>%
  mutate(cpu_usage_total = rowSums(select(., starts_with("cpu_usage_")), na.rm = TRUE))

# Create a new dataframe with the selected columns and the summed CPU usage
new_runtable <- selected_columns %>%
  select('__run_id', llm, language, problem, prompt, energy_usage_package, cpu_usage_total, memory_usage, execution_time, machine_code_size) %>%
  mutate(
    across(c(energy_usage_package, cpu_usage_total, memory_usage, execution_time, machine_code_size), as.numeric)
  )
sorted_new_runtable <- new_runtable %>%
  arrange(energy_usage_package)

# Assuming new_runtable is your data frame

# List of dependent variables
dependent_vars <- c("energy_usage_package", "cpu_usage_total", "memory_usage", "execution_time", "machine_code_size")

# Apply the Shapiro-Wilk test for each variable
shapiro_results <- lapply(dependent_vars, function(var) {
  cat("Shapiro-Wilk test for", var, ":\n")
  test_result <- shapiro.test(new_runtable[[var]])
  print(test_result)
  cat("\n")
})

# Alternatively, store results in a list if you want to access them later
shapiro_results <- lapply(dependent_vars, function(var) shapiro.test(new_runtable[[var]]))

# If you want to access specific test results, for example:
# shapiro_results[[1]] for energy_usage_package
