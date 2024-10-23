# # Install the readr package if it is not already installed
#install.packages("readr")
#install.packages("dplyr")


# Load necessary library
library(readr)
library(dplyr) # Load necessary library for data manipulation

# Read the CSV file
run_table <- read_csv("run_table.csv")

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