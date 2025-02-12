# Data Analysis
# Date: Feb 5, 2025

# Load package
library(tidyverse)
# Grab the data for our analysis
sample_data <- read.csv("sample_data.csv")
glimpse(sample_data)

# Summarize 
summarize(sample_data, avg_cells = mean(cells_per_ml))

# Syntax/style
# Use the pipe operator %>% to pass the data value into the summarize function. 
# It take the output from the left side and use it as input to the right side.
# Note: The pipe (%>%) is a bit different from using the ggplot plus (+). 
# (+) layer on additional information (right side) to a preexisting plot (left side).
sample_data %>%
  #summarize(avg_cells = mean(cells_per_ml))
  # group the data by environmental group
  group_by(env_group) %>%
  # calculate the mean
  summarize(avg_cells = mean(cells_per_ml))
  
# Save the output into a new dataframe: "sample_data_summarized"
sample_data_summarized <- sample_data %>%
  summarize(average_cells = mean(cells_per_ml))
# Or: change sample_data itself
#sample_data <- sample_data %>%
  #summarize(average_cells=mean(cells_per_ml))

# Filter: subset data by rows based in same value
sample_data %>%
  # subset samples only from the deep
  filter(temperature < 5) %>%
  # Why two equal signs? We will subset based on a logical, TURE == 1
  #filter(env_group == "Deep") %>%
  # calculate the mean cell abundance
  summarize(average_cells = mean(cells_per_ml))

# Mutate: create a new column
sample_data %>%
  # calculate new column with the TN:TP ratio
  mutate(tn_tp_ratio = total_nitrogen / total_phosphorus)
  # visualize it 
  View()

# select():subset by entire coloumns
  sample_data %>%
    # pick specific columns
    #select(sample_id, depth)
  # pick all of the columns between sample_id and temerature
    #select(sample_id:temperature)
  # remove a column
    select(-(total_nitrogen:chlorophyll))

  # clean uo data
  
  
  
         