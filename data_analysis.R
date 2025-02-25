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

# Date: Feb 12, 2025

# Filter: subset data by rows based in same value
sample_data %>%
  # subset samples only from the deep
  filter(temperature < 5) %>%
  # We will get one summary value for each group
  #filter(env_group) %>%
  # We will get the summary value for "Deep"
  # Why we use two equal signs? We will subset based on a logical, TURE == 1
  #filter(env_group == "Deep") %>%
  # calculate the mean cell abundance
  summarize(average_cells = mean(cells_per_ml), min=min(cells_per_ml))

# Mutate: create a new column
sample_data %>%
  # calculate new column with the TN:TP ratio
  mutate(tn_tp_ratio = total_nitrogen / total_phosphorus)
  # add two new columns with the TN:TP ratio and cells in millions
  #mutate(tn_tp_ratio = total_nitrogen / total_phosphorus, 
       #cellsInMillions = cells_per_ml / 1000000) 
  # visualize it
  View(sample_data)

# select():subset by entire columns
  sample_data %>%
    # pick specific columns
    #select(sample_id, depth)
  # pick all of the columns between sample_id and temperature
    #select(sample_id:temperature)
  # remove a column
    #select(-env_group)
  # remove a range of columns
    select(-(total_nitrogen:chlorophyll))

# clean up data
# using the skip by giving it a number of lines to skip
taxon_dirty <- read_csv("taxon_abundance.csv", skip = 2)
head(taxon_dirty, 6)

# Only pick to the Cyanobacteria
taxon_clean <- taxon_dirty %>%
  select(sample_id:Cyanobacteria)
# What are the wide format dimension? 71 rows by 7 columns
dim(taxon_clean)

# The functions 'pivot_wider' and 'pivot_longer' that make it easy to switch between the two formats.
# Note: If you have multiple columns which all share the same units and scale, 
# or add up to a larger whole, your data might be wide.

# Pivot_Longer: shape the data from wide into long format
taxon_long <- 
  taxon_clean %>%
  pivot_longer(cols = Proteobacteria:Cyanobacteria, 
               names_to = "Phylum",
               values_to = "Abundance")
# If we want to return our data to wide format, we can use pivot_wider
#taxon_long %>%
  #pivot_wider(names_from = "Phylum", values_from = "Abundance")
# check the new dimensions: 426 rows by 3 columns
dim(taxon_long)

# calculate avg abundance of each phylum
taxon_long %>%
  group_by(Phylum) %>%
  summarize(avg_abund = mean(Abundance))

# Plot our data
taxon_long %>%
  ggplot(aes(x = sample_id, y = Abundance, fill = Phylum)) + 
  geom_col() + 
  theme(axis.text.x = element_text(angle = 90))

# Joining data frames: merge the taxon data to the sample data
head(sample_data, 6)
taxon_clean %>%
  head(6)
# Commonly using inner join; joining by sample_id
sample_data %>%
  inner_join(., taxon_clean, by = "sample_id")
#Or: inner_join(sample_data, taxon_clean)

# Intuition check on filtering joins
length(unique(taxon_clean$sample_id))
length(unique(sample_data$sample_id))

# Anti-join: which rows are not joining? this will show us the data for the ... 
# keys on the left that are missing from the data frame on the right.
sample_data %>%
  anti_join(., taxon_clean, by="sample_id")
#Or: anti_join(sample_data, taxon_clean, by="sample_id")

# Fix September samples (In our sample_data, our sample IDs used the full word “September”, 
# but in our taxon table it looks like someone shortened it to “Sep”) 
taxon_clean_goodSept <-
  taxon_clean %>%
  # Replace sample_id column with fixed September names
  mutate(sample_id = str_replace(sample_id, 
                                 patter = "Sep", replacement = "Septmeber"))
# Save this as a new dataframe
sample_and_taxon <- sample_data %>%
  inner_join(., taxon_clean_goodSep, by = "sample_id")

# Intuition check
dim(sample_and_taxon)

# Test
stopifnot(nrow(sample_and_taxon) == nrow(sample_data))

# Write out our clean data into a new file
write_csv(sample_and_taxon, "sample_and_taxon.csv")

# Quick plot of Chloroflexi
sample_and_taxon %>%
  ggplot(aes(x=depth, y=Chloroflexi)) +
  geom_point() + 
# Add a statistical model
geom_smooth()
#Or: force the line to be a linear model (straight) using method="lm"
#geom_smooth(method="lm")


# HW_4_Q4
taxon_clean_goodSept_long <- 
  taxon_clean_goodSept %>%
  pivot_longer(cols = Proteobacteria:Cyanobacteria, 
               names_to = "Phylum", 
               values_to = "Abundance")
View(taxon_clean_goodSept_long)
View(taxon_clean_goodSept)

# HW_4_Q5
sample_taxon_long <- 
  taxon_clean_goodSept %>%
  inner_join(., taxon_clean_goodSept_long, by = "sample_id")

# HW_4_Q6
selected_data <-
  taxon_clean_goodSept_long %>%
  filter(Phylum %in% c("Chloroflexi", "Cyanobacteria", "Bacteroidota"))
View(selected_data)
# Plotting
plot <- ggplot(data = inner_join(sample_data, selected_data, by = "sample_id"), 
               aes(x = env_group, y = Abundance, 
                   fill = env_group, color = env_group)) +
  geom_boxplot(alpha = 0.4) +
  geom_jitter() +
  facet_grid(~Phylum) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none") + 
  labs(x = "Depth and Season", y = "Phylum Abundance", 
       title = "Selected Phyla Abundance Changes by Environmental Group")
  
ggsave("Phylum_plot.png", plot = plot, width=6, height=4)

# HW_4_Q10
install.packages("ggpubr")
library(ggpubr) 
find.package("ggpubr")

selected_data <-
  taxon_clean_goodSept_long %>%
  filter(Phylum %in% c("Chloroflexi", "Cyanobacteria", "Bacteroidota"))

plot2 <- ggplot(data = inner_join(sample_data, selected_data, by = "sample_id"), 
               aes(x = env_group, y = Abundance, 
                   fill = env_group, color = env_group)) +
  geom_boxplot(alpha = 0.4) +
  geom_jitter() +
  facet_grid(~Phylum) +
  theme_minimal() +
  # Add Wilcoxon test results
  stat_compare_means(method = "wilcox.test") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none") + 
  labs(x = "Depth and Season", y = "Phylum Abundance", 
       title = "Selected Phyla Abundance Changes by Environmental Group")

ggsave("Phylum_plot2.png", plot = plot2, width=6, height=4)


