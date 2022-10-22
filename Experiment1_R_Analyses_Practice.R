# Install and load Packages.

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
library(tidyverse)
library(ggplot2)
library(dplyr)

# Assign the data to an object. We'll name our object "Experiment_1_Data".

Experiment_1_Data <- read.csv("Experiment1_10.11.22.csv")

# In order to view the data we're working with, use the function "View()".

View(Experiment_1_Data)

# Assign the DQ.Attention.Check variable to an object.
# We'll name our object "DQ_Hair_Color".

DQ_Hair_Color <- Experiment_1_Data %>%
  select(DQ.Attention.Check)

# In order to view the new object, use the function "View()".

View(DQ_Hair_Color)

# Filter the data and call it "Experiment_1_Data_Filtered".

Experiment_1_Data_Filtered <- Experiment_1_Data %>%
  filter(DQ_Hair_Color == 5)

# In order to view our new FILTERED dataset, use the function "View()".

View(Experiment_1_Data_Filtered)

attach(Experiment_1_Data_Filtered)

Tatt_in_Any <- Experiment_1_Data_Filtered %>%
  select(Tatt.in.Any)

Lineup_Condition <- Experiment_1_Data_Filtered %>%
  select(Lineup.Condition)

# In order to make a crosstabs table, use the function...
# ... table name <-  table(A,B)
# A is rows, B is columns.

Tatt_in_Any_by_Lineup_Condition <- table(Experiment_1_Data_Filtered$Tatt.in.Any, Experiment_1_Data_Filtered$Lineup.Condition)

Tatt_in_Any_by_Lineup_Condition

ftable(Tatt_in_Any_by_Lineup_Condition)

barplot(Tatt_in_Any_by_Lineup_Condition, legend = TRUE, beside = TRUE, main = "Tatt in Any by Lineup Condition")

# No Tattoo Condition + No Tatt.in.Any = 156
# No Tattoo Condition + Yes Tatt.in.Any = 13
# Tattoo Condition + No Tatt.in.Any = 96
# Tattoo Condition + Yes Tatt.in.Any = 66

# Run crosstabs with Tatt.in.Any by Describe.Before by Feedback.
Tatt_in_Any_by_Describe_Before_by_Feedback <- table(Experiment_1_Data_Filtered$Tatt.in.Any, Experiment_1_Data_Filtered$Describe.Before, Experiment_1_Data_Filtered$Feedback)

Tatt_in_Any_by_Describe_Before_by_Feedback

ftable(Tatt_in_Any_by_Describe_Before_by_Feedback)

data.frame(Tatt_in_Any_by_Lineup_Condition)


