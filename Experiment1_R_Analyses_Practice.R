R.Version()
# Install and load Packages

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("gmodels")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(gmodels)

# Assign the data to an object. We'll name our object "Experiment_1_Data"

Experiment_1_Data <- read.csv("Experiment1_10.11.22.csv")

# In order to view the data we're working with, use the function "View()"

View(Experiment_1_Data)

# Assign the DQ.Attention.Check variable to an object
# We'll name our object "DQ_Hair_Color"

DQ_Hair_Color <- Experiment_1_Data %>%
  select(DQ.Attention.Check)

# In order to view the new object, use the function "View()"

View(DQ_Hair_Color)

# Filter the data and call it "Experiment_1_Data_Filtered"

Experiment_1_Data_Filtered <- Experiment_1_Data %>%
  filter(DQ_Hair_Color == 5)

# In order to view our new FILTERED dataset, use the function "View()"

View(Experiment_1_Data_Filtered)

# Assign the Tatt.in.Any variable to an object-- we'll call it "Tatt_in_Any"

Tatt_in_Any <- Experiment_1_Data_Filtered %>%
  select(Tatt.in.Any)

# Assign the Lineup.Condition variable to an object-- we'll call it "Lineup_Condition"

Lineup_Condition <- Experiment_1_Data_Filtered %>%
  select(Lineup.Condition)

# In order to view these datasets, use the function "View()"

View(Tatt_in_Any)
View(Lineup_Condition)

# In order to make a crosstabs table, use the function...
# ... table name <-  table(A,B)
# A is rows, B is columns

Tatt_in_Any_by_Lineup_Condition <- table(Experiment_1_Data_Filtered$Tatt.in.Any, 
                                         Experiment_1_Data_Filtered$Lineup.Condition)


Tatt_in_Any_by_Lineup_Condition

barplot(Tatt_in_Any_by_Lineup_Condition, legend = TRUE, beside = TRUE, main = "Tatt in Any by Lineup Condition")

# No Tattoo Condition + No Tatt.in.Any = 156
# No Tattoo Condition + Yes Tatt.in.Any = 13
# Tattoo Condition + No Tatt.in.Any = 96
# Tattoo Condition + Yes Tatt.in.Any = 66

# Run crosstabs with Tatt.in.Any by Describe.Before by Feedback.
Tatt_in_Any_by_Describe_Before_by_Feedback <- table(Experiment_1_Data_Filtered$Tatt.in.Any, Experiment_1_Data_Filtered$Describe.Before, Experiment_1_Data_Filtered$Feedback)

Tatt_in_Any_by_Describe_Before_by_Feedback

ftable(Tatt_in_Any_by_Describe_Before_by_Feedback)

data.frame(Tatt_in_Any_by_Describe_Before_by_Feedback)





## CrossTables
require(gmodels)

CrossTable(Experiment_1_Data_Filtered$Tatt.in.Any, 
           Experiment_1_Data_Filtered$Lineup.Condition, digits = 3,
           expected = TRUE, prop.r = TRUE, 
           prop.c = TRUE, prop.t = TRUE, 
           prop.chisq = TRUE, chisq = FALSE,
           fisher = FALSE, mcnemar = FALSE, 
           missing.include = FALSE)

### digits: # of decimals
### expected: shows expected frequencies
### prop.r: row proportions
### prop.c: column proportions
### prop.chisq: chi square contributions
### chisq: computes chi square test for association
### fisher: Fisher exact test
## mcnemar: computers the McNemar test (2x2 tables only)
## missing.include: if TRUE removes unused factor levels

## to show only observed counts, expected = FALSE, change prop.r = FALSE, prop.c = FALSE, prop.t = FALSE,
## prop.chisq = FALSE, chisq = FALSE, fisher = FALSE, mcnemar = FALSE, missing.include = FALSE

CrossTable(Experiment_1_Data_Filtered$Tatt.in.Any, Experiment_1_Data_Filtered$Lineup.Condition, digits = 3,
           expected = FALSE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE, chisq = FALSE,
           fisher = FALSE, mcnemar = FALSE, missing.include = FALSE)

# Using xtabs

Tatt_in_Any_by_Lineup_Condition <- xtabs(~ Tatt.in.Any + Lineup.Condition, data = Experiment_1_Data_Filtered)
xtabs(~ Tatt.in.Any + Describe.Before + Feedback, data = Experiment_1_Data_Filtered)


Tatt_in_Any_by_Lineup_Condition_Proportions <- prop.table(Tatt_in_Any_by_Lineup_Condition)
Tatt_in_Any_by_Lineup_Condition_Proportions
