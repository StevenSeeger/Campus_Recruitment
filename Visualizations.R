#### Load in libraries #####
library(readr)
library(tidyverse)

#### Read in/Validate Data ####
placement_data <- read_csv("data/Placement_Data_Full_Class.csv")

sum(is.na(placement_data)) # There are 67 NA's in the data
sum(is.na(placement_data$salary)) # They are all in salary - it makes sense

#### EDA/Viz ####
## Extra challenge: use tidyverse

## Look at basic counts and visualize them

# Count of Male vs Females
placement_data %>% 
  group_by(gender) %>%
  summarise(n())
# More males than females in the data
ggplot(placement_data, aes(gender, color=gender, fill=gender)) + 
  geom_bar() + 
  geom_text(stat='count', aes(label=..count..), vjust=-.5) +
  ggtitle("Number of Each \nGender in the Data Set") +
  xlab('Gender')+
  ylab('Count')

# Gender Undergrad Degree
placement_data %>% 
  group_by(gender, degree_t) %>%
  summarise(n())
# More males than females in the data
ggplot(placement_data, aes(gender, color=gender, fill=gender)) + 
  geom_bar() + 
  facet_grid(~degree_t) +
  geom_text(stat='count', aes(label=..count..), vjust=-.5) +
  ggtitle("Degree by Gender") +
  xlab('Gender')+
  ylab('Count')

# Overall Salary Difference
placement_data %>%
  group_by(gender) %>%
  summarize(mean(salary, na.rm=TRUE))
# The salaries are differed by around 3k
# Will it maintain when split out?
placement_data %>%
  group_by(gender, degree_t) %>%
  summarize(mean(salary, na.rm=TRUE), n())
# Seeing large disparity between Comm&Management and Sci&Tech
ggplot(placement_data, aes(x=salary, color=gender, fill=gender)) + 
  geom_density(alpha=.2, fill="#FF6666") + 
  facet_grid(~degree_t) + 
  scale_x_continuous(labels = scales::comma) + 
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Too few observations for `Others`, but obvious pay disparity in Sci&Tech
       