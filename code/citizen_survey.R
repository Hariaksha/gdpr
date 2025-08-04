library(ggplot2)
library(MASS)
library(dplyr)
library(car)
library(tidyr)
library(ordinal)

# transfer data from spreadsheet to variable
citizen_survey <- read.csv("C:/Users/haria/Downloads/gdpr_survey_citizens.csv")

# inspect structure of data set
str(citizen_survey)
summary(citizen_survey)

data_table = table(citizen_survey$Nutzung_Stationaerer_Computer)
data_table

response_summary <- citizen_survey %>%
  select(Nutzung_Laptop_Notebook_Netbook, Nutzung_Stationaerer_Computer, Nutzung_Smartphone, Nutzung_Tablet, Nutzung_Mobile_Spielkonsole, Nutzung_TV) %>%
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Response") %>%
  group_by(Question, Response) %>%
  summarise(Count = n(), .groups = "drop") 

ggplot(response_summary, aes(x = Question, y=Count, fill = factor(Response))) +
  geom_col(position = "dodge") +
  labs(x = "Device Type", y = "Count", fill = "Response", title = "Response Counts by Device") +
  theme_minimal()
