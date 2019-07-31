## Libraries
library(readr)
library(tidyverse)
library(magrittr)
library(lubridate)

## Import
Boston_Crime <- read_csv("crime.csv", col_types = cols(OCCURRED_ON_DATE = col_datetime(format = "%Y-%m-%d %H:%M:%S"), SHOOTING = col_character()))

## Overview
## Number of Crimes by Offense Code Group
Boston_Crime %>% group_by(OFFENSE_CODE_GROUP) %>% summarise(n = n()) %>% arrange(desc(n))

#### Simplify and Tidy Motor Vehicle Accident Response (MVAR) Overview
MVAR <- Boston_Crime %>% 
  filter(OFFENSE_CODE_GROUP == "Motor Vehicle Accident Response") %>% 
  group_by(OFFENSE_DESCRIPTION) %>%
  summarise(n = n()) %>% arrange(desc(n)) %>% 
  mutate(OFFENSE_DESCRIPTION = str_remove_all(OFFENSE_DESCRIPTION, "\"|�"),
         MAJOR_GROUP = str_extract(OFFENSE_DESCRIPTION, "M/V ACCIDENT|M/V - LEAVING SCENE"),
         MINOR_GROUP = str_remove(OFFENSE_DESCRIPTION, "M/V ACCIDENT - |M/V - LEAVING SCENE - "),
         MAJOR_GROUP = str_remove(MAJOR_GROUP, "^M/V - |^M/V "), 
         MINOR_GROUP = str_remove(MINOR_GROUP, "M/V ACCIDENT "),
         INJURY = str_extract(MINOR_GROUP, " - INJURY| - NO INJURY"),
         MINOR_GROUP = str_remove(MINOR_GROUP, " - INJURY| - NO INJURY"),
         INJURY = str_remove(INJURY, " - "),
         INJURY = case_when(
           INJURY == "NO INJURY" ~ FALSE,
           INJURY == "INJURY" | MINOR_GROUP == "PERSONAL INJURY" ~ TRUE,
           is.na(INJURY) ~ FALSE
         )) %>%
  select(MAJOR_GROUP, MINOR_GROUP, INJURY, n)

## MVAR - Accident or Leaving Scene?
MVAR %>% group_by(MAJOR_GROUP) %>% summarise(n = sum(n)) %>% 
  mutate(MAJOR_GROUP = str_to_title(MAJOR_GROUP)) %>%
  ggplot(aes(fct_rev(MAJOR_GROUP), scales::percent(n/sum(n)), label = n)) +
  geom_col() + 
  geom_text(nudge_y = -0.1, colour = "white") + 
  coord_flip() + 
  labs(x = NULL, y = NULL)

## Minor Group of MVAR - regardless of whether accident or leaving scene
MVAR %>% group_by(MINOR_GROUP) %>% summarise(n = sum(n))  %>% 
  mutate(MINOR_GROUP = str_to_title(MINOR_GROUP)) %>%
  ggplot(aes(fct_reorder(MINOR_GROUP, n), n/sum(n), label = n)) +
  geom_col() + 
  geom_text(nudge_y = -0.007, colour = "white", angle = 270) + 
  coord_flip() + 
  labs(x = NULL, y = NULL)


MVAR %>% group_by(INJURY) %>% summarise(n = sum(n)) %>%
  ggplot(aes(INJURY, scales::percent(n/sum(n)))) +
  geom_col() + 
  labs(y = NULL)

## Number of MVAR incidents by weekday and offense type
Boston_Crime %>% filter(OFFENSE_CODE_GROUP == "Motor Vehicle Accident Response") %>% 
  select(OFFENSE_DESCRIPTION, SHOOTING, OCCURRED_ON_DATE, YEAR, MONTH, DAY_OF_WEEK, HOUR) %>% 
  mutate(OFFENSE_DESCRIPTION = 
           str_to_title(OFFENSE_DESCRIPTION)) %>%
  group_by(OFFENSE_DESCRIPTION, DAY_OF_WEEK) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ggplot(aes(DAY_OF_WEEK, n, colour = OFFENSE_DESCRIPTION)) +
  geom_point() + 
  labs(x = NULL, y = "Number of Incidents", colour = "Offense Description") +
  scale_x_discrete(limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) +
  theme(panel.grid.major.x = element_blank(),
        axis.ticks.x = element_blank())

## Number of MVAR incidents by Hour of Day and offense type
Boston_Crime %>% filter(OFFENSE_CODE_GROUP == "Motor Vehicle Accident Response") %>% 
  select(OFFENSE_DESCRIPTION, SHOOTING, OCCURRED_ON_DATE, YEAR, MONTH, DAY_OF_WEEK, HOUR) %>% 
  mutate(OFFENSE_DESCRIPTION = 
           str_to_title(OFFENSE_DESCRIPTION)) %>%
  group_by(OFFENSE_DESCRIPTION, HOUR) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ggplot(aes(HOUR, n, colour = OFFENSE_DESCRIPTION)) +
  geom_point() + 
  labs(x = "Hour of Day", y = "Number of Incidents", colour = "Offense Description") +
  scale_x_discrete(limits = c(0, 3, 6, 9, 12, 15, 18, 21)) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major.x = element_line(colour = "grey80"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = "black"))

## Have MVAR Incidents increased or decreased year on year (2015 - 2018) by type - Flawed 
Boston_Crime %>% filter(OFFENSE_CODE_GROUP == "Motor Vehicle Accident Response") %>% 
  select(OFFENSE_DESCRIPTION, SHOOTING, OCCURRED_ON_DATE, YEAR, MONTH, DAY_OF_WEEK, HOUR) %>% 
  mutate(OFFENSE_DESCRIPTION = 
           str_to_title(OFFENSE_DESCRIPTION)) %>%
  group_by(OFFENSE_DESCRIPTION, YEAR) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ggplot(aes(YEAR, n, colour = OFFENSE_DESCRIPTION)) +
  geom_line() + 
  geom_point(size = 1) + 
  labs(x = "Hour of Day", y = "Number of Incidents", colour = "Offense Description")

##
Boston_Crime %>% filter(OFFENSE_CODE_GROUP == "Motor Vehicle Accident Response") %>% 
  select(OFFENSE_DESCRIPTION, SHOOTING, OCCURRED_ON_DATE, YEAR, MONTH, DAY_OF_WEEK, HOUR) %>% 
  mutate(OFFENSE_DESCRIPTION = 
           str_to_title(OFFENSE_DESCRIPTION)) %>%
  mutate(MONTH = month(OCCURRED_ON_DATE, label = TRUE)) %>%
  group_by(OFFENSE_DESCRIPTION, MONTH) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ggplot(aes(MONTH, n, colour = OFFENSE_DESCRIPTION)) +
  geom_point()

#### Shooting incidents

##  If Incident involves shooting is TRUE = 1, if FALSE = 0
Shooting <- Boston_Crime %>% mutate(SHOOTING = if_else(SHOOTING == "Y", 1, 0, missing = 0))

## Shooting rate by weekday
Shooting %>% 
  group_by(DAY_OF_WEEK) %>% summarise(PCT = mean(SHOOTING)) %>% 
  ggplot(aes(DAY_OF_WEEK, PCT)) + 
  geom_col() + 
  ylim(0, 0.0055) + 
  scale_x_discrete(limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

## Shooting rate by hour of day
Shooting %>% 
  group_by(HOUR) %>% summarise(PCT = mean(SHOOTING)) %>% 
  ggplot(aes(HOUR, PCT*100, fill = PCT*100)) + 
  geom_col() + 
  labs(title = "Crimes involving shooting in Boston",
       x = "Hour of Day", 
       y = "Percent of crimes involving shooting", 
       fill = NULL) +
  ylim(0, 0.9) + 
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12, 15, 18, 21)) +
  scale_fill_viridis_c(option = "inferno") + 
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey60"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.ticks = element_blank())

## Shooting rate by Month
Shooting %>% mutate(MONTH = month(OCCURRED_ON_DATE, label = TRUE)) %>%
  group_by(MONTH) %>%
  summarise(PCT = mean(SHOOTING)) %>% 
  ggplot(aes(MONTH, PCT, fill = PCT)) + 
  geom_col()  +
  scale_fill_viridis_c(option = "inferno") + 
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey60"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.ticks = element_blank())


#### Further Overview
# Drugs <- 
Boston_Crime %>% filter(OFFENSE_CODE_GROUP == "Drug Violation") %>% 
  group_by(OFFENSE_DESCRIPTION) %>%
  summarise(n = n()) %>% arrange(desc(n))

Boston_Crime %>% filter(OFFENSE_CODE_GROUP == "Larceny") %>% 
  group_by(OFFENSE_DESCRIPTION) %>%
  summarise(n = n()) %>% arrange(desc(n))

Boston_Crime %>% filter(OFFENSE_CODE_GROUP == "Medical Assistance") %>% 
  group_by(OFFENSE_DESCRIPTION) %>%
  summarise(n = n()) %>% arrange(desc(n))

Boston_Crime %>% filter(OFFENSE_CODE_GROUP == "Other") %>% 
  group_by(OFFENSE_DESCRIPTION) %>%
  summarise(n = n()) %>% arrange(desc(n))
