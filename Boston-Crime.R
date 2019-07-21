library(readr)
library(tidyverse)
library(magrittr)

Boston_Crime <- read_csv("crime.csv", col_types = cols(OCCURRED_ON_DATE = col_datetime(format = "%Y-%m-%d %H:%M:%S"), SHOOTING = col_character()))

## Remove unuseful columns
B_Crime <- Boston_Crime %>% select(-c(INCIDENT_NUMBER, OFFENSE_CODE, DISTRICT, REPORTING_AREA, Location))

## If Shooting is TRUE = 1, if FALSE = 0
B_Crime %<>% mutate(SHOOTING = if_else(SHOOTING == "Y", 1, 0, missing = 0))

## Rate of shootings by Month - Incomplete
B_Crime %>% group_by(MONTH) %>% summarise(PCT = mean(SHOOTING)) %>% ggplot(aes(MONTH, PCT)) + 
  geom_line() + ylim(0, 0.005) + xlim(0.8, 12.2)

## Rate of shootings by Weekday
B_Crime %>% group_by(DAY_OF_WEEK) %>% summarise(PCT = mean(SHOOTING)) %>% ggplot(aes(DAY_OF_WEEK, PCT)) + 
  geom_col() + ylim(0, 0.0055) + 
  scale_x_discrete(limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

## Percent of Crimes by Hour of Day
B_Crime %>% group_by(HOUR) %>% summarise(PCT = mean(SHOOTING)) %>% ggplot(aes(HOUR, PCT*100, fill = PCT*100)) + 
  geom_col() + labs(title = "Crimes involving shooting in Boston", 
                    x = "Hour of Day", y = "Percent of crimes involving shooting", fill = NULL) +
  ylim(0, 0.9) + 
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12, 15, 18, 21)) +
  scale_fill_viridis_c(option = "inferno") + 
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey60"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.ticks = element_blank())

## Number of Crimes by Offense Code Group
Boston_Crime %>% group_by(OFFENSE_CODE_GROUP) %>% summarise(n = n()) %>% arrange(desc(n))

## Simply and Tidy Motor Vehicle Accident Response (MVAR)
MVAR <- Boston_Crime %>% 
  filter(OFFENSE_CODE_GROUP == "Motor Vehicle Accident Response") %>% 
  group_by(OFFENSE_DESCRIPTION) %>%
  summarise(n = n()) %>% arrange(desc(n)) %>% 
  mutate(OFFENSE_DESCRIPTION = str_remove_all(OFFENSE_DESCRIPTION, "\"|�")) %>%
  mutate(MAJOR_GROUP = str_extract(OFFENSE_DESCRIPTION, "M/V ACCIDENT|M/V - LEAVING SCENE"),
         MINOR_GROUP = str_remove(OFFENSE_DESCRIPTION, "M/V ACCIDENT - |M/V - LEAVING SCENE - ")) %>%
  mutate(MAJOR_GROUP = str_remove(MAJOR_GROUP, "^M/V - |^M/V "), 
         MINOR_GROUP = str_remove(MINOR_GROUP, "M/V ACCIDENT ")) %>%
  mutate(INJURY = str_extract(MINOR_GROUP, " - INJURY| - NO INJURY"),
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
  ggplot(aes(fct_rev(MAJOR_GROUP), scales::percent(n/sum(n)), label = n)) +
  geom_col() + 
  geom_text(nudge_y = -0.1, colour = "white") + 
  coord_flip() + 
  labs(x = NULL, y = NULL)

## Minor Group of MVAR
MVAR %>% group_by(MINOR_GROUP) %>% summarise(n = sum(n))  %>% 
  ggplot(aes(fct_reorder(MINOR_GROUP, n), n/sum(n), label = n)) +
  geom_col() + 
  geom_text(nudge_y = -0.007, colour = "white", angle = 270) + 
  coord_flip() + 
  labs(x = NULL, y = NULL)


## Exploration
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


