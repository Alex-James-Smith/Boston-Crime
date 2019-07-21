library(readr)
library(tidyverse)
library(magrittr)

Boston_Crime <- read_csv("crime.csv", col_types = cols(OCCURRED_ON_DATE = col_datetime(format = "%Y-%m-%d %H:%M:%S"), SHOOTING = col_character()))

B_Crime <- Boston_Crime %>% select(-c(INCIDENT_NUMBER, OFFENSE_CODE, DISTRICT, REPORTING_AREA, Location))
B_Crime %<>% mutate(SHOOTING = if_else(SHOOTING == "Y", 1, 0, missing = 0))

B_Crime %>% group_by(MONTH) %>% summarise(PCT = mean(SHOOTING)) %>% ggplot(aes(MONTH, PCT)) + 
  geom_line() + ylim(0, 0.005) + xlim(0.8, 12.2)

B_Crime %>% group_by(DAY_OF_WEEK) %>% summarise(PCT = mean(SHOOTING)) %>% ggplot(aes(DAY_OF_WEEK, PCT)) + 
  geom_col() + ylim(0, 0.0055) + 
  scale_x_discrete(limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

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
