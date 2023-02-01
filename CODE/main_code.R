library(tidyverse)
library(janitor)
library(lubridate)
library(RJSONIO)
library(jsonlite)
library(dplyr)
library(ggplot2)


raw_data <- read.csv("all_year_glsea_avg_o_C.csv")

ggplot(data = raw_data, aes(x = X, y = `X2022`)) +
  geom_point()

avg_data <- raw_data %>% 
  select(!X) %>% 
  select(!`X2023`)
avg_data$row_mean <- rowMeans(avg_data, na.rm=TRUE)

avg_data <- avg_data %>% 
  mutate(row_id=row_number())%>% 
  mutate(diff = `X2022` - row_mean)

extracted_col <- extracted_col %>% 
  mutate(row_id = row_number())

total <- merge(avg_data,extracted_col,by="row_id")

#1995 - 2022 Average Lake Ontario Temperature
ggplot(data = avg_data, aes(x = row_id, y = row_mean)) +
  geom_line()

ggplot(total, aes(x=row_id)) + 
  geom_line(aes(y = row_mean), color = "yellow", size = 1.5) + 
  geom_line(aes(y = `X2023`), color= "red", size = 1.5) +
  labs(title = "Lake Ontario Water Temperature Average 1995-2022",
       subtitle = "Created by Noah Cornish",
       caption = "Data source: NOAA CoastWatch Great Lakes", colors = "Legend") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Water Temperature") +
  xlab("Day of Year") +
  theme(panel.background = element_rect(fill = 'black'),
        panel.grid.major = element_line(color = 'blue'),
        panel.grid.minor = element_line(color = 'black', size = 2)) +
  annotate("text", x=50, y=12, label= "Yellow - 1995-2022 Average", color= "yellow") +
  annotate("text", x=50, y=13, label= "Red - 2023 Temperature", color= "yellow")



#z-score calculation
#z_scores <- (avg_data-mean(avg_data$X2022))/sd(avg_data$X2022)
#z_scores






