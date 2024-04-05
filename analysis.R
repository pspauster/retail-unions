library(tidyverse)
library(janitor)
library(lubridate)

recent_filings_all_dates<- read_csv("recent_filings__all_dates_--20240404225917.csv")

recent_filings_clean <- recent_filings_all_dates %>% 
  clean_names() %>% 
  mutate(date_filed = mdy(date_filed),
         month_filed = floor_date(date_filed, "month"),
         week_filed = floor_date(date_filed, "week"),
         after_press_release = if_else(date_filed > as.Date("2024-02-27"), T, F), 
         starbucks = str_detect(name, regex("starbucks", ignore_case = T)))

starbucks_filings <- filter(recent_filings_clean, starbucks == T)

month_sum <- starbucks_filings %>% 
  group_by(month_filed) %>% 
  summarize(filings = n())

month_stacked_sum <- starbucks_filings %>% 
  group_by(month_filed, after_press_release) %>% 
  summarize(filings = n())

month_stacked_sum %>% pivot_wider(names_from = after_press_release, values_from = filings) %>% write_csv("starbucks_stacked_bar.csv")

ggplot(month_stacked_sum) +
  geom_col(mapping = aes(x = month_filed, y = filings, fill = after_press_release)) +
  scale_color_manual(values = c("white", "black")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = "Month",
       y = "Filings") +  
  urbnthemes::remove_ticks() +
  urbnthemes::remove_axis() +
  guides(color = "none")

week_sum <- starbucks_filings %>% 
  group_by(week_filed) %>% 
  summarize(filings = n())
