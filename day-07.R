#nick beegle
#3/5/25
#daily excersise 7

library(tidyverse)

# Load COVID-19 data
covid_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
covid_data <- read.csv(covid_url)

# Identify the 6 states with the most cases on the most recent date
top_states <- covid_data %>%
  filter(date == max(date)) %>%
  arrange(desc(cases)) %>%      
  slice_head(n = 6) %>%         
  pull(state)                   

# Filter data to include only those 6 states
filtered_data <- covid_data %>%
  filter(state %in% top_states)

# Create a faceted line plot
p1 <- ggplot(filtered_data, aes(x = date, y = cases, color = state, group = state)) +
  geom_line(size = 1.2) +  # Line plot
  facet_wrap(~ state, scales = "free_y") +  # Facet by state
  labs(
    title = "COVID-19 Cases in the 6 Most Affected States",
    x = "Date",
    y = "Total Cases",
    color = "State"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold")  # State names under each plot
  )

# Save the plot
ggsave("img/top_6_states_cases.png", p1, width = 10, height = 6)


# Summarize daily total cases for the whole USA
daily_cases <- covid_data %>%
  group_by(date) %>%
  summarise(total_cases = sum(cases)) %>%
  filter(lubridate::month(date) %in% 5:9)  # Filter for May (5) to September (9)

# Create a line graph for total cumulative cases (May to September)
p2 <- ggplot(daily_cases, aes(x = date, y = total_cases)) +
  geom_line(color = "blue", size = 1.2) +  # Smooth line
  labs(
    title = "Total COVID-19 Cases in the USA (May - September)",
    x = "Date",
    y = "Cumulative Cases"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Save the plot
ggsave("img/total_cases_usa_may_sept.png", p2, width = 10, height = 6)