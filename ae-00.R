#| label: load-packages
#| warning: false
#| message: false

library(tidyverse)
library(lubridate)
library(scales)
library(DT)
library(unvotes)

unvotes <- un_votes |>
  inner_join(un_roll_calls, by = "rcid") |>
  inner_join(un_roll_call_issues, by = "rcid", relationship =
               "many-to-many")

#| label: plot-yearly-yes-issue
#| message: false
#| fig-width: 10
#| fig-height: 6

# Canada added by Glen Armstrong

unvotes |>
  filter(country %in% c("United Kingdom", "United States", "Turkey", "Canada")) |>
  mutate(year = year(date)) |>
  group_by(country, year, issue) |>
  summarize(percent_yes = mean(vote == "yes")) |>
  ggplot(mapping = aes(x = year, y = percent_yes, color = country)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~issue) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Percentage of 'Yes' votes in the UN General Assembly",
    subtitle = "1946 to 2019",
    y = "% Yes",
    x = "Year",
    color = "Country"
  )
