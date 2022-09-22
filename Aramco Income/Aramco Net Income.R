library(tidyverse)
library(nflreadr)
library(zoo)

# custom theme
theme_owen <- function() {
  theme_minimal(base_size = 9, base_family = 'Consolas') %+replace%
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(size = 0.2),
      plot.background = element_rect(color = 'floralwhite', fill = 'floralwhite'),
      axis.text = element_text(color = '#4C4E52')
    )
}

# read data
df <- read_csv("Aramco_Net_Income.csv")
df$yrqtr <- as.yearqtr(df$yrqtr, format = "%Y-Q%q")

# plot data
df |> 
  ggplot(aes(x = factor(yrqtr), y = net_income)) +
  geom_col(fill = "#22459F", width = 0.65) +
  scale_y_continuous(labels = scales::dollar_format(suffix = "bn")) +
  geom_text(aes(label = scales::dollar(net_income, suffix = "bn")), 
            vjust = -0.5, size = 3, family = 'Consolas', color = '#63666A') +
  labs(title = "Saudi Aramco's Quarterly Net Profit",
       subtitle = "[2020 - present]", 
       x = "", 
       y = "", 
       caption = "Source: Company Filings") +
  theme_owen() +
  theme(plot.title = element_text(face = 'bold', size = 12))

# save the plot
ggsave("saudi_profits.png", width = 8, height = 6, dpi = 300)






