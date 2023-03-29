library(tidyverse)
library(here)

library(showtext)
showtext_opts(dpi = 300)
showtext_auto()

library(myriad)
import_myriad_semi()
import_myriad_condensed()
theme_set(theme_myriad_semi())


compare <- c("AUT", "AUS", "BEL", "CAN", "DEU", "DNK", "ESP", "FIN", "FRA",
             "GBR", "GRC", "IRL", "ITA", "JPN", "NLD", "NOR", "NZL",
             "SWE", "USA")

my_colors <-  c("grey50", "firebrick")

df_lex <- read_csv(here("data", "lifexp.csv")) |>
  janitor::clean_names() |>
  select(cou:value) |>
  rename(lifexp = value)

df_spend <- read_csv(here("data", "spending.csv")) |>
  janitor::clean_names() |>
  select(location:value) |>
  rename(health_ppp = value)

df_lex

df_spend

df <- left_join(df_spend, df_lex, by = c("country", "year")) |>
  mutate(us_flag = ifelse(cou == "USA", "United States", "Eighteen Other OECD Countries"))

out <- df |>
  drop_na() |>
  arrange(country, year) |>
  group_by(country) |>
  mutate(avg_spend = slider::slide_dbl(health_ppp, mean, .before = 3)) |>
  filter(cou %in% compare) |>
  ggplot(aes(x = avg_spend, y = lifexp, group = country, color = us_flag)) +
  geom_line() +
  scale_color_manual(values = my_colors) +
  scale_x_continuous(labels = scales::label_dollar()) +
  labs(color = NULL,
       title = "Health Spending and Life Expectancy, 1970-2021",
       x = "Heath Spending (Per capita, constant prices, constant PPPs, 3 year rolling average)",
       y = "Life Expectancy",
       caption = "Data: OECD. Graph: @kjhealy")

ggsave(here::here("figures", "health-gdp.pdf"), out, height = 8, width = 8)
ggsave(here::here("figures", "health-gdp.png"), out, height = 8, width = 8, bg = "white")
