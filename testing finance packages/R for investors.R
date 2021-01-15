library(tidyverse)
library(tidyquant)
library(timetk)
library(tibbletime)
library(scales)
library(highcharter)
library(broom)
library(PerformanceAnalytics)
library(tsibble)


symbols <- c("^GSPC", "^IRX")

prices <- tq_get(symbols,
                 get = "stock.prices",
                 from = "1990-01-01")

sp500 <- tq_get("^GSPC",
                get = "stock.prices",
                from = "1995-01-01")

treasury <- tq_get("^IRX",
                   get = "stock.prices",
                   from = "199-01-01")

prices %>%
  filter(symbol == "^GSPC") %>% 
  hchart(., hcaes(x = date, y = adjusted), type = "line") %>% 
  hc_title(text = "GSPC prices")

returns <- prices %>%
  select(symbol, date, adjusted) %>% 
  spread(symbol, adjusted) %>% 
  rename(sp500 = "^GSPC", treasury = "^IRX") %>%
  na.omit() %>% 
  mutate(sp500_returns = log(sp500) - log(lag(sp500)),
         daily_treasury = (1 + (treasury / 100)) ^ (1/252) - 1)

returns %>%
  tq_performance(Ra = sp500_returns,
                 performance_fun = table.Stats) %>% 
  t() %>%
  knitr::kable()

sp500_returns_xts <- xts(returns$sp500_returns, order.by = returns$date)

table.Stats(sp500_returns_xts)

table.CAPM(sp500_returns_xts, sp500_returns_xts)

table.Drawdowns(sp500_returns_xts)

table.DownsideRisk(sp500_returns_xts)

returns %>% 
  ggplot(aes(date, sp500_returns)) +
  geom_point() +
  scale_x_date(breaks = breaks_pretty(n = 30)) +
  labs(title = "SP500 daily returns",
       y = "daily_percent") +
  theme(axis.text.x = element_text(angle = 90))

roll_mean_50 <- rollify(mean, window = 50)
roll_mean_200 <- rollify(mean, window = 200)

sma_trend <- returns %>% 
  mutate(sma_200 = roll_mean_200(sp500),
         sma_50 = roll_mean_50(sp500)) %>%
  mutate(trend_signal = if_else(sma_50 > sma_200, 1, 0),
         buy_hold_returns = (0.9 * sp500_returns + 0.1 * daily_treasury),
         trend_returns = if_else(lag(trend_signal) == 1, (trend_signal * sp500_returns), daily_treasury)) %>%
  na.omit() %>%
  mutate(trend_growth = accumulate(1 + trend_returns, `*`),
         buy_hold_growth = accumulate(1 + buy_hold_returns, `*`))

sma_trend %>% 
  select(date, buy_hold_returns, trend_returns) %>% 
  gather(strategy, returns, -date) %>% 
  group_by(strategy) %>% 
  tq_performance(Ra = returns,
                 performance_fun = table.Stats) %>% 
  t() %>%
  knitr::kable()


add_z_trend <- sma_trend %>% 
  mutate(z_spread = (sp500 - sma_200),
         z_score  = (z_spread - mean(z_spread)) / sd(z_spread),
         z_signal = if_else(lag(z_score, 1) < - 0.05 &
                            lag(z_score, 2) < - 0.05 &
                            lag(z_score, 3) < - 0.05,
                            0, 1),
         trend_z_returns = if_else(lag(trend_signal) == 1 &
                                   z_signal == 1,
                                   (trend_signal * sp500_returns), daily_treasury)) %>%
  na.omit() %>% 
  mutate(trend_z_growth = accumulate(1 + trend_z_returns, `*`))

add_z_trend %>% 
  select(date, buy_hold_returns, trend_returns, trend_z_returns) %>% 
  gather(strategy, returns, -date) %>% 
  group_by(strategy) %>% 
  tq_performance(Ra = returns,
                 performance_fun = table.Stats) %>%
  t() %>%
  knitr::kable()

results <- add_z_trend %>% 
  select(date, buy_hold_growth, trend_growth, trend_z_growth, daily_treasury) %>% 
  gather(strategy, growth, -date, -daily_treasury)

results %>%
  hchart(., hcaes(x = date, y = growth, group = strategy), type = "line") %>% 
  hc_tooltip(pointFormat = "{point.strategy}: ${point.growth: .2f}") %>%
  hc_title(text = "Portfolio Growth")


