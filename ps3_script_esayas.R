library(tidyverse)
library(fpp3)

# 2.2
retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)

# 2.3a
retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")

# 2.4a
retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)

# 2.4b
head(retail_ts)
has_gaps(retail_ts)

# 2.5a
head(canadian_gas)
key_vars(canadian_gas)
index_var(canadian_gas)

# 2.5b
can_gas <- canadian_gas |>
  select(Month, Volume)

has_gaps(can_gas)
head(can_gas)

# 2.6a
can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))

# 2.6b
can_gas_recent <- can_gas_recent |>
  mutate(
    year = year(Month),
    month = month(Month, label = TRUE),
    quarter = quarter(Month)
  )

can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))

# 2.6c
can_gas_recent |>
  mutate(
    next_month = Month + 1,
    same_month_last_year = Month - 12
  ) |>
  head()

# 2.6d
can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)

# 2.7b
p_retail <- retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()

# 2.7c
p_cangas <- can_gas |>
  autoplot(Volume) +
  labs(
    title = "Monthly Canadian Natural Gas Production",
    x = NULL, y = "Billions of cubic meters"
  ) +
  theme_minimal()

# 2.7d
p_cangas_choice <- can_gas |>
  autoplot(Volume) +
  geom_smooth(method = "loess", se = FALSE, color = "firebrick") +
  labs(
    title = "Canadian Natural Gas Production with Trend",
    x = NULL, y = "Billions of cubic meters"
  ) +
  theme_minimal()

  # 3.1b
can_gas <- can_gas |>
  mutate(log_volume = log(Volume))

p_cangas_log <- can_gas |>
  autoplot(log_volume) +
  labs(title = "Log Monthly Canadian Natural Gas Production", y = "log(Volume)") +
  theme_minimal()

# 3.1c
retail_nsa <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  mutate(log_sales = log(sales))

p_retail_nsa_level <- retail_nsa |>
  autoplot(sales) +
  labs(title = "U.S. Retail Sales (NSA) - Level", y = "Millions of dollars") +
  theme_minimal()

p_retail_nsa_log <- retail_nsa |>
  autoplot(log_sales) +
  labs(title = "U.S. Retail Sales (NSA) - Log", y = "log(Sales)") +
  theme_minimal()

# 3.2a
can_gas <- can_gas |>
  mutate(
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume),
    growth_gap = abs(growth_direct - growth_logdiff)
  )

p_growth_direct <- can_gas |>
  autoplot(growth_direct) +
  labs(title = "Canadian Gas Production: Direct MoM Growth Rate", y = "% Growth") +
  theme_minimal()

p_growth_gap <- can_gas |>
  autoplot(growth_gap) +
  labs(title = "Absolute Gap: Direct vs. Log-Difference Growth", y = "Gap (% points)") +
  theme_minimal()

# 3.3
retail_ts <- retail_ts |>
  group_by_key() |>
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()

p_growth_sa_nsa <- retail_ts |>
  autoplot(growth_mom) +
  labs(
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL, y = "Growth rate (%)", color = NULL
  ) +
  theme_minimal()