
library(readr)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)

# Load
anes <- read_csv("data/anes_timeseries_cdf_csv_20220916.csv")

# Race
# VCF0105a

# Year
# VCF0004

# Census Region
# VCF0112
#1. Northeast (CT, ME, MA, NH, NJ, NY, PA, RI, VT)
#2. North Central (IL, IN, IA, KS, MI, MN, MO, NE, ND, OH, SD, WI)
#3. South (AL, AR, DE, D.C., FL, GA, KY, LA, MD, MS, NC, OK, SC,TN, TX, VA, WV)
#4. West (AK, AZ, CA, CO, HI, ID, MT, NV, NM, OR, UT, WA, WY)

anes$south <- ifelse(anes$VCF0112 == 3, "South", "Non-South")

# Income
#1. 0 to 16 percentile
#2. 17 to 33 percentile
#3. 34 to 67 percentile
#4. 68 to 95 percentile
#5. 96 to 100 percentile

anes$income <- ifelse(anes$VCF0114 == 0, NA, anes$VCF0114)
### Party
# 1. Democrats (including leaners)
# 2. Independents
# 3. Republicans (including leaners)
anes$party <- ifelse(anes$VCF0303 == 0, NA, anes$VCF0303)

### Analysis
inc_party_cor <- anes %>%
  filter(VCF0105a == 1) %>%
  group_by(VCF0004) %>%
  summarize(n_obs = sum(!is.na(income) & !is.na(party)),
            inc_party_cor = ifelse(n_obs > 5, cor(income, party, use = "complete.obs"), NA))

inc_rep_support <- anes %>%
  filter(VCF0105a == 1) %>%
  group_by(VCF0004, income) %>%
  summarize(inc_party_cor = mean(party == 3, na.rm = T)) %>%
  pivot_wider(names_from = income, values_from = c(inc_party_cor))


inc_party_cor_region <- anes %>%
  filter(VCF0105a == 1) %>%
  group_by(VCF0004, south) %>%
  summarize(n_obs = sum(!is.na(income) & !is.na(party)),
            inc_party_cor = ifelse(n_obs > 5, cor(income, party, use = "complete.obs"), NA)) %>%
  pivot_wider(names_from = south, values_from = c(n_obs, inc_party_cor))

print(inc_party_cor, n = 100)
print(inc_party_cor_region, n = 100)
print(inc_party_cor %>% 
        filter(VCF0004 > 1978) %>% 
        arrange(inc_party_cor), 
      n = 40)

ggplot(anes, aes(x = income, y = party)) +
  stat_smooth(
    method = "loess",
    formula = y ~ x,
    se = FALSE,
    data = . %>% filter(!is.na(party))) +  
  facet_wrap(~ VCF0004) +
  theme_minimal()
  
  ggplot(anes, aes(x = income, y = party, color = VCF0004, group = VCF0004)) +
  geom_line(aes(color = VCF0004), stat = "smooth", method = "loess", se = FALSE) +
  scale_color_gradient(low = "#dddddd", high = "#000000") +
  theme_minimal()

  proportion_data <- anes %>%
    group_by(VCF0004, income) %>%
    summarise(prop_rep = mean(party == 3, na.rm = TRUE))
  
  ggplot(proportion_data, aes(x = income, y = prop_rep)) +
    stat_smooth(
      method = "loess",
      se = FALSE
    ) +
    ylab("Proportion supporting Republicans") +
    xlab("Income Quintile") +  
    facet_wrap(~ VCF0004) +
    theme_bw()
    
  ggsave("income_quintile_prop_rep_over_time_white_anes.png", width = 5)
  
  
  proportion_data <- anes %>%
    group_by(VCF0004, south, income) %>%
    summarise(prop_rep = mean(party == 3, na.rm = TRUE))
  
  ggplot(proportion_data, aes(x = income, y = prop_rep, color = south)) +
    stat_smooth(
      method = "loess",
      se = FALSE,
      data = . %>% filter(!is.na(south))
    ) +
    ylab("Proportion supporting Republicans") +
    xlab("Income Quintile") +  
    facet_wrap(~ VCF0004) +
    theme_bw() + 
    annotate("text", x = Inf, y = -Inf, label = "Source: ANES Cum. https://github.com/soodoku/anes_inc_pid_cor", hjust = 1, vjust = 0, size = 3)

  ggsave("income_quintile_prop_rep_over_time_white_south_anes.png", width = 5)
  