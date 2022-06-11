library(dplyr)
library(readr)
cases <- read_delim("std_pub_do_mscad_cases_20220502.csv", 
                                              delim = ";", escape_double = FALSE, trim_ws = TRUE)

library(readr)
stat <- read_delim("std_pub_do_mscad_stat_20220502.csv", 
                                              delim = ";", escape_double = FALSE, trim_ws = TRUE)

# filter out countrys with less than 10 occurences
cases_filter_country = cases %>% group_by(COUNTRY_CODE) %>%  filter(n() >= 10)
cases_filter_country

library(ggplot2)
ggplot(data=cases_filter_country, aes(x=COUNTRY_CODE)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
