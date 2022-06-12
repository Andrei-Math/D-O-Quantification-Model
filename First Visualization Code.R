# Navigate to the folder
setwd("C:/Users/CraciunescuA/OneDrive - Willis Towers Watson/Cyber&D&O Pricing/D&O/Public/Data Prep")

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
# overview of all countries
ggplot(data=cases, aes(x=COUNTRY_CODE)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) +
  labs(y = "Frequency", x = "Country")

ggplot(data=cases_filter_country, aes(x=COUNTRY_CODE)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(y = "Frequency", x = "Country")


ggplot(data=cases, aes(x=CASE_CATEGORY)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(y = "Frequency", x = "Loss events")


cases$CASE_TYPE <- factor(cases$CASE_TYPE)  # Converts the gear variable into a factor

for (case_category in unique(cases$CASE_CATEGORY)) {
  ggplot(data = cases[which(cases$CASE_CATEGORY==case_category),], aes(x = CASE_TYPE, y = ..prop.., group = 1)) + 
  geom_bar( fill = "blue") + 
  geom_text(stat='count', aes(label=round(..prop.., digits=2)), vjust=-1) +
  ggtitle(paste("Distribution of Case types for ", case_category)) + 
  xlab("Case types")  +
  ylab("Relative Frequency") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  expand_limits(y=1)
  
  ggsave(paste("Distribution of case types for ", case_category, ".png"))
}

