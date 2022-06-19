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
# make plots for case types per case category (Shareholder risks, etc)
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



# find years in text case description
library(stringr)
cases = cases %>%
  #mutate(year_missing = str_extract(CASE_DESCRIPTION, '(?<=, )([0-9]{4})')) #%>%
  mutate(year_missing = str_extract(CASE_DESCRIPTION, '([0-9]{4})')) # (?<=[I-O]n \\w+ \\w*\\s*)([0-9]{4})
cases$year_missing = as.numeric(cases$year_missing)
# merge years with the ones found in text
cases$YEAR = coalesce(cases$YEAR, cases$year_missing)
cat('Number of the years still missing? ',nrow(cases[which(is.na(cases$YEAR)),]))

cases_year = cases %>% group_by(YEAR) %>% summarize(count = n())
cases_year
cases_year = na.omit(cases_year)
cat("How many claims per year in average? ", mean(cases_year$count))
cat("How many years >0?", nrow(cases_year))
cat("Last year = ", max(cases_year$YEAR))

# plot frequency of claims by year
ggplot(data = cases[which(cases$YEAR >1950),], aes(x = YEAR, y = ..prop.., group = 1)) + 
  geom_bar( fill = "blue") + 
  #geom_text(stat='count', aes(label=round(..prop.., digits=6)), vjust=-1) +
  ggtitle("Distribution of cases by year.") + 
  xlab("YEAR")  +
  ylab("Relative Frequency") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  xlim(1950,2022)
  #expand_limits(y=0.1, x=c(1950, 2022)) # fix the limits of the graph
  #scale_x_continuous(breaks = round(seq(1950, 2022, by = 10),1))

ggsave("Distribution of cases by year.png")

# plot frequency of case status
ggplot(data = cases, aes(x = CASESTATUS, y = ..prop.., group = 1)) + 
  geom_bar( fill = "blue") + 
  #geom_text(stat='count', aes(label=round(..prop.., digits=6)), vjust=-1) +
  ggtitle("Distribution of case status") + 
  xlab("CASE STATUS")  +
  ylab("Relative Frequency") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave("Distribution of case status.png")

sum(nrow(cases[with(cases, which(CASESTATUS %in% c("Settled", "Dismissed"))),]))

write.csv(cases, "cases_cleaned.csv")

# outer join cases and stat by company id
all_data = merge(x = cases, y = stat, by = "COMPANY_ID", all.x = TRUE)

write.csv(all_data, "all_data.csv")

sic_missing = nrow(all_data[which(is.na(all_data$SIC_DESC)),])
cat("number of missing SIC = ", sic_missing)

library(tidyr)

# group by industry, country, case status
data_final = all_data %>%
             filter(YEAR >= 1990) %>%
             filter(str_detect(CASESTATUS, 'Dismissed|Settled')) %>%
             drop_na(SIC_DESC, COUNTRY_CODE, CASESTATUS) %>%
             group_by(COUNTRY_CODE, CASESTATUS, SIC_DESC) %>%
             summarise(count = n())

data_final$CASESTATUS = sub("Dismissed.+", "Dismissed", data_final$CASESTATUS)

# FREQUENCY MODELLING
# frequency features: industry, country, marketcap
glm.model <-glm(formula = count ~ COUNTRY_CODE + SIC_DESC + CASESTATUS,
             data = data_final, family = poisson)
print(summary(glm.model))    



#use model to predict value of am
newdata = data.frame(SIC_DESC="PHARMACEUTICAL PREPARATIONS", CASESTATUS="Dismissed", COUNTRY_CODE="USA")
nb_claims_dismissed = predict(glm.model, newdata, type="response")
newdata$CASESTATUS = "Settled"
nb_claims_settled = predict(glm.model, newdata, type="response")
freq_dismissed = nb_claims_dismissed/(nb_claims_dismissed + nb_claims_settled)
freq_dismissed = nb_claims_settled/(nb_claims_dismissed + nb_claims_settled)
freq = (nb_claims_dismissed + nb_claims_settled)/32
cat("How often a claim occurs for ", newdata$SIC_DESC, "? ", freq*100, "%")
cat("How often a claim is dimised for ", newdata$SIC_DESC, "? ", freq_dismissed*100, "%")
