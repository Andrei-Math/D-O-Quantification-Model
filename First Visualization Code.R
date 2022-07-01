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
  mutate(year_missing = str_extract(CASE_DESCRIPTION, '(19[0-9][0-9]|200[0-9]|201[0-9]|202[0-2])')) # (?<=[I-O]n \\w+ \\w*\\s*)([0-9]{4})
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

# summarize the industry type by nb claims
industries = stat %>% group_by(by=SIC_DESC) %>%
                      summarize(NbClaims = n()) %>%
                      arrange(NbClaims)

write.csv(industries, 'industries.csv')

# summarize the country code by nb claims
countries_avg = cases %>% group_by(COUNTRY_CODE, YEAR) %>%
  summarize(NbClaims = n()) %>%
  group_by(COUNTRY_CODE) %>%
  summarize(NbClaims = mean(NbClaims)) %>%
  arrange(desc(NbClaims)) %>%
  mutate(NbClaims = round(NbClaims, 3))

write.table(countries_avg, 'countries_avg.csv', dec=',')

sum(nrow(cases[with(cases, which(CASESTATUS %in% c("Settled", "Dismissed"))),]))

write.csv(cases, "cases_cleaned.csv")

# grab the table to relate country code and subregion
# work out the subregions to join countries
countries_code = read.csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv")
countries_code = countries_code[,c('alpha.3', 'sub.region')]

# Europe
countries_code$sub.region[which(countries_code$alpha.3=="DEU")] = 'Germany'
countries_code$sub.region[which(countries_code$alpha.3=="ESP")] = 'Spain'
countries_code$sub.region[which(countries_code$alpha.3=="FRA")] = 'France'
countries_code$sub.region[which(countries_code$alpha.3=="GBR")] = 'United Kingdom'
countries_code$sub.region[which(countries_code$alpha.3=="NLD")] = 'Netherlands'
countries_code$sub.region[which(countries_code$alpha.3=="CHE")] = 'Switzerland'
europe_regions = c('Northern Europe',
                   'Southern Europe',
                   'Eastern Europe',
                   'Western Europe')
countries_code$sub.region[which(countries_code$sub.region %in% europe_regions)] = 'Rest of Europe'
#Africa
africa_regions = c('Sub-Saharan Africa',
                   'Northern Africa')
countries_code$sub.region[which(countries_code$sub.region %in% africa_regions)] = 'Africa'
# America
countries_code$sub.region[which(countries_code$alpha.3=="USA")] = 'United States'
countries_code$sub.region[which(countries_code$sub.region=="Northern America")] = 'United States'
countries_code$sub.region[which(countries_code$alpha.3=="CAN")] = 'Canada'
# Asia
countries_code$sub.region[which(countries_code$alpha.3=="CHN")] = 'China'
countries_code$sub.region[which(countries_code$alpha.3=="HKG")] = 'Honk Kong'
countries_code$sub.region[which(countries_code$alpha.3=="JPN")] = 'Japan'
asia_regions = c('Eastern Asia','Southern Asia','South-eastern Asia','Western Asia','Central Asia')
countries_code$sub.region[which(countries_code$sub.region %in% asia_regions)] = 'Rest of Asia'
# Middle East
countries_code$sub.region[which(countries_code$alpha.3=="ISR")] = 'Israel'
# Oceania
oceania_regions = c("Australia and New Zealand",
                    "Micronesia",
                    "Polynesia")
countries_code$sub.region[which(countries_code$sub.region %in% oceania_regions)] = 'Oceania'

# join the subregions in the dataset by country code
library(dplyr)
cases = cases %>% left_join(countries_code, by=c("COUNTRY_CODE" = "alpha.3"))

# summarize the sub region by nb claims
regions_avg = cases %>% group_by(sub.region, YEAR) %>%
  summarize(NbClaims = n()) %>%
  group_by(sub.region) %>%
  summarize(NbClaims = mean(NbClaims)) %>%
  arrange(desc(NbClaims)) %>%
  mutate(NbClaims = round(NbClaims, 3))

write.table(regions_avg, 'regions_avg.csv', dec=',')

# outer join cases and stat by company id
all_data = merge(x = cases, y = stat, by = "COMPANY_ID", all.x = TRUE)

# make column is_USA_juris to check if juris country is USA (1) or not (0)
all_data$is_USA_juris <- ifelse(all_data$JURIS_COUNTRY_CODE == "USA", 1, 0)
all_data$is_USA_juris[is.na(all_data$is_USA_juris)] <- 0

write.csv(all_data, "all_data.csv")

sic_missing = nrow(all_data[which(is.na(all_data$SIC_DESC)),])
cat("number of missing SIC = ", sic_missing)


library(tidyr)

# group by industry, country, case status
cdata = all_data %>%
             filter(YEAR >= 1990) %>%
             #filter(str_detect(CASESTATUS, 'Dismissed|Settled')) %>%
             #drop_na(SIC_DESC, COUNTRY_CODE, CASESTATUS) %>%
             group_by(YEAR, COMPANY_ID, is_USA_juris) %>%
             summarise(ClaimNb = n(), 
                       REVENUES = unique(REVENUES),
                       SUB_REGION = unique(sub.region),
                       #SIC_DESC = unique(SIC_DESC),
                       EMPLOYEES = unique(EMPLOYEES))

# drop all NANs
cdata <- cdata[complete.cases(cdata),]

# remove dots from REVENUES
cdata$REVENUES<-gsub("\\.","",as.character(cdata$REVENUES))

# plot nbclaim against sic_desc
ggplot(cdata, aes(x=SIC_DESC, y=ClaimNb)) +  
    geom_boxplot(fill='green') +
    ylim(0,20)
  

# Calculate total number of policies
n_policies = nrow(cdata)
n_policies

# check the ClaimNb
ClaimNb <- cdata$ClaimNb
summary(ClaimNb)
min(ClaimNb)
mean(ClaimNb)
max(ClaimNb)


write.csv(cdata, "cdata.csv")

# store the original data separately
odata <- cdata


# What, if we assume (disregarding Exposure!) that ClaimNb ~ Poi(lambda) ?
# We can estimate lambda via ML from ...

sum(ClaimNb)/length(ClaimNb)

# ######################
# # Cleaning unrealistic
# # number of claims
# ######################
# 
# for (i in (1:n))
# {
#   if (odata$ClaimNb[i]>4)
#   {
#     cdata$ClaimNb[i]<-4
#   }
# }
# 
# mean(odata$ClaimNb)
# mean(cdata$ClaimNb)
# ClaimNb <- cdata$ClaimNb

################################################################################
# 1.1.2.b Frequency
################################################################################

Exposure = max(cdata$YEAR) - min(cdata$YEAR)
Exposure
Frequency <- sum(ClaimNb)/sum(Exposure)
Frequency

################################################################################
# 2. Learning / Testing data split
################################################################################

n <- nrow(cdata)
set.seed(100)
subsetlearn <- sample(c(1:n), round(0.9*n), replace=FALSE)
learn <- cdata[subsetlearn,]
test <- cdata[-subsetlearn,]

######################
# tests if done correctly
######################
n-(nrow(learn)+nrow(test))
nrow(learn)/n
nrow(test)/n

################################################################################
# 3. GLM
################################################################################

#IndustryGLM <- cbind( unique(learn$SIC_DESC), c(1:length(unique(learn$SIC_DESC))))
#learn$IndustryGLM <- as.factor(learn$SIC_DESC)
learn$USAJurisGLM <- as.integer(learn$is_USA_juris)
learn$REVENUESGLM <- as.numeric(learn$REVENUES)
learn$REGIONGLM <- as.factor(learn$SUB_REGION)
learn$EMPLOYEESGLM <- as.numeric(learn$EMPLOYEES)

ClaimNb         <- learn$ClaimNb
#IndustryGLM     <- learn$IndustryGLM
USAJurisGLM     <- learn$USAJurisGLM
REVENUESGLM      <- learn$REVENUESGLM 
REGIONGLM      <- learn$REGIONGLM
EMPLOYEESGLM    <- learn$EMPLOYEESGLM

#test$IndustryGLM <- as.factor(test$SIC_DESC)
test$USAJurisGLM <- as.integer(test$is_USA_juris)
test$REVENUESGLM <- as.numeric(test$REVENUES)
test$REGIONGLM <- as.factor(test$SUB_REGION)
test$EMPLOYEESGLM <- as.numeric(test$EMPLOYEES)

glm_freq = function(formula, glm_name){
  # GLM model withl all covariates
  glm<-glm(formula=formula, family=poisson(), data = learn)
  learn[,glm_name] <- fitted(glm)
  summary(glm)
  
  
  # ERROR - there are unknown factors in the test set
  # Faktor 'IndustryGLM' hat neue Stufen ADJUSTMENT AND COLLECTION SERVICES, BEAUTY SHOPS, CANE SUGAR, EXCEPT REFINING, CHEWING AND SMOKING TOBACCO AND SNUFF, COMPUTER AND COMPUTER SOFTWARE STORES, CRUSHED AND BROKEN LIMESTONE, FLOUR AND OTHER GRAIN MILL PRODUCTS, FROZEN SPECIALTIES, NEC, INDUSTRIAL PROCESS FURNACES AND OVENS, JEWELRY, PRECIOUS METAL, MEAT PACKING PLANTS, ORDNANCE AND ACCESSORIES, NEC, PHOTOFINISHING LABORATORIES, PRIMARY METAL PRODUCTS, NEC, TESTING LABORATORIES
  test[,glm_name] <- predict(glm, newdata=test, type="response")
  
  ######################
  # In-Sample errors
  ######################
  
  in_sample <- 2*( sum( learn[,glm_name] ) - sum( learn$ClaimNb ) + sum( log(( learn$ClaimNb / learn[,glm_name] )^(learn$ClaimNb))))
  average_in_sample <- in_sample / nrow(learn)
  cat("in-sample avg error = ", average_in_sample,'\n')
  
  ######################
  # Out-of-Sample errors
  ######################
  
  out_of_sample <- 2*( sum( test[,glm_name] ) - sum( test$ClaimNb ) + sum( log(( test$ClaimNb / test[,glm_name] )^(test$ClaimNb))))
  average_out_of_sample <- out_of_sample / nrow(test)
  cat("out-of-sample avg error = ", average_out_of_sample)
  
  results <- list(
    'in-sample avg error'=average_in_sample,
    'out-of-sample avg error'=average_out_of_sample,
    'learn' = fitted(glm),
    'test' = predict(glm, newdata=test, type="response"),
    'model' = glm
    )
  
  return(results)
}

results1 = glm_freq(ClaimNb ~  USAJurisGLM + REVENUESGLM + REGIONGLM + EMPLOYEESGLM, 'GLM1')
results2 = glm_freq(ClaimNb ~  USAJurisGLM, 'GLM2')

################################################################################
# 4. Tree
################################################################################

ClaimNb         <- learn$ClaimNb
IndustryGLM     <- learn$IndustryGLM
USAJurisGLM     <- learn$USAJurisGLM
REVENUEGLM      <- learn$REVENUEGLM 
COUNTRYGLM      <- learn$COUNTRYGLM 
EMPLOYEESGLM    <- learn$EMPLOYEESGLM
tree1 <- rpart(cbind(Exposure, ClaimNb) ~ Area + VehPower + VehAge + DrivAge + BonusMalus + VehBrand + VehGas + Density + Region, data = learn , method="poisson", control=rpart.control(xval=1, minbucket=10000, cp=0.0005))
# rpart.plot( tree1 )


######################
# In-Sample errors
######################

learn$fit <- predict(tree1, learn, type = 'vector')
in_sample <- 2*( sum( learn$fit ) - sum( learn$ClaimNb ) + sum( log(( learn$ClaimNb / learn$fit )^(learn$ClaimNb))))
average_in_sample <- in_sample / nrow(learn)

average_in_sample

######################
# Out-of-Sample errors
######################

test$fit <- predict(tree1, test, type = 'vector')

out_of_sample <- 2*( sum( test$fit ) - sum( test$ClaimNb ) + sum( log(( test$ClaimNb / test$fit )^(test$ClaimNb))))
average_out_of_sample <- out_of_sample / nrow(test)
average_out_of_sample 
# data_final$CASESTATUS = sub("Dismissed.+", "Dismissed", data_final$CASESTATUS)
# 
# # FREQUENCY MODELLING
# # frequency features: industry, country, marketcap
# glm.model <-glm(formula = count ~ COUNTRY_CODE + SIC_DESC + CASESTATUS,
#              data = data_final, family = poisson)
# print(summary(glm.model))    
# 
# 
# 
# #use model to predict value of am
# newdata = data.frame(SIC_DESC="PHARMACEUTICAL PREPARATIONS", CASESTATUS="Dismissed", COUNTRY_CODE="USA")
# nb_claims_dismissed = predict(glm.model, newdata, type="response")
# newdata$CASESTATUS = "Settled"
# nb_claims_settled = predict(glm.model, newdata, type="response")
# freq_dismissed = nb_claims_dismissed/(nb_claims_dismissed + nb_claims_settled)
# freq_dismissed = nb_claims_settled/(nb_claims_dismissed + nb_claims_settled)
# freq = (nb_claims_dismissed + nb_claims_settled)/32
# cat("How often a claim occurs for ", newdata$SIC_DESC, "? ", freq*100, "%")
# cat("How often a claim is dimised for ", newdata$SIC_DESC, "? ", freq_dismissed*100, "%")
