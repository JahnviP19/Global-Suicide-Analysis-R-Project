# Reading in files
library(dplyr)
library(ggplot2)
library(gridExtra)
library(countrycode)
library(purrr)
library(randomForest)

suicides_data <- read.csv('master.csv')

# DATA CLEANING AND PRE-PROCESSING
suicides_data_final <- filter(suicides_data,year<2016)
summary(suicides_data_final)

#no. of countries
summarise(suicides_data_final,n_distinct(country))  # or
nrow(distinct(suicides_data_final, country))

#quartiles
summary(suicides_data_final$gdp_per_capita....)

#categorising <- #quartiles: 1 - underdeveloped, 2 - developing, 3 - developed, 4 - highly developed
suicides_data_final$development_status <- ifelse(suicides_data_final$gdp_per_capita....>=251 & suicides_data_final$gdp_per_capita....<3436,1,
                                                 ifelse(suicides_data_final$gdp_per_capita....>=3436 & suicides_data_final$gdp_per_capita.... < 9283,2,
                                                        ifelse(suicides_data_final$gdp_per_capita....>=9283 & suicides_data_final$gdp_per_capita.... < 24796,3,4)))
suicides_data_final$development_status <- as.factor(suicides_data_final$development_status)
summary(suicides_data_final$development_status)

#droppiong hdi, country.year, generation
suicides_data_final <- suicides_data_final %>% select(-c(HDI.for.year,country.year,generation))
suicides_data_final$gdp_for_year.... <- as.numeric(suicides_data_final$gdp_for_year....)

#total 31 years of data, some countries have very few years of data
rm <- group_by(suicides_data_final,country) %>% summarise(year_count = n_distinct(year))

#removing countries with less than 10 years of data
rm_country <- rm$country[rm$year_count<10]
suicides_data_final<- suicides_data_final %>% filter(!(country %in% rm_country))

#renaming columns
suicides_data_final <- suicides_data_final %>% rename(gdp_for_year = gdp_for_year....,gdp_per_capita = gdp_per_capita....)
str(suicides_data_final)

suicides_data_final$sex = ifelse(suicides_data_final$sex == "male", "Male", "Female")
head(suicides_data_final$sex)

suicides_data_final$continent <- countrycode(suicides_data_final$country, "country.name", "continent")
suicides_data_final$continent <- as.factor(suicides_data_final$continent)
suicides_data_final$sex <- as.factor(suicides_data_final$sex)
suicides_data_final$age <- gsub(" years", "", suicides_data_final$age)
suicides_data_final$age <- factor(suicides_data_final$age, ordered = T, levels = c("5-14", "15-24", "25-34", "35-54", "55-74", "75+"))
str(suicides_data_final)

# Visualisations

#Global Trend over time
global_average <- mean(suicides_data_final$suicides.100k.pop)
year.wise.suicide.plot <- (suicides_data_final %>% 
                             group_by(year) %>% 
                             summarise(mean.suicides.per.100K.pop = mean(suicides.100k.pop)) %>% 
                             ggplot(aes(x = year, y = mean.suicides.per.100K.pop,group = 1)) + 
                             geom_line(col = 'coral',size = 1) + 
                             geom_point(col = 'coral',size = 2) +
                             geom_hline(yintercept = global_average, linetype = 2, size = 1) +
                             scale_x_continuous(breaks = seq(1985, 2015, 2))) + 
  labs(title = "Global Trend over time (1985-2015)", x = "Year", y = "Average Suicides (per 100k)") +
  theme(plot.title = element_text(size = 20), axis.text = element_text(size = 14), axis.title = element_text(size = 16),
        axis.text.x = element_text(angle = 90,hjust =1) )

year.wise.suicide.plot

#Median Suicides over time 
suicides_data_final %>% 
  group_by(year) %>% 
  summarise(count = n(), median_suicides = median(suicides.100k.pop)) %>% 
  ungroup() %>% 
  ggplot(aes(year, median_suicides)) +
  geom_point(aes(size = median_suicides, color = median_suicides)) +
  geom_line(aes(color = median_suicides,alpha = 0.2)) +
  geom_hline(aes(yintercept = max(median_suicides),alpha = 0.2), linetype = "dotted") +
  geom_hline(aes(yintercept = median(median_suicides),alpha = 0.2), linetype = "dotted") +
  geom_hline(aes(yintercept = min(median_suicides),alpha = 0.2), linetype = "dotted") +
  #geom_vline(aes(xintercept = max(medianSuicides), alpha = 0.2)) +
  scale_color_continuous(low = "blue", high = "red") +
  guides(alpha = FALSE) +
  labs(title = "Median Suicides across the years", subtitle = "Median Suicides overall is roughly 25", y = "Median Suicides", x = "Year")


