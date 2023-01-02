
# In each dataset change only the name of the country in each section for different graphs 

# Libraries

library(tidyverse)
library(randomForest)
library(leaflet)
library(dplyr)
library(caret)
library(e1071)

df = read_csv("C:/Users/dhaval/Desktop/R/6600 - Visualization/project/app/www/data/country_migration.csv")
# head(df)

#Summing up the net per 10k for each country
country_wise_net_per_year_lr <- df %>% group_by(base_country_name, base_lat, base_long) %>% summarise(net_per_10K_2015 = sum(net_per_10K_2015), net_per_10K_2016 = sum(net_per_10K_2016), net_per_10K_2017 = sum(net_per_10K_2017), net_per_10K_2018 = sum(net_per_10K_2018), net_per_10K_2019 = sum(net_per_10K_2019))
# head(country_wise_net_per_year_lr)

country_wise_net_per_year_rf <- country_wise_net_per_year_lr

# Linear Regression
# Creating the Linear Model with the variables
country_linear_model <- lm(net_per_10K_2019 ~ net_per_10K_2015 + net_per_10K_2016 + net_per_10K_2017 + net_per_10K_2018, data = country_wise_net_per_year_lr)
# country_linear_model

# Renaming the columns for prediciton
country_wise_net_per_year_rf <- country_wise_net_per_year_lr
colnames(country_wise_net_per_year_lr)[5] = "net_per_10K_2015"
colnames(country_wise_net_per_year_lr)[6] = "net_per_10K_2016"
colnames(country_wise_net_per_year_lr)[7] = "net_per_10K_2017"
colnames(country_wise_net_per_year_lr)[8] = "net_per_10K_2018"

# Displaying the dataset
# head(country_wise_net_per_year_lr)


# Predicting the column for net per 10K for the year 2023
# (2020, 2021 and 2022 are considered COVID years. Migration analysis of years before 2020 are only considered to predict for the year 2023)

country_wise_net_per_year_lr$net_per_10K_2023 = predict(country_linear_model, newdata = country_wise_net_per_year_lr %>% select(5, 6, 7, 8))
# country_wise_net_per_year_lr

colnames(country_wise_net_per_year_lr)[5] = "net_per_10K_2016"
colnames(country_wise_net_per_year_lr)[6] = "net_per_10K_2017"
colnames(country_wise_net_per_year_lr)[7] = "net_per_10K_2018"
colnames(country_wise_net_per_year_lr)[8] = "net_per_10K_2019"

#head(country_wise_net_per_year_lr)

#########################################################################################################################################################################################

# Testing the Linear Model 

test_2019 <- predict(country_linear_model, newdata = country_wise_net_per_year_lr %>% select(4 ,5, 6, 7))
lm_model_results <- data.frame(year_2019 = country_wise_net_per_year_lr$net_per_10K_2019, test_2019)
head(lm_model_results)

summary(country_linear_model)

# (a) Prediction error, RMSE
RMSE(lm_model_results$test_2019, lm_model_results$year_2019)

# (b) R-square
R2(lm_model_results$test_2019, lm_model_results$year_2019)


#########################################################################################################################################################################################


# Plotting (Line graph) the yearwise net per 10K for each country 
Country = "Afghanistan"

country_wise_net_lr_country <- country_wise_net_per_year_lr %>%filter(base_country_name %in% Country)

country_wise_net_lr_country <- country_wise_net_lr_country %>% gather("Years", "Net_Per", c(net_per_10K_2016, net_per_10K_2017, net_per_10K_2018, net_per_10K_2019, net_per_10K_2023))

ggplot(country_wise_net_lr_country, aes(x = Years, y = Net_Per, group = 1)) + geom_line(color="red") + geom_point() + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



#########################################################################################################################################################################################


rf <- randomForest(net_per_10K_2019 ~ net_per_10K_2015 + net_per_10K_2016 + net_per_10K_2017 + net_per_10K_2018, data =  country_wise_net_per_year_rf, proximity = TRUE)
# rf

# head(country_wise_net_per_year_rf)

colnames(country_wise_net_per_year_rf)[5] = "net_per_10K_2015"
colnames(country_wise_net_per_year_rf)[6] = "net_per_10K_2016"
colnames(country_wise_net_per_year_rf)[7] = "net_per_10K_2017"
colnames(country_wise_net_per_year_rf)[8] = "net_per_10K_2018"

# head(country_wise_net_per_year_rf)

country_wise_net_per_year_rf$net_per_10K_2023 = predict(rf, newdata = country_wise_net_per_year_rf %>% select(5, 6, 7, 8))

# head(country_wise_net_per_year_rf)

colnames(country_wise_net_per_year_rf)[5] = "net_per_10K_2016"
colnames(country_wise_net_per_year_rf)[6] = "net_per_10K_2017"
colnames(country_wise_net_per_year_rf)[7] = "net_per_10K_2018"
colnames(country_wise_net_per_year_rf)[8] = "net_per_10K_2019"

#head(country_wise_net_per_year_rf)


#########################################################################################################################################################################################

# Testing the Random Forest Model 

test_2019_rf <- predict(rf, newdata = country_wise_net_per_year_rf %>% select(4 ,5, 6, 7))
rf_model_results <- data.frame(year_2019_rf = country_wise_net_per_year_rf$net_per_10K_2019, test_2019_rf)
head(rf_model_results)

# (a) Prediction error, RMSE
RMSE(rf_model_results$test_2019_rf, rf_model_results$year_2019_rf)

# (b) R-square
R2(rf_model_results$test_2019_rf, rf_model_results$year_2019_rf)

#########################################################################################################################################################################################


Country = "Afghanistan"

country_wise_net_rf_country <- country_wise_net_per_year_rf %>%filter(base_country_name %in% Country)
country_wise_net_rf_country <- country_wise_net_rf_country %>% gather("Years", "Net_Per", c(net_per_10K_2016, net_per_10K_2017, net_per_10K_2018, net_per_10K_2019, net_per_10K_2023))

ggplot(country_wise_net_rf_country, aes(x = Years, y = Net_Per, group = 1)) + geom_line(color="red") + geom_point() + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


#  The R2 Squared of Linear Regression model is 0.9235135, with RMSE = 9.302516 and the R2 Squared of Random Forest is 0.9632946 with RMSE = 8.102539. Clearly Random Forest is a better model for this dataset, hence we choose Random Forest for our predictions.


saveRDS(rf, "C:/Users/dhaval/Desktop/R/6600 - Visualization/project/app/www/countrymigration.rds")

#########################################################################################################################################################################################

#Industry Migration


country_wise_net_per_year_rf_industry = read_csv("C:/Users/dhaval/Desktop/R/6600 - Visualization/project/app/www/data/industry_migration.csv")

country_wise_net_per_year_rf_industry <- country_wise_net_per_year_rf_industry %>% group_by(country_name) %>% summarise(net_per_10K_2015 = sum(net_per_10K_2015), net_per_10K_2016 = sum(net_per_10K_2016), net_per_10K_2017 = sum(net_per_10K_2017), net_per_10K_2018 = sum(net_per_10K_2018), net_per_10K_2019 = sum(net_per_10K_2019))
#head(country_wise_net_per_year_rf_industry)

#head(country_wise_net_per_year_rf_industry)

rf <- randomForest(net_per_10K_2019 ~ net_per_10K_2015 + net_per_10K_2016 + net_per_10K_2017 + net_per_10K_2018, data =  country_wise_net_per_year_rf_industry, proximity = TRUE)

colnames(country_wise_net_per_year_rf_industry)[3] = "net_per_10K_2015"
colnames(country_wise_net_per_year_rf_industry)[4] = "net_per_10K_2016"
colnames(country_wise_net_per_year_rf_industry)[5] = "net_per_10K_2017"
colnames(country_wise_net_per_year_rf_industry)[6] = "net_per_10K_2018"

#head(country_wise_net_per_year_rf_industry)

country_wise_net_per_year_rf_industry$net_per_10K_2023 = predict(rf, newdata = country_wise_net_per_year_rf_industry %>% select(3, 4, 5, 6))

#head(country_wise_net_per_year_rf_industry)

colnames(country_wise_net_per_year_rf_industry)[3] = "net_per_10K_2016"
colnames(country_wise_net_per_year_rf_industry)[4] = "net_per_10K_2017"
colnames(country_wise_net_per_year_rf_industry)[5] = "net_per_10K_2018"
colnames(country_wise_net_per_year_rf_industry)[6] = "net_per_10K_2019"

#head(country_wise_net_per_year_rf_industry)

Country = "Belgium"

country_wise_net_rf_country_industry <- country_wise_net_per_year_rf_industry %>%filter(country_name %in% Country)
country_wise_net_rf_country_industry <- country_wise_net_rf_country_industry %>% gather("Years", "Net_Per", c(net_per_10K_2016, net_per_10K_2017, net_per_10K_2018, net_per_10K_2019, net_per_10K_2023))

ggplot(country_wise_net_rf_country_industry, aes(x = Years, y = Net_Per, group = 1)) + geom_line(color="red") + geom_point() + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


saveRDS(rf, "C:/Users/dhaval/Desktop/R/6600 - Visualization/project/app/www/industrymigration.rds")

#########################################################################################################################################################################################

#Skill Migration

country_wise_net_per_year_rf_skill = read_csv("C:/Users/dhaval/Desktop/R/6600 - Visualization/project/app/www/data/skill_migration.csv")
country_wise_net_per_year_rf_skill <- country_wise_net_per_year_rf_skill %>% group_by(country_name) %>% summarise(net_per_10K_2015 = sum(net_per_10K_2015), net_per_10K_2016 = sum(net_per_10K_2016), net_per_10K_2017 = sum(net_per_10K_2017), net_per_10K_2018 = sum(net_per_10K_2018), net_per_10K_2019 = sum(net_per_10K_2019))

#head(country_wise_net_per_year_rf_skill)

country_wise_net_per_year_rf_skill <- na.omit(country_wise_net_per_year_rf_skill)
rf <- randomForest(net_per_10K_2019 ~ net_per_10K_2015 + net_per_10K_2016 + net_per_10K_2017 + net_per_10K_2018, data =  country_wise_net_per_year_rf_skill, proximity = TRUE)

colnames(country_wise_net_per_year_rf_skill)[3] = "net_per_10K_2015"
colnames(country_wise_net_per_year_rf_skill)[4] = "net_per_10K_2016"
colnames(country_wise_net_per_year_rf_skill)[5] = "net_per_10K_2017"
colnames(country_wise_net_per_year_rf_skill)[6] = "net_per_10K_2018"

#head(country_wise_net_per_year_rf_skill)

country_wise_net_per_year_rf_skill$net_per_10K_2023 = predict(rf, newdata = country_wise_net_per_year_rf_skill %>% select(3, 4, 5, 6))

#head(country_wise_net_per_year_rf_skill)

colnames(country_wise_net_per_year_rf_skill)[3] = "net_per_10K_2016"
colnames(country_wise_net_per_year_rf_skill)[4] = "net_per_10K_2017"
colnames(country_wise_net_per_year_rf_skill)[5] = "net_per_10K_2018"
colnames(country_wise_net_per_year_rf_skill)[6] = "net_per_10K_2019"

#head(country_wise_net_per_year_rf_skill)

Country = "India"

country_wise_net_rf_country_skill <- country_wise_net_per_year_rf_skill %>%filter(country_name %in% Country)

country_wise_net_rf_country_skill <- country_wise_net_rf_country_skill %>% gather("Years", "Net_Per", c(net_per_10K_2016, net_per_10K_2017, net_per_10K_2018, net_per_10K_2019, net_per_10K_2023))

ggplot(country_wise_net_rf_country_skill, aes(x = Years, y = Net_Per, group = 1)) + geom_line(color="red") + geom_point() + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



saveRDS(rf, "C:/Users/dhaval/Desktop/R/6600 - Visualization/project/app/www/skillmigration.rds")
#########################################################################################################################################################################################