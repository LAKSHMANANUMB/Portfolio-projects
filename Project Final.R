library(readxl)
library(plm)
library(car)       
library(lmtest)     
library(sandwich)
library(AER)
library(plm)
library(stargazer)
# creating a data frame to store the excel file

air_pollution<-read_excel("C:/Users/Akash/OneDrive/Desktop/Multivariate Staticts/final project/papers/Dataset-NCAP( 2013-2022).xlsx")
air_pollution<-data.frame(air_pollution)

# changing the first column name to state
colnames(air_pollution)[1] <- "state"
air_pollution

#checking for the dimensions,names,object size of the data frame
dim(air_pollution)
names(air_pollution)
object.size(air_pollution)

# Calculating the Mean for all the polluntants
avg_PM2.5<-mean(air_pollution$PM2.5)
avg_PM10<-mean(air_pollution$PM10)
avg_no2<-mean(air_pollution$NO2)
avg_so2<-mean(air_pollution$SO2)

# Calculating the Standard deviation for all the polluntants
sd_PM2.5<-sd(air_pollution$PM2.5)
sd_PM10<-sd(air_pollution$PM10)
sd_no2<-sd(air_pollution$NO2)
sd_so2<-sd(air_pollution$SO2)

# Calculating the Quantile range for all the polluntants
quantiles <- c(0.10, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9)

quant_PM2.5 <- quantile(air_pollution$PM2.5, quantiles)
quant_PM10 <- quantile(air_pollution$PM10, quantiles)
quant_no2 <- quantile(air_pollution$NO2, quantiles)
quant_so2 <- quantile(air_pollution$SO2, quantiles)

# gather everything in a data.frame 
DistributionSummary <- data.frame(Average = c(avg_PM2.5,avg_PM10,avg_no2,avg_so2), 
                                  StandardDeviation = c(sd_PM2.5,sd_PM10,sd_no2,sd_so2), 
                                  quantile = rbind(quant_PM2.5,quant_PM10,quant_no2,quant_so2))

DistributionSummary

# calculating the correlation of various pollunatnts
cor(air_pollution$PM2.5,air_pollution$PM10)
cor(air_pollution$NO2,air_pollution$SO2)


#Comparing Multiple Variables at once
cor(air_pollution[,c(4:7)])

#PM2.5 AND PM10 are highy correlated, so there is a strong relationship

#Ploting correlation between all the polluntants
install.packages("GGally")
GGally::ggpairs(air_pollution[,c(4:7)])

# Checking for multicollinearity
install.packages(car)
library(car)
model <- lm(PM2.5 ~ PM10 + NO2 + SO2, data = air_pollution)
vif(model)

#Plot the desnity plot for all the pollunatnts and deaths
plot(density(air_pollution$PM2.5))
plot(density(air_pollution$PM10))
plot(density(air_pollution$NO2))
plot(density(air_pollution$SO2))
plot(density(air_pollution$COPD.MAIN))
plot(density(air_pollution$LUNG.CANCER.MAIN))

#Calculate the average PM2.5 for each state
state_avg <- aggregate(PM2.5 ~ state, data = air_pollution,  mean)
barplot(state_avg$PM2.5, names.arg = state_avg$State, las = 2,
        col = "lightblue", main = "Average PM2.5 Levels by State",
        xlab = "State", ylab = "Average PM2.5")
safe_level <- 50
abline(h = safe_level, col = "blue", lwd = 2, lty = 2)

 #Calculating the average PM 10 for each state
state_avg <- aggregate(PM10 ~ state, data = air_pollution,  mean)
# Plot the average PM2.5 levels by state
barplot(state_avg$PM10, names.arg = state_avg$State, las = 2,
        col = "violet", main = "Average PM10 Levels by State",
        xlab = "State", ylab = "Average PM10")

# Create a high pollution variable based on PM2.5 level and plotting it through bar plot
air_pollution$high_pollution <- ifelse(air_pollution$PM2.5 > 100, "High", "Low")

high_pollution_states <- air_pollution[air_pollution$high_pollution == "High", ]
barplot(high_pollution_states$PM2.5,names.arg = high_pollution_states$state,
        col = "red", main = "High Pollution States (PM2.5 > 100)",
        xlab = "State", ylab = "PM2.5 Level", las = 2, cex.names = 0.7)
pairs(air_pollution[, c("PM2.5", "PM10", "NO2", "SO2")])

#################
#HYPOTHESIS TESTING 
#################

# Analysing the polluntant is normally distrubuted or not and check the variance using var.test

qqnorm(air_pollution$PM2.5)
qqline(air_pollution$PM2.5,col="red")

library(dplyr)
shapiro.test(air_pollution$PM2.5)

# Creating a subset before and after the implmentation of the program
before_ncap_pm10 <- air_pollution$PM10[air_pollution$Year < 2019 ]
after_ncap_pm10 <- air_pollution$PM10[air_pollution$Year >= 2019 ]
#Variance Test
var.test(before_ncap_pm10,after_ncap_pm10,data=air_pollution)
#t-test
t_test_pm10 <- t.test(before_ncap_pm10, after_ncap_pm10)
t_test_pm10

# So From The above Hypothesis we can conclude that the air pollution level of pm10 has been decreased after the
# The implmentation of NCAP program

# calculating the variance to check the variance is equally distrubuted or not

var.test(PM2.5~NCAP,data=air_pollution)
t.test(PM2.5~NCAP,data=air_pollution)

# From the above t-test we can conclude that  group 0(no ncap cities) has mean 62.64 and group 1(ncap cities)
# has a mean of 51.60 , and P value is less than 0.05 we can reject null hypothesis at cities with NCAP implementation 

#Hypothesis Analysis for Population Density vs NCAP

t_test_density <- t.test(`Population.Density` ~ NCAP, data = air_pollution)
print(t_test_density)
# The above t test suggest that the p value is less than 0.05 , so we can reject null hypothesis and we can
# conclude that the NCAP programme is implmenented in high population are , to reduce air pollution

# one-sided sample test for the air pollution level of pollutant PM 2.5 level of 10
t_test_PM25_WHO <- t.test(air_pollution$PM2.5, mu = 10)
print(t_test_PM25_WHO)

# Analysizing the p-value is very small , so we can reject null hypothesis, So we can conclude the PM2.5 levels are 
#much higher than the mean of 10 

before_ncap_pm10_chennai <- air_pollution$PM10[air_pollution$Year < 2019 & air_pollution$state == "Chennai"]
after_ncap_pm10_chennai <- air_pollution$PM10[air_pollution$Year >= 2019 & air_pollution$state== "Chennai"]

# Perform t-test
t_test_pm10_chennai <- t.test(before_ncap_pm10_chennai, after_ncap_pm10_chennai)

# Print the result
print(t_test_pm10_chennai)

#P value is less than 0.05, we can reject null hypothesis and there is significant difference after implmentation 
# especially chennai.

cor.test(air_pollution$Population.Density, air_pollution$NO2)
cor.test(air_pollution$Humidity_2m_.g.kg., air_pollution$NO2)
cor.test(air_pollution$Temperature_2m_.c., air_pollution$NO2)
cor.test(air_pollution$Windspeed_2m_.m.s., air_pollution$NO2)
#Humidity and temperature are important climatic factors influencing NO2 levels,
#with higher humidity and temperature associated with lower NO2 concentrations.


plot(air_pollution$PM2.5,air_pollution$LUNG.CANCER.MAIN,
     data=air_pollution,
     xlab= "PM2.5 Pollutant",
     ylab= "Lung Cancer Deaths",
     col="blue")

plot(air_pollution$City.Population,air_pollution$LUNG.CANCER.MAIN,
     data=air_pollution,
     xlab= "Population Density",
     ylab= "Lung Cancer Deaths",
     col="green2")

plot(air_pollution$Humidity_2m_.g.kg., air_pollution$NO2, 
     main = "Humidity vs NO2", 
     xlab = "Humidity (g/kg)", 
     ylab = "NO2 Levels")

plot(air_pollution$Temperature_2m_.c., air_pollution$NO2, 
     main = "Temperature vs NO2", 
     xlab = "Temperature (°C)", 
     ylab = "NO2 Levels",
     col="deeppink")

###############
#Regression Models
##############
model1 <- lm(PM2.5 ~  Temperature_2m_.c.*Humidity_2m_.g.kg. +
                      Population.Density * Area.of.city..miles.square. + 
                      PM10 +   
                      Population.Density * State.Population + 
                      Year + NCAP,
                    data = air_pollution)
summary(model1)
bptest(model1)
coeftest(model1, vcov = vcovHC, type = "HC1")

par(mfrow = c(2, 2)) 
plot(model1)


# The above model is used for checking what are the factors influenicng PM2.5 majorly and  polluntants. As Humidity,For each unit increase in humidity, 
#PM2.5 increases by 6.229.,and PM 10 is also highly significant  Higher PM10 levels lead to higher PM2.5 levels.
#And The next variable is if larger populations is there we will get higher PM2.5 levels
# Important note from the above analysis from the above regression analysis the NCAP programme implemented in the
#cities have lower PM2.5 levels on average( It has a negative coeffcient of -11.86)
#temperature and humidity impacts PM2.5 levels. As temperature and humidity increase together, the effect on PM2.5 is moderated.
#This interaction is significant (p-value = 0.000552), showing a meaningful relationship between population density and state population on PM2.5 levels.
# Temp and Humidity has strong interaction effect,his indicates that the relationship between temperature and PM2.5 depends on humidity levels.
# The model r^2 is also good, explaining 0.86 , it is good

# Model 2:

model2<-lm(NO2~Temperature_2m_.c.+Windspeed_2m_.m.s.+Humidity_2m_.g.kg.+Population.Density+SO2+
            NCAP+Population.Density *Area.of.city..miles.square.+Humidity_2m_.g.kg.*Temperature_2m_.c., 
           Windspeed_2m_.m.s. * Temperature_2m_.c.,data=air_pollution)
summary(model2)
bptest(model2)
coeftest(model2, vcov = vcovHC, type = "HC1")

par(mfrow = c(2, 2)) 
plot(model2)

# The above model is used for predicting what  are the factors which affect NO2 
# In my regression model, the state with high population density has high no2 levels, 
#because the population density p value is 0.0413 and positive relationship between SO2 and NO2 levels.
#NCAP policies are associated with a decrease in NO2 level has a  co-effcient on -11.02
#The coefficient is 0.00146, with a p-value of 0.0413, which is significant at the 5% level. 
# combined effect of temperature and humidity on NO2 is important and positive.
# And we can conlcude that weather plays a crucial role in determine NO2

#Model 3

model3<-lm(LUNG.CANCER.MAIN ~ PM2.5 + Population.Density + SO2 + Humidity_2m_.g.kg. + Tracheal..bronchus..and.lung.cancer_State.wise + COPD.MAIN + NCAP * PM2.5 + State.Population + City.Population + Area.of.city..miles.square.,data=air_pollution)

summary(model3)
bptest(model3)
coeftest(model3, vcov = vcovHC, type = "HC1")

par(mfrow = c(2, 2)) 
plot(model3,which =1)
plot(model3,which =2)
plot(model3,which =3)
plot(model3,which =4)

# As the above model has r^2 of 95 , the lung cancer deaths has been 
# explained by the above model. The cities with more population has 
#high lung cancer rates , the high rate of so2 may leads to significant health risk and the p value for s02 is very less. lung cancer is also dependent on the state level rates, because
#higher rates of tracheal, bronchus, and lung cancer at the state level are strongly related to lung cancer rates at the city level.
#Program is associated with lower lung cancer rates, likely due to air quality improvements.
#The PM2.5 on lung cancer rates depends on population density, i.e., the impact of pollution is more severe in densely populated areas.

###########################################
# Difference in Difference Model
###########################################
library(dplyr)
library(ggplot2)
library(lmtest)
library(sandwich)

# Creating a dummy variable if NCAP implmented means 1 or 0
air_pollution$NCAP_implemented <- ifelse(air_pollution$NCAP == 1, 1, 0)

# Creating  a time period of implementation of post NCAP after 2019
air_pollution$post_ncap <- ifelse(air_pollution$Year >= 2019, 1, 0)
air_pollution$post_ncap
cor(air_pollution$NCAP_implemented, air_pollution$post_ncap)


# Creating a interaction term for DID model
air_pollution$interaction_term <- air_pollution$NCAP_implemented * air_pollution$post_ncap

model_did <- lm(PM2.5 ~ NCAP_implemented + post_ncap  + Windspeed_2m_.m.s.*post_ncap + Population.Density * post_ncap +City.Population, data = air_pollution)

summary(model_did)

robust_se <- vcovHC(model_did, type = "HC1")

coeftest(model_did, vcov = robust_se)
#Plot
agg_data <- air_pollution %>%
  group_by(post_ncap, NCAP_implemented) %>%
  summarize(avg_pm25 = mean(PM2.5, na.rm = TRUE))

# Plot the results
ggplot(agg_data, aes(x = factor(post_ncap), y = avg_pm25, fill = factor(NCAP_implemented))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Post-NCAP Period (1 = After 2019, 0 = Before 2019)", 
       y = "Average PM2.5 Levels", 
       fill = "NCAP Implemented") +
  ggtitle("Average PM2.5 Levels by NCAP Implementation and Time Period") +
  theme_minimal()


par(mfrow = c(2, 2))
plot(model_did, which = 1)  
plot(model_did, which = 2)  
plot(model_did, which = 3)  
plot(model_did, which = 4) 

##################
# Difference in Difference model 2(w.r.t city and Year)
air_pollution$treatment_period <- ifelse(air_pollution$Year >= 2019, 1, 0)

did_model1 <- lm(PM2.5 ~ Year + state+ City.Population , data = air_pollution)

cor(air_pollution$NCAP, air_pollution$treatment_period)
robust_se <- vcovHC(did_model1, type = "HC1")
coeftest(did_model1, vcov = robust_se)


summary(did_model1)

library(ggplot2)

ggplot(air_pollution, aes(x = Year, y = PM2.5, color = factor(NCAP))) +
  geom_line() +
  labs(title = "PM2.5 Trends in NCAP vs Non-NCAP Cities",
       x = "Year",
       y = "PM2.5 Levels (µg/m³)") +
  theme_minimal()


did_model2 <- lm(PM10 ~ Year + state+ City.Population , data = air_pollution)

cor(air_pollution$NCAP, air_pollution$treatment_period)

summary(did_model2)
robust_se <- vcovHC(did_model2, type = "HC1")
coeftest(did_model2, vcov = robust_se)

library(ggplot2)

ggplot(air_pollution, aes(x = Year, y = PM10, color = factor(NCAP))) +
  geom_line() +
  labs(title = "PM10 Trends in NCAP vs Non-NCAP Cities",
       x = "Year",
       y = "PM10 Levels (µg/m³)") +
  theme_minimal()

################################
# Analyszing the polluntant level for the top most polluted states 
###############################
filtered_data_states <- subset(air_pollution, state %in% c("New Delhi", "Bangalore", "Mumbai", "Pune", "Lucknow"))

# NNCAP implmented after 2019 , so i created a dummy variable after 2019
filtered_data_states$post_ncap <- ifelse(filtered_data_states$Year >= 2019, 1, 0)

filtered_data_states$NCAP_implemented <- 1

filtered_data_states$NCAP_post <- filtered_data_states$NCAP_implemented * filtered_data_states$post_ncap

did_model3 <- lm(PM2.5 ~  post_ncap +Windspeed_2m_.m.s. + Population.Density + City.Population, data = filtered_data_states)

summary(did_model3)
robust_se <- vcovHC(did_model3, type = "HC1")
coeftest(did_model3, vcov = robust_se)
library(ggplot2)

# Ensure the state column has correct names
filtered_data_states$state <- factor(
  filtered_data_states$state, 
  levels = c("Bangalore", "Lucknow", "Mumbai", "New Delhi", "Pune")
)

# Create the combined plot
ggplot(filtered_data_states, aes(x = Year, y = PM2.5, color = as.factor(post_ncap))) +
  geom_line(size = 1) +
  labs(
    title = "PM 2.5 Levels Before and After NCAP Implementation",
    x = "Year",
    y = "PM2.5 Levels",
    color = "Post NCAP"
  ) +
  theme_minimal() +
  facet_wrap(~ state, scales = "free_y") +
  theme(strip.text = element_text(size = 12, face = "bold"))



##################################
# Panel Data Analaysis
###################################
library(plm)
library(car)       
library(lmtest)     
library(sandwich)   

names(air_pollution)
model4 <- plm(LUNG.CANCER.MAIN ~ PM2.5 + Population.Density + 
               Tracheal..bronchus..and.lung.cancer_State.wise + NCAP + City.Population + 
               Area.of.city..miles.square. + PM2.5 * Population.Density,
             data = air_pollution, 
             effect = "twoways", 
             model = "within", 
             index = c("state", "Year"))


bptest(model4)
summary(model4, robust = TRUE)
coeftest(model4, vcov = vcovHC(model4, type = "HC1"))



model5 <- plm(COPD.MAIN ~ PM2.5 * PM10 + Population.Density + 
                       Tracheal..bronchus..and.lung.cancer_State.wise + NCAP + 
                       City.Population + Area.of.city..miles.square. + 
                       PM2.5 * Population.Density, 
                     data = air_pollution, 
                     effect = "twoways", 
                     model = "within", 
                     index = c("state", "Year"))


summary(model5)
bptest(model5)
summary(model5, robust = TRUE)
coeftest(model5, vcov = vcovHC(model5, type = "HC1")) 


model6 <- plm(PM2.5 ~ Temperature_2m_.c.+ Humidity_2m_.g.kg. + Population.Density+ NCAP + City.Population + Area.of.city..miles.square., 
                       data = air_pollution,
                       index = c("state", "Year"),
                       model = "within",  
                       effect = "twoways") 
summary(model6)
bptest(model6)
summary(model6, robust = TRUE)
coeftest(model6, vcov = vcovHC, type = "HC1")


#############################
# Analysing Different types of cases for the polluntant of PM2.5
#############################

# Creating a Dummy Variable for the PM 2.5 , If It is greater than 50 means high pollution 1
air_pollution$high_pollution <- ifelse(air_pollution$PM2.5 > 50, 1, 0)  
air_pollution$low_pollution <- ifelse(air_pollution$PM2.5 <= 50, 1, 0)  

air_pollution$NCAP_implemented <- ifelse(air_pollution$NCAP == 1, 1, 0)

# Analyzing The effect of the center period of my analysis, because from 2013 to 2022, I am Taking only these
# Two years and analysing the effect
air_pollution_2015_2018 <- air_pollution[air_pollution$Year %in% c(2015, 2016, 2017, 2018), ]

model7 <- plm(LUNG.CANCER.MAIN ~ PM2.5 + Population.Density + 
                Tracheal..bronchus..and.lung.cancer_State.wise + NCAP + City.Population + 
                Area.of.city..miles.square. + PM2.5 * Population.Density,
              data = air_pollution_2015_2018, 
              effect = "twoways", 
              model = "within", 
              index = c("state", "Year"))

model8<- plm(COPD.MAIN ~ PM2.5 * PM10 + Population.Density + 
                Tracheal..bronchus..and.lung.cancer_State.wise + NCAP + 
                City.Population + Area.of.city..miles.square. + 
                PM2.5 * Population.Density, 
              data = air_pollution_2015_2018, effect = "twoways", 
              model = "within", index = c("state", "Year"))


model9 <- plm(PM2.5 ~ NCAP_implemented + post_ncap + Tracheal..bronchus..and.lung.cancer_State.wise + 
                Chronic.obstructive.pulmonary.disease_State.wise, 
              data = air_pollution_2015_2018, model = "within", effect = "twoways")


model10 <- plm(PM2.5 ~ NCAP_implemented + post_ncap + State.Population + Population.Density + 
                 Area.of.city..miles.square. + high_pollution + low_pollution, 
               data = air_pollution_2015_2018, model = "within", effect = "twoways")


 # Including additional pollution categories
model11 <- plm(PM2.5 ~ Windspeed_2m_.m.s. + Population.Density + NCAP_implemented + high_pollution + low_pollution,
              data = air_pollution,
              index = c("state", "Year"),
              model = "within",
              effect = "twoways")


air_pollution$After_2019 <- ifelse(air_pollution$Year > 2019, 1, 0)
air_pollution_after_2019 <- subset(air_pollution, After_2019 == 1)
# Compute average PM2.5 for states with and without NCAP implemented
average_PM25_by_NCAP <- aggregate(PM2.5 ~ NCAP + state, data = air_pollution_after_2019, mean)
colnames(average_PM25_by_NCAP) <- c("NCAP_Implemented", "State", "Average_PM25")
print(average_PM25_by_NCAP)

# Visualize the differences using a bar plot
library(ggplot2)
ggplot(average_PM25_by_NCAP, aes(x = State, y = Average_PM25, fill = as.factor(NCAP_Implemented))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average PM2.5 Levels by State (After 2019)",
       x = "State", y = "Average PM2.5",
       fill = "NCAP Implemented") +
  theme_minimal()
##########################################################################

# Implementing through Stargazer package
library(stargazer)

model1 <- lm(PM2.5 ~ Population.Density + 
               Population.Density * Area.of.city..miles.square. + 
               PM10 + NO2 +  
               Population.Density * State.Population + 
               Year + NCAP,
             data = air_pollution)

model2<-lm(NO2~Population.Density+SO2+
             NCAP+Population.Density *Area.of.city..miles.square., 
           ,data=air_pollution)

model3<-lm(LUNG.CANCER.MAIN ~ PM2.5 + Population.Density + SO2  + Tracheal..bronchus..and.lung.cancer_State.wise + COPD.MAIN + NCAP * PM2.5 + State.Population + City.Population + Area.of.city..miles.square.,data=air_pollution)

model_did <- lm(PM2.5 ~ NCAP_implemented + post_ncap + City.Population+Population.Density + PM10, data = air_pollution)

did_model1 <- lm(PM2.5 ~  Year + state+ City.Population , data = air_pollution)

did_model2 <- lm(PM10 ~ Year + state+ City.Population , data = air_pollution)

did_model3 <- lm(PM2.5 ~  post_ncap  + Population.Density + City.Population, data = filtered_data_states)

model4 <- plm(LUNG.CANCER.MAIN ~ PM2.5 + Population.Density + 
                Tracheal..bronchus..and.lung.cancer_State.wise + NCAP + City.Population + 
                Area.of.city..miles.square. + PM2.5 * Population.Density,
              data = air_pollution, 
              effect = "twoways", 
              model = "within", 
              index = c("state", "Year"))

model5 <- plm(COPD.MAIN ~ PM2.5 * PM10 + Population.Density + 
                Tracheal..bronchus..and.lung.cancer_State.wise + NCAP + 
                City.Population + Area.of.city..miles.square. + 
                PM2.5 * Population.Density, 
              data = air_pollution, 
              effect = "twoways", 
              model = "within", 
              index = c("state", "Year"))

model6 <- plm(PM2.5 ~ NCAP_implemented, data = air_pollution, model = "within", effect = "individual")

model7 <- plm(LUNG.CANCER.MAIN ~ PM2.5 + Population.Density + 
                Tracheal..bronchus..and.lung.cancer_State.wise + NCAP + City.Population + 
                Area.of.city..miles.square. + PM2.5 * Population.Density,
              data = air_pollution_2015_2018, 
              effect = "twoways", 
              model = "within", 
              index = c("state", "Year"))

model8<- plm(COPD.MAIN ~ PM2.5 * PM10 + Population.Density + 
               Tracheal..bronchus..and.lung.cancer_State.wise + NCAP + 
               City.Population + Area.of.city..miles.square. + 
               PM2.5 * Population.Density, 
             data = air_pollution_2015_2018, effect = "twoways", 
             model = "within", index = c("state", "Year"))


model9 <- plm(PM2.5 ~ NCAP_implemented + post_ncap + Tracheal..bronchus..and.lung.cancer_State.wise + 
                Chronic.obstructive.pulmonary.disease_State.wise, 
              data = air_pollution_2015_2018, model = "within", effect = "twoways")


model10 <- plm(PM2.5 ~ NCAP_implemented + post_ncap + State.Population + Population.Density + 
                 Area.of.city..miles.square. + high_pollution + low_pollution, 
               data = air_pollution_2015_2018, model = "within", effect = "twoways")


model11 <- plm(PM2.5 ~ Windspeed_2m_.m.s. + Population.Density + NCAP_implemented + high_pollution + low_pollution,
               data = air_pollution,
               index = c("state", "Year"),
               model = "within",
               effect = "twoways")

rob_se <- list(sqrt(diag(vcovHC(model1, type = "HC1"))),
               sqrt(diag(vcovHC(model2, type = "HC1"))),
               sqrt(diag(vcovHC(model3, type = "HC1"))),
               sqrt(diag(vcovHC(model_did, type = "HC1"))),
               sqrt(diag(vcovHC(did_model1, type = "HC1"))),
               sqrt(diag(vcovHC(did_model2, type = "HC1"))),
               sqrt(diag(vcovHC(did_model3, type = "HC1"))),
               sqrt(diag(vcovHC(model4, type = "HC1"))),
               sqrt(diag(vcovHC(model5, type = "HC1"))),
               sqrt(diag(vcovHC(model6, type = "HC1"))),
               sqrt(diag(vcovHC(model7, type = "HC1"))),
               sqrt(diag(vcovHC(model8, type = "HC1"))),
               sqrt(diag(vcovHC(model9, type = "HC1"))),
               sqrt(diag(vcovHC(model10, type = "HC1"))),
               sqrt(diag(vcovHC(model11, type = "HC1"))))

stargazer(model1,model2,model3,se=rob_se,type="html",title="Linear Regression Models",out="123.html")
stargazer(model4,model5,model6,se=rob_se,type="html",title="Panel Data Model for Lung cancer deaths,COPD Cases and PM2.5 Pollutant",out="123.html")
stargazer(model_did,did_model1,did_model2,did_model3,se=rob_se,type="html",title="Difference in Difference models For PM2.5 AND PM10",out="123.html")
stargazer(model7,model8 ,model9,type = "html",se=rob_se, title = "Regression Results for Lung Cancer,COPD Cases,PM2.5 Pollunatnt From 2015 - 2018",out="123.html")


####################################################
# generate another table with correct R2 and adj R2
####################################################
model41 <- plm(LUNG.CANCER.MAIN ~ PM2.5 + Population.Density + 
                Tracheal..bronchus..and.lung.cancer_State.wise + NCAP + City.Population + 
                Area.of.city..miles.square. + PM2.5 * Population.Density,
              data = air_pollution, 
              effect = "twoways", 
              model = "pool", 
              index = c("state", "Year"))

model51 <- plm(COPD.MAIN ~ PM2.5 * PM10 + Population.Density + 
                Tracheal..bronchus..and.lung.cancer_State.wise + NCAP + 
                City.Population + Area.of.city..miles.square. + 
                PM2.5 * Population.Density, 
              data = air_pollution, 
              effect = "twoways", 
              model = "pool", 
              index = c("state", "Year"))

model61 <- plm(PM2.5 ~ NCAP_implemented, data = air_pollution, model = "pool", effect = "individual")

model71 <- plm(LUNG.CANCER.MAIN ~ PM2.5 + Population.Density + 
                Tracheal..bronchus..and.lung.cancer_State.wise + NCAP + City.Population + 
                Area.of.city..miles.square. + PM2.5 * Population.Density,
              data = air_pollution_2015_2018, 
              effect = "twoways", 
              model = "pool", 
              index = c("state", "Year"))

model81<- plm(COPD.MAIN ~ PM2.5 * PM10 + Population.Density + 
               Tracheal..bronchus..and.lung.cancer_State.wise + NCAP + 
               City.Population + Area.of.city..miles.square. + 
               PM2.5 * Population.Density, 
             data = air_pollution_2015_2018, effect = "twoways", 
             model = "pool", index = c("state", "Year"))


model91 <- plm(PM2.5 ~ NCAP_implemented + post_ncap + Tracheal..bronchus..and.lung.cancer_State.wise + 
                Chronic.obstructive.pulmonary.disease_State.wise, 
              data = air_pollution_2015_2018, model = "pool", effect = "twoways")


model101 <- plm(PM2.5 ~ NCAP_implemented + post_ncap + State.Population + Population.Density + 
                 Area.of.city..miles.square. + high_pollution + low_pollution, 
               data = air_pollution_2015_2018, model = "pool", effect = "twoways")


model111 <- plm(PM2.5 ~ Windspeed_2m_.m.s. + Population.Density + NCAP_implemented + high_pollution + low_pollution,
               data = air_pollution,
               index = c("state", "Year"),
               model = "within",
               effect = "twoways")


stargazer(model41,model51,model61,se=rob_se,type="html",title="Panel Data Model for Lung cancer deaths,COPD Cases and PM2.5 Pollutant",out="123.html")
stargazer(model71,model81 ,model91,type = "html",se=rob_se, title = "Regression Results for Lung Cancer,COPD Cases,PM2.5 Pollunatnt From 2015 - 2018",out="123.html")




