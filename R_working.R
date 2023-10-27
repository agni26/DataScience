# Packages ----------------------------------------------------------------
#install required packages
install.packages('scales')
install.packages('tidyverse')
install.packages('rgl')
install.packages('forecast')
library(scales)
library(tidyverse)
library(RColorBrewer)
library(rgl)
library(lubridate)
library(forecast)

#to switch off scientific notation (default is zero)
options(scipen=999)

# Data Pre-Processing -----------------------------------------------------

#Reading csv file
data <- read.csv('./owid-covid-data.csv')

#Creating Data set with all required variables
covid <- data.frame(
  continent = data$continent,
  location = data$location,
  date = data$date,
  total_cases = data$total_cases,
  new_cases = data$new_cases,
  new_cases_smoothed = data$new_cases_smoothed,
  total_deaths = data$total_deaths,
  new_deaths = data$new_deaths,
  new_tests = data$new_tests,
  new_deaths_smoothed = data$new_deaths_smoothed,
  population = data$population,
  people_vaccinated = data$people_vaccinated,
  reproduction_rate = data$reproduction_rate,
  stringency_index = data$stringency_index,
  positive_rate = data$positive_rate
)

#create new dataset by filtering main dataset using location and date
covid<-filter(covid, 
              (location=="India" |
               location=="Brazil" |
               location=="United States" |
               location=="Germany") & 
               date<"2023-01-01"
              ) 

#Date Formatting
covid <- covid %>% 
  mutate(date=ymd(covid$date))

#Creation of per population and year variable
covid <- covid %>% 
  mutate(
    new_cases_per_population=(new_cases_smoothed/population)*1000000,
    new_deaths_per_population=(new_deaths_smoothed/population)*1000000,
    year=format(date, "%Y")
    )

#Checking the dataset
head(covid)
tail(covid)

#Summarize variables
summary(covid)

# Visualizations ----------------------------------------------------------

#Line Plot - Comparing Positive rates of India and USA
ggplot(filter(covid, location=="India" | location=="United States"), 
                aes(color=location)) +
  geom_line(aes(x=date, y=positive_rate), size=1.2) + 
  labs(x = "\nYears\n", y = "Positive Rate \n", color = "Country \n",
       title = "Comparing Positive Rate of India and USA",
       subtitle = "Trend of positive covid cases per covid test\n") +
  theme_minimal()+
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust=0.5),
        plot.subtitle = element_text(color = "black", size = 14, face = "bold", hjust=0.5),
        legend.title = element_text(color = "black", size = 14, face = "bold"),
        legend.text = element_text(color = "black", size = 13, face = "italic"),
        strip.text.x = element_text(color = "black", size = 13),
        axis.title.x.bottom = element_text(color = "black", size = 13, face = "bold"),
        axis.title.y.left = element_text(color = "black", size = 13, face = "bold"),
        axis.text = element_text(color = "black", size = 12, face="bold"))

#Bar Chart - Compare New cases for all countries per year
ggplot(filter(covid, !is.na(new_cases)), 
       aes(fill=location, y=new_cases, x=year)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_colour_brewer(palette = "Dark2")+
  scale_y_continuous(breaks=c(200000,400000,600000,800000,1000000,1200000,1400000)) +
  labs(x = "\n Years", y = "Total Positive Cases \n", fill = "Country \n", 
       title = "Positive Covid-19 Cases Per Year", subtitle = "Yearly Aggregate of Covid cases in 2020, 2021 and 2022\n") +
  theme_minimal()+
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust=0.5),
        plot.subtitle = element_text(color = "black", size = 14, face = "bold", hjust=0.5),
        legend.title = element_text(color = "black", size = 14, face = "bold"),
        legend.text = element_text(color = "black", size = 13, face = "italic"),
        axis.title.x.bottom = element_text(color = "black", size = 13, face = "bold"),
        axis.title.y.left = element_text(color = "black", size = 13, face = "bold"),
        axis.text = element_text(color = "black", size = 12, face="bold"))

#Bar Chart - Compare New tests for all countries per year
ggplot(filter(covid, !is.na(new_tests)), 
       aes(fill=location, y=new_tests, x=year)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_colour_brewer(palette = "Dark2")+
  labs(x = "\n Years", y = "Total Tests \n", fill = "Country \n", 
       title = "Total Covid Tests Per Year", subtitle = "Yearly Aggregate of Covid tests in 2020, 2021 and 2022\n") +
  theme_minimal()+
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust=0.5),
        plot.subtitle = element_text(color = "black", size = 14, face = "bold", hjust=0.5),
        legend.title = element_text(color = "black", size = 14, face = "bold"),
        legend.text = element_text(color = "black", size = 13, face = "italic"),
        axis.title.x.bottom = element_text(color = "black", size = 13, face = "bold"),
        axis.title.y.left = element_text(color = "black", size = 13, face = "bold"),
        axis.text = element_text(color = "black", size = 12, face="bold"))

#Comparing New Deaths with Reproduction Rate of Virus for different Countries
ggplot( filter(covid, !is.na(reproduction_rate) & !is.na(new_deaths_per_population)) )+ 
    geom_bar(aes(x=date, y=new_deaths_per_population), stat="identity", colour="#F96167", size=1.2)+
    geom_line(aes(x=date, y=reproduction_rate), stat="identity", color="#4831D4", size=1.2) + 
    facet_wrap(~location) +
    labs(title= "Comparing New Deaths with Reproduction Rate of Virus for Different Countries\n",
         x="\nDate",y="New Deaths per Population\n" ) +
    theme_linedraw() +
    theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust=0.5),
          plot.subtitle = element_text(color = "black", size = 14, face = "bold", hjust=0.5),
          legend.title = element_text(color = "black", size = 14, face = "bold"),
          legend.text = element_text(color = "black", size = 13, face = "italic"),
          strip.text.x = element_text(color = "white", size = 13),
          axis.title.x.bottom = element_text(color = "black", size = 13, face = "bold"),
          axis.title.y.left = element_text(color = "black", size = 13, face = "bold"),
          axis.text = element_text(color = "black", size = 12, face="bold"))
  
#Comparing trends of new cases vs date
ggplot( filter(covid, !is.na(new_cases_smoothed)) )+ 
    geom_bar(aes(x=date, y=new_cases_smoothed), stat="identity", colour="#4831D4", size=1.2)+
    facet_grid(. ~ location) +
    labs(title= "Comparing New Cases \n", x="\nDate",y="New Cases")+
    theme_linedraw() +
    theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust=0.5),
          plot.subtitle = element_text(color = "black", size = 14, face = "bold", hjust=0.5),
          legend.title = element_text(color = "black", size = 14, face = "bold"),
          legend.text = element_text(color = "black", size = 13, face = "italic"),
          strip.text.x = element_text(color = "white", size = 13),
          axis.title.x.bottom = element_text(color = "black", size = 13, face = "bold"),
          axis.title.y.left = element_text(color = "black", size = 13, face = "bold"),
          axis.text = element_text(color = "black", size = 10, face="bold"))
  
#Comparing trends of Stringency Index with date
ggplot( filter(covid, !is.na(stringency_index)) ) + 
    geom_line(aes(x=date, y=stringency_index), stat="identity", colour="#00008b", size=1.2) +
    facet_grid(. ~ location) +
    labs(title= "Comparing Stringency Index \n", x="\nDate", y="Stringency Index") +
    theme_linedraw() +
    theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust=0.5),
          plot.subtitle = element_text(color = "black", size = 14, face = "bold", hjust=0.5),
          legend.title = element_text(color = "black", size = 14, face = "bold"),
          legend.text = element_text(color = "black", size = 13, face = "italic"),
          strip.text.x = element_text(color = "white", size = 13),
          axis.title.x.bottom = element_text(color = "black", size = 13, face = "bold"),
          axis.title.y.left = element_text(color = "black", size = 13, face = "bold"),
          axis.text = element_text(color = "black", size = 10, face="bold"))

#Pie Chart - Comparing Total Cases in Countries at the end of 2022
ggplot(filter(covid, date=="2022-12-31"),
       aes(x = "", y = total_cases, fill = location)) +
  geom_col(color = "black") +
  geom_label(aes(label = total_cases),
             color = "white",
             label.size = 1.2,
             position = position_stack(vjust = 0.5),
             show.legend = FALSE)+
  theme_void() +
  labs(fill = "Country \n",
       title = "Overall Covid Cases in Different Countries" ,
       subtitle = "Comparing total Cases by end of 2022") +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette="Dark2") +
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust=0.5),
        plot.subtitle = element_text(color = "black", size = 14, face = "bold", hjust=0.5),
        legend.title = element_text(color = "black", size = 14, face = "bold"),
        legend.text = element_text(color = "black", size = 13, face = "italic"),
        strip.text.x = element_text(color = "black", size = 13))

#Comparing total Cases in Countries at the end of 2022 with respect to population
ggplot(filter(covid, date=="2022-12-31"),
       aes(x = "", y = total_cases/population, fill = location)) +
  geom_col(color = "black") +
  geom_label(aes(label = round(total_cases*100/population, digits=2)),
             color = "white",
             label.size = 1.2,
             position = position_stack(vjust = 0.5),
             show.legend = FALSE)+
  theme_void() +
  labs(fill = "Country \n",
       title = "Percentage of population affected by covid" ,
       subtitle = "Comparing total Cases by end of 2022 with respect to Population") +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette="Dark2") +
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust=0.5),
        plot.subtitle = element_text(color = "black", size = 14, face = "bold", hjust=0.5),
        legend.title = element_text(color = "black", size = 14, face = "bold"),
        legend.text = element_text(color = "black", size = 13, face = "italic"),
        strip.text.x = element_text(color = "black", size = 13))

#Scatter plot - Comparing New Cases of India and USA with respect to New Deaths
ggplot( filter(covid, !is.na(total_deaths) & !is.na(new_cases))) +
  geom_point(aes(x=date, y=total_deaths, size=new_cases, color=location), alpha = 0.8) +
  labs(x = "\n Years", y = "Total Deaths \n", color = "Country \n", size = "New Cases \n",
       title = "Comparing Total Deaths and New Cases", subtitle = "Trend of Total Cases across 3 years along with New cases for Different Countries\n") +
  theme_minimal() +
  scale_color_brewer(palette="Set2") + 
  scale_size(range = c(1,10)) +
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust=0.5),
        plot.subtitle = element_text(color = "black", size = 14, face = "bold", hjust=0.5),
        legend.title = element_text(color = "black", size = 14, face = "bold"),
        legend.text = element_text(color = "black", size = 13, face = "italic"),
        strip.text.x = element_text(color = "black", size = 13),
        axis.title.x.bottom = element_text(color = "black", size = 13, face = "bold"),
        axis.title.y.left = element_text(color = "black", size = 13, face = "bold"),
        axis.text = element_text(color = "black", size = 12, face="bold"))

# Regression Model --------------------------------------------------------
#Making subset of data frame with required variables
covid_rgl <- data.frame(
  location = covid$location,
  date = covid$date,
  new_deaths = covid$new_deaths,
  people_vaccinated = covid$people_vaccinated,
  stringency_index = covid$stringency_index
)

#Filtering country and dates
covid_rgl <- filter(covid_rgl, location == "India" & date<"2023-01-01")

#to remove rows with NA values
covid_rgl <- na.exclude(covid_rgl)

#check the dataset
nrow(covid_rgl)

#ggpairs(data=covid_na, columns=3:5, title="Covid Data \n")

#splitting the data
covid_train <- covid_rgl[1:472,]
covid_test <- covid_rgl[473:674,]

#Exploring the dataset
covid_train[1:10,]
summary(covid_train)

cor.test(
  covid_train$stringency_index,
  covid_train$new_deaths
)

cor.test(
  covid_train$people_vaccinated,
  covid_train$new_deaths
)

# predicting new_deaths using stringency_index
mod_str <- lm(formula=new_deaths~stringency_index, data=covid_train)

summary(mod_str)
coef(mod_str)
coefs_str <- coef(mod_str)

# predicting new_deaths using people_vaccinated
mod_vac <- lm(formula=new_deaths~people_vaccinated, data=covid_train)

summary(mod_vac)
coef(mod_vac)
coefs_vac <- coef(mod_vac)

#Plot with Fit line
ggplot(
  data=covid_train,
  aes(x=stringency_index, y=new_deaths)
) +
  geom_point() +
  geom_abline(mapping=aes(
    slope=coefs_str["stringency_index"],
    intercept=coefs_str["(Intercept)"]
  ), color='red') +
  labs(x = "\nStringency index\n", y = "New Deaths\n",
       title = "Regression Line") +
  theme_minimal() + 
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust=0.5),
        axis.title.x.bottom = element_text(color = "black", size = 13, face = "bold"),
        axis.title.y.left = element_text(color = "black", size = 13, face = "bold"),
        axis.text = element_text(color = "black", size = 12, face="bold"))

ggplot(
  data=covid_train,
  aes(x=people_vaccinated, y=new_deaths)
) +
  geom_point() +
  geom_abline(mapping=aes(
    slope=coefs_vac["people_vaccinated"],
    intercept=coefs_vac["(Intercept)"]
  ), color='red') +
  labs(x = "\nPeople Vaccinated\n", y = "New Deaths\n",
       title = "Regression Line") +
  theme_minimal() + 
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust=0.5),
        axis.title.x.bottom = element_text(color = "black", size = 13, face = "bold"),
        axis.title.y.left = element_text(color = "black", size = 13, face = "bold"),
        axis.text = element_text(color = "black", size = 12, face="bold"))

# Residuals
covid_resid_str <- covid_train  # make a copy of the dataset, to leave the original untouched
covid_resid_str$predicted <- predict(mod_str)    # if data are not specified, uses the data the model was fit to
covid_resid_str$residuals <- residuals(mod_str)

# Residuals
covid_resid_vac <- covid_train  # make a copy of the dataset, to leave the original untouched
covid_resid_vac$predicted <- predict(mod_vac)    # if data are not specified, uses the data the model was fit to
covid_resid_vac$residuals <- residuals(mod_vac)

# show the data with predicted and residual values
covid_resid_str[1:10,]
covid_resid_vac[1:10,]

ggplot(
  data=covid_resid_str,
  aes(x=stringency_index,y=new_deaths)
) +
  geom_point(size=3) +  # make the actual values show up more clearly
  geom_point(size=2, aes(y=predicted), shape=1) +  # show the predicted values
  geom_segment(aes(xend=stringency_index, yend=predicted), alpha=0.9, color='red') +
  geom_abline(mapping=aes(
    slope=coefs_str["stringency_index"],
    intercept=coefs_str["(Intercept)"]
  ), color='gray') +
  labs(x = "Stringency index\n", y = "New deaths\n",
        title = "Stringency index with New Deaths with Fit Line") +
  theme_minimal() + 
  theme(plot.title = element_text(color = "black", size = 16, face = "bold", hjust=0.5),
        axis.title.x.bottom = element_text(color = "black", size = 13, face = "bold"),
        axis.title.y.left = element_text(color = "black", size = 13, face = "bold"))

ggplot(
  data=covid_resid_vac,
  aes(x=people_vaccinated,y=new_deaths)
) +
  geom_point(size=3) +  # make the actual values show up more clearly
  geom_point(size=2, aes(y=predicted), shape=1) +  # show the predicted values
  geom_segment(aes(xend=people_vaccinated, yend=predicted), alpha=0.9, color='red') +
  geom_abline(mapping=aes(
    slope=coefs_vac["people_vaccinated"],
    intercept=coefs_vac["(Intercept)"]
  ), color='gray') + 
  labs(x = "Years\n", y = "Positive Rate \n", color = "Country \n",
       title = "Comparing Positive Rate of India and USA",
       subtitle = "Trend of positive covid cases per covid test\n") +


# Multi-variate linear regression

ggplot(
  data=covid_train,
  aes(x=stringency_index, y=people_vaccinated)
) + geom_point()

cor.test(covid_train$stringency_index, covid_train$people_vaccinated)

plot3d(
  x=covid_train$stringency_index,
  y=covid_train$people_vaccinated,
  z=covid_train$new_deaths
)
rglwidget()  # this should open an interactive 3-d widget in the Viewer pane

# predicting time using both stringency_index AND people_vaccinated
mod <- lm(
  formula=new_deaths~stringency_index+people_vaccinated,  
  data=covid_train
)

summary(mod)
coefs <- coef(mod)

plot3d(
  x=covid_train$stringency_index,
  y=covid_train$people_vaccinated,
  z=covid_train$new_deaths,
  type='s', size=2, col='red'  # show the data points as big blue spheres for visibility
)

# this uses the model coefficients to plot the regression plane in 3-d space
planes3d( a=coefs["stringency_index"],
          b=coefs["people_vaccinated"],
          c=-1,
          d=coefs["(Intercept)"],
          col='steelblue')  
rglwidget()

predict(
  mod,
  newdata=covid_test
)

covid_multi_test <- covid_test 
covid_multi_test$predicted <- predict(mod, newdata=covid_multi_test)
covid_multi_test$residuals <- covid_multi_test$predicted - covid_multi_test$new_deaths
covid_multi_test

sse_multi <- sum(covid_multi_test$residuals**2)
sse_multi

layout(matrix(1:3, ncol=3))
plot(mod_str, which=1, main = "Stringency Index based model")      # Stringency Index`Based Model
plot(mod_vac, which=1, main = "People Vaccinated based model")     # People Vaccinated Based Model
plot(mod, which=1, main = "Multivariate based model")              # Multivariate Model

# Forecasting -------------------------------------------------------------

covid_ts <- data.frame(
  location = covid$location,
  date = covid$date,
  total_cases = covid$total_cases
)

covid_ts <- filter(covid_ts, location=="India")

covid_ts <- na.exclude(covid_ts)

x <- covid_ts$total_cases

# creating time series object
# from date 30 January, 2020
mts <- ts(x, start = decimal_date(ymd("2020-01-30")),
          frequency = 365.25 / 1)

# plotting the graph
plot(mts, xlab ="Daily Data",
     ylab ="Total cases",
     main ="COVID-19 Pandemic",
     col.main ="darkgreen")

# forecasting model using arima model
fit <- auto.arima(mts)

# Next 90 forecasted values
forecast(fit, 90)

# plotting the graph with next forecasted values
plot(forecast(fit, 90), xlab ="Daily Data",
     ylab ="Total Cases",
     main ="COVID-19 Pandemic", col.main ="darkgreen")

