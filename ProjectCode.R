library(ggplot2)
library(dplyr)
library(tidyverse)
library(cluster)
library(lubridate)
library(psych)
library(viridis)
library(ggpointdensity)
library(writexl)

traffic <- read.csv("Metro_Interstate_Traffic_Volume.csv")
traffic2 <- read.csv("Metro_Interstate_Traffic_Volume.csv")
#second dataframe to explore relationships/potential interesting values
#but not actually "clean" the dataframe. Will clean only original traffic
#data and use that dataframe to import to python for analysis. Certain relationships
#created variables only valuable for observation not the actual analysis. 
#also dummy coding in python as it's easier/more intuitive in my opinion

#converting kelvin to celsius for easier interpretation 
traffic <- traffic %>%
   mutate(temp = (temp - 273.15))

#boxplots to identify any extraneous outliers
#checking mostly to see if there are any errors, not actual observed outliers
ggplot(traffic, aes(x = temp)) +
   geom_boxplot()

ggplot(traffic, aes(x = rain_1h)) +
   geom_boxplot()

ggplot(traffic, aes(x = snow_1h)) +
   geom_boxplot()

ggplot(traffic, aes(x = traffic_volume)) +
   geom_boxplot()

#obvious error observations in rain, temp categories that need to be removed
traffic <- subset(traffic, rain_1h < 1000 & temp > -100)

describe(traffic)

#writing cleaned traffic df to excel to use as final for python input 
write.csv(traffic,"C:\\Users\\cav52\\Documents\\R\\SDS322\\FinalTraffic.csv", row.names = FALSE)

##TRAFFIC 2

#separating out date/time variable for relationship comparison. Using R here 
#and preserving original date/time in first traffic df as python has an easy
#parsing method to split the variables so will import first df and use traffic2 for analysis

#creating weekday variable
traffic2 <- traffic2 %>% 
   separate(date_time, sep = " ", into = c("date", "time")) 
traffic2$weekday <- weekdays(as.Date(traffic2$date))

#additional separating
traffic2 <- traffic2 %>%
   separate(date, sep = "-", into = c("year", "month", "day")) 

#removing repetitive weather_description column as weather_main already accounts for this
traffic2 <- traffic2 %>%
   select(-weather_description) %>%
   separate(time, sep = ":", into = c("hour", "delete", "none")) 

#removing extra minutes/seconds columns/holiday column
traffic2 <- traffic2 %>%
   select(-c(delete, none))

#converting kelvin to celsius for easier interpretation 
traffic2 <- traffic2 %>%
   mutate(temp = (temp - 273.15))


#traffic volume histogram
barcol <- "#3399FF"
barlinecol <- "#000000"
ggplot(traffic2, aes(x = traffic_volume)) +
   geom_histogram(fill = barcol, colour = barlinecol) +
   scale_x_continuous(name = "Traffic Volume") +
   scale_y_continuous(name = "Frequency") +
   theme_bw()

#weather histogram
ggplot(data = traffic2, aes(x = weather_main)) +
   geom_histogram(stat = "count", fill = barcol) +
   scale_y_continuous(name = "Frequency") +
   ggtitle("Frequency by Weather Type") +
   theme(plot.title = element_text(hjust = 0.5),
         axis.title.x = element_blank())

#charting average traffic volume by month
mean_month <- traffic2 %>% 
   group_by(month) %>%
   summarize(mean = mean(traffic_volume))

barcol2 <- "#00CCCC"
ggplot(data = mean_month, mapping = aes(x = month, y = mean)) +
   geom_bar(stat = "identity", fill = barcol2, colour = barlinecol) +
   scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May",
                               "Jun", "Jul", "Aug", "Sep", "Oct",
                               "Nov", "Dec")) +
   scale_y_continuous(name = "Average Traffic Volume") +
   ggtitle("Traffic Levels per Month") +
   theme(plot.title = element_text(hjust = 0.5),
         axis.title.x = element_blank())


#charting average traffic volume by year
mean_year <- traffic2 %>%
   group_by(year) %>%
   summarize(mean = mean(traffic_volume))

barcol3 <- "#3399FF"
ggplot(data = mean_year, aes(x = year, y = mean)) +
   geom_bar(stat = "identity", fill = barcol3, colour = barlinecol) +
   scale_y_continuous(name = "Average Traffic Volume") +
   theme(axis.title.x = element_blank(),
         plot.title = element_text(hjust = 0.5)) +
   ggtitle("Traffic Levels per Year") 


#charting average traffic volume by day
mean_day <- traffic2 %>%
   group_by(weekday) %>%
   summarize(mean = mean(traffic_volume))
mean_day <- mean_day %>%
   arrange(factor(weekday, levels = c('Monday','Tuesday','Wednesday', 'Thursday',
                                      'Friday', 'Saturday', 'Sunday')))
barcol4 <- "#CCCFFF"
ggplot(data = mean_day, mapping = aes(x = weekday, y = mean)) +
   geom_bar(stat = "identity", fill = barcol4, colour = barlinecol) +
   scale_x_discrete(limits = c("Monday", "Tuesday", "Wednesday",
                               "Thursday", "Friday", "Saturday", "Sunday")) +
   scale_y_continuous(name = "Average Traffic Volume") +
   ggtitle("Traffic Levels by Weekday") +
   theme(plot.title = element_text(hjust = 0.5),
         axis.title.x = element_blank())


#charting average traffic volume by weather main
mean_weather <- traffic2 %>%
   group_by(weather_main) %>%
   summarize(mean = mean(traffic_volume))

barcol5 <- "#66CCCC"
ggplot(data = mean_weather, mapping = aes(x = weather_main, y = mean)) +
   geom_bar(stat = "identity", fill = barcol5, colour = barlinecol) +
   scale_y_continuous(name = "Average Traffic Volume") +
   ggtitle("Traffic Levels by Weather Occurence") +
   theme(plot.title = element_text(hjust = 0.5),
         axis.title.x = element_blank())


#charting ATV by hour
mean_hour <- traffic2 %>%
   group_by(hour) %>%
   summarize(mean = mean(traffic_volume))

barcol6 <- "#6666FF"
ggplot(data = mean_hour, mapping = aes(x = hour, y = mean)) +
   geom_bar(stat = "identity", fill = barcol6, colour = barlinecol) +
   scale_y_continuous(name = "Average Traffic volume") +
   ggtitle("Traffic Levels by Hour of Day") +
   theme(plot.title = element_text(hjust = 0.5),
         axis.title.x = element_blank())


#examining snow/rain variables
ggplot(traffic, aes(x = snow_1h)) +
   geom_histogram()
ggplot(traffic, aes(x = rain_1h)) +
   geom_histogram(binwidth = 2)

#both rain and snow were heavily skewed so figured best to compare conditions
#when it did rain or snow and see any difference

#rain to traffic volume
traffic2$rain_yes <- ifelse(traffic2$rain_1h > 0, "Yes", "No")
ggplot(traffic2, aes(x = rain_yes, y = traffic_volume)) +
   geom_violin() +
   geom_boxplot(width = .2) +
   scale_y_continuous(name = "Traffic Volume") +
   scale_x_discrete(name = "Rain?") 

#snow to traffic volume
traffic2$snow_yes <- ifelse(traffic2$snow_1h > 0, "Yes", "No")
ggplot(traffic2, aes(x = snow_yes, y = traffic_volume)) +
   geom_violin() +
   geom_boxplot(width = .2) +
   scale_y_continuous(name = "Traffic Volume") +
   scale_x_discrete(name = "Snow?") 

#temp to traffic volume
ggplot(traffic, aes(x = temp, y = traffic_volume)) +
   geom_pointdensity(alpha = .5) +
   scale_color_viridis()

#clouds to traffic volume
mean_clouds <- traffic %>%
   group_by(clouds_all) %>%
   summarize(mean = mean(traffic_volume))

barcol7 <- "#3333CC"
ggplot(data = mean_clouds, mapping = aes(x = clouds_all , y = mean)) +
   geom_bar(stat = "identity", fill = barcol7, colour = barlinecol) +
   scale_y_continuous(name = "Average Traffic volume") +
   scale_x_discrete(labels= c("10", "20", "30", "40", "50", "60", "70", "80", "90"), name = "Percentage of Cloud Cover (0 to 100)")

#correlation matrix for checking if numeric variables have relevant correlations
cormatrix <- traffic2 %>% select_if(is.numeric) %>% cor(use = "pair")

tidymat <- cormatrix %>% as.data.frame %>% rownames_to_column("var1") %>%
   pivot_longer(-1, names_to = "var2", values_to = "correlation")

tidymat %>% ggplot(aes(var1, var2, fill = correlation))+
   geom_tile() +
   geom_text(aes(label= round(correlation,2)), color = "black", size = 5) +
   coord_fixed() +
   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
   scale_fill_gradient2(low = "yellow", mid = "orange", high = "red")

