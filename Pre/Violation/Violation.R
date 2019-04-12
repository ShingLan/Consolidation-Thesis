# Read Data
library(tidyverse)
library(dplyr)
violation_ground_38Y <- data.frame(read_csv('Violation_GW_38Y.csv'))
violation_surface_38Y <- data.frame(read_csv('Violation_SW_38Y.csv'))
violation_ground_5Y <- data.frame(read_csv('Violation_GW_5Y.csv'))
violation_surface_5Y <- data.frame(read_csv('Violation_SW_5Y.csv'))
population <- data.frame(read_csv('Population.csv'))
population <- population[,-1]
population <- population[,-3]
population <- population[,-3]
names(population) <- c("region","population")
violation_38Y <- rbind(violation_ground_38Y,violation_surface_38Y)
planes_38Y <- group_by(violation_38Y, Primacy.Agency)
violation_s_38Y <- summarise(planes_38Y,count = n())
colnames(violation_s_38Y) <- c("region","number")
violation_s_38Y$number <- violation_s_38Y$number/38
violation_5Y <- rbind(violation_ground_5Y,violation_surface_5Y)
planes_5Y <- group_by(violation_5Y, Primacy.Agency)
violation_s_5Y <- summarise(planes_5Y,count = n())
colnames(violation_s_5Y) <- c("region","number")
violation_s_5Y$number <- violation_s_5Y$number/5

# Heat Map for Violations From 1980
library(maps)
library(mapproj)
library(ggthemes)
library(ggplot2)
us_states <- map_data("state")
violation_s_38Y$region <- tolower(violation_s_38Y$region)
violation_amount <- function(x){
  y <- ifelse(0 <= x & x <=100, "0-100",
              ifelse(100 < x & x <=200, "100-200",
                     ifelse(200 < x & x <=300, "200-300",
                            ifelse(300 < x & x <=400, "300-400",
                                   ifelse(400 < x & x <=500, "400-500",
                                          ifelse(500 < x & x <=600, "500-600", "600-700"))))))
  return(y)
}
for (i in c(1:nrow(violation_s_38Y))){
  violation_s_38Y$amount.range[i] <- violation_amount(violation_s_38Y$number[i])
}
us_states_n_38Y <- left_join(us_states, violation_s_38Y)
ggplot(data = us_states_n_38Y, mapping = aes(x = long, y = lat, group = group, fill = amount.range)) + geom_polygon(color = "grey90", size = 0.1) + coord_map(projection = "albers", lat0 = 39, lat1 = 45) + labs(fill = "Amount") + theme_map() + labs(title="Average Health-based Violations Each Year From 1980") + theme(plot.title = element_text(hjust = 0.5,lineheight=.8, face="bold"))+ scale_fill_discrete(limits = c('0-100','100-200','200-300','300-400','400-500','500-600','600-700')) 


# Heat Map for Violations From 2014
violation_s_5Y$region <- tolower(violation_s_5Y$region)
violation_amount_5Y <- function(x){
  y <- ifelse(0 <= x & x <=100, "0-100",
            ifelse(100 <x & x <=200, "100-200",
                      ifelse(200 < x & x <=300, "200-300",
                             ifelse(300 < x & x <=400, "300-400",
                                  ifelse(400 < x & x <=500, "400-500",
                                     ifelse(500 <x & x <=1500, "500-1500","1500-2500"))))))
  return(y)
}
for (i in c(1:nrow(violation_s_5Y))){
  violation_s_5Y$amount.range[i] <- violation_amount_5Y(violation_s_5Y$number[i])
}
us_states_n_5Y <- left_join(us_states, violation_s_5Y)
ggplot(data = us_states_n_5Y, mapping = aes(x = long, y = lat, group = group, fill = amount.range)) + geom_polygon(color = "grey90", size = 0.1) + coord_map(projection = "albers", lat0 = 39, lat1 = 45) + labs(fill = "Amount") + theme_map() + labs(title="Average Health-based Violations Each Year From 2014") + theme(plot.title = element_text(hjust = 0.5,lineheight=.8, face="bold")) + scale_fill_discrete(limits = c('0-100','100-200','200-300','300-400','400-500','500-1500','1500-2500'))


# Heat Map for Violations Per Person From 1980
population$region <- tolower(population$region)
violation_aver_38Y <- inner_join(population, violation_s_38Y, by = "region")
violation_aver_38Y$violation.person <- violation_aver_38Y$number/violation_aver_38Y$population
average_38Y <- function(x){
  y <- ifelse(5e-07 <= x & x <=2.5e-06, "5e-07 - 2.5e-06",
              ifelse(2.5e-06 <x & x <=5e-06, "2.5e-06 - 5e-06",
                     ifelse(5e-06 <x & x <=1e-05, "5e-06 - 1e-05",
                            ifelse(1e-05 <x & x <=5e-05, "1e-05 - 5e-05",
                                   ifelse(5e-05 <x & x <=1e-04, "5e-05 - 1e-04", "1e-04 - 1.5e-04")))))
  return(y)
}
for (i in c(1:nrow(violation_aver_38Y))){
  violation_aver_38Y$aver.range[i] <- average_38Y(violation_aver_38Y$violation.person[i])
}
us_states_aver_38Y <- left_join(us_states, violation_aver_38Y)
ggplot(data = us_states_aver_38Y, mapping = aes(x = long, y = lat, group = group, fill = aver.range)) + geom_polygon(color = "grey90", size = 0.1) + coord_map(projection = "albers", lat0 = 39, lat1 = 45) + labs(fill = "Amount") + theme_map() + labs(title="Health-based Violations Per Person Each Year From 1980") + theme(plot.title = element_text(hjust = 0.5,lineheight=.8, face="bold")) + scale_fill_discrete(limits = c('5e-07 - 2.5e-06','2.5e-06 - 5e-06','5e-06 - 1e-05','1e-05 - 5e-05','5e-05 - 1e-04','1e-04 - 1.5e-04'))


# Heat Map for Violations Per Person From 2014
violation_aver_5Y <- inner_join(population, violation_s_5Y, by = "region")
violation_aver_5Y$violation.person <- violation_aver_5Y$number/violation_aver_5Y$population
average_5Y <- function(x){
  y <- ifelse(5e-07 <= x & x <=2.5e-06, "5e-07 - 2.5e-06",
              ifelse(2.5e-06 <x & x <=5e-06, "2.5e-06 - 5e-06",
                     ifelse(5e-06 <x & x <=1e-05, "5e-06 - 1e-05",
                            ifelse(1e-05 <x & x <=5e-05, "1e-05 - 5e-05",
                                   ifelse(5e-05 <x & x <=1e-04, "5e-05 - 1e-04", "1e-04 - 5e-04")))))
  return(y)
}
for (i in c(1:nrow(violation_aver_5Y))){
  violation_aver_5Y$aver.range[i] <- average_5Y(violation_aver_5Y$violation.person[i])
}
us_states_aver_5Y <- left_join(us_states, violation_aver_5Y)
ggplot(data = us_states_aver_5Y, mapping = aes(x = long, y = lat, group = group, fill = aver.range)) + geom_polygon(color = "grey90", size = 0.1) + coord_map(projection = "albers", lat0 = 39, lat1 = 45) + labs(fill = "Amount") + theme_map() + labs(title="Health-based Violations Per Person Each Year From 2014") + theme(plot.title = element_text(hjust = 0.5,lineheight=.8, face="bold")) + scale_fill_discrete(limits = c('5e-07 - 2.5e-06','2.5e-06 - 5e-06','5e-06 - 1e-05','1e-05 - 5e-05','5e-05 - 1e-04', '1e-04 - 5e-04')) 



