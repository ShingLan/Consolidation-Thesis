# Read Data
library(tidyverse)
library(dplyr)
violation_2014 <- data.frame(read_csv('violation_2014.csv'))
violation_2015 <- data.frame(read_csv('violation_2015.csv'))
violation_2016 <- data.frame(read_csv('violation_2016.csv'))
violation_2017 <- data.frame(read_csv('violation_2017.csv'))
violation_2018 <- data.frame(read_csv('violation_2018.csv'))
violation <- rbind (violation_2014, violation_2015, violation_2016, violation_2017, violation_2018)
planes <- group_by(violation, Primacy.Agency)
violation_s <- summarise(planes,count = n())
colnames(violation_s) <- c("region","number")

# Heat Map
library(maps)
library(mapproj)
library(ggthemes)
library(ggplot2)
us_states <- map_data("state")
violation_s$region <- tolower(violation_s$region)
us_states_elec <- left_join(us_states, violation_s)
ggplot(data = us_states_elec, mapping = aes(x = long, y = lat, group = group, fill = number)) + geom_polygon(color = "grey90", size = 0.1) + coord_map(projection = "albers", lat0 = 39, lat1 = 45) + labs(fill = "Number") +  scale_fill_gradient(low = "white", high = "#CB454A") + theme_map() 

# System Percentage 
population_1 <- violation[violation$Population.Served.Count>=0&violation$Population.Served.Count<501,]
population_2 <- violation[violation$Population.Served.Count>=501&violation$Population.Served.Count<=3300,]
population_3 <- violation[violation$Population.Served.Count>=3301&violation$Population.Served.Count<=10000,]
population_4 <- violation[violation$Population.Served.Count>=10001&violation$Population.Served.Count<=100000,]
population_5 <- violation[violation$Population.Served.Count>100000,]
system_p <- data.frame(matrix(ncol = 3, nrow = 5))
colnames(system_p) <- c("system","number","value")
system_p$system <- c("<= 500", "501-3,300", "3,301-10,000", "10,001-100,000", ">100,000")
system_p[1,2] <- count(population_1)
system_p[2,2] <- count(population_2)
system_p[3,2] <- count(population_3)
system_p[4,2] <- count(population_4)
system_p[5,2] <- count(population_5)
system_p[1,3] <- 0.433
system_p[2,3] <- 0.348
system_p[3,3] <- 0.131
system_p[4,3] <- 0.083
system_p[5,3] <- 0.005
ggplot(system_p, aes(x="", y=number, fill=system)) + geom_bar(stat="identity", width=1) + coord_polar("y", start=0) + geom_text(aes(label = paste0(value*100, "%")), position = position_stack(vjust = 0.3)) + scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999")) + labs(x = NULL, y = NULL, fill = "Population") + theme_classic() + theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

# California
violation_cal <- filter(violation, Primacy.Agency == "California")
count(violation_cal, Population.Served.Count <= 500)

