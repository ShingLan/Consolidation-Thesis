# Loading Package
library(tidyverse)

# Read Data
region_1 <- data.frame(read_csv('facility_report_region_1.csv')) 
region_2 <- data.frame(read_csv('facility_report_region_2.csv')) 
region_3 <- data.frame(read_csv('facility_report_region_3.csv'))
region_4 <- data.frame(read_csv('facility_report_region_4.csv')) 
region_5 <- data.frame(read_csv('facility_report_region_5.csv'))
region_6 <- data.frame(read_csv('facility_report_region_6.csv'))
region_7 <- data.frame(read_csv('facility_report_region_7.csv')) 
region_8 <- data.frame(read_csv('facility_report_region_8.csv'))
region_9 <- data.frame(read_csv('facility_report_region_9.csv')) 
region_10 <- data.frame(read_csv('facility_report_region_10.csv'))
facility <- rbind (region_1, region_2, region_3, region_4, region_5, region_6, region_7, region_8, region_9, region_10) %>% setNames(tolower(names(.)))
facility_California <- facility %>% filter(facility.activity == "Active", pws.type == "Community water system", primacy.agency == "California")

# Primary Source
facility_California_N <- facility_California %>% select(pws.id, primary.source, owner.type, population.served.count)
facility_California_N <- distinct(facility_California_N)
primary_source<-table(facility_California_N$primary.source)

# Ownership 
owner_type<-table(facility_California_N$owner.type)

# Population Served
Population_1 <- facility_California_N[facility_California_N$population.served.count>=0&facility_California_N$population.served.count<501,]
Population_2 <- facility_California_N[facility_California_N$population.served.count>=501&facility_California_N$population.served.count<=3300,]
Population_3 <- facility_California_N[facility_California_N$population.served.count>=3301&facility_California_N$population.served.count<=10000,]
Population_4 <- facility_California_N[facility_California_N$population.served.count>=10001&facility_California_N$population.served.count<=100000,]
Population_5 <- facility_California_N[facility_California_N$population.served.count>100000,]
summarise(group_by(Population_1, owner.type),count = n()) 
summarise(group_by(Population_2, owner.type),count = n()) 
summarise(group_by(Population_3, owner.type),count = n()) 
summarise(group_by(Population_4, owner.type),count = n()) 
summarise(group_by(Population_5, owner.type),count = n()) 

# Treatment Objective
table(facility_California$primary.source,facility_California$treatment.objective)


