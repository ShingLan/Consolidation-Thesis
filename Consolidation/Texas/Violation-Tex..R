# Loading Package
library(tidyverse)

# Read Data
vio_2016_1 <- data.frame(read_csv('violation-2016-1.csv')) 
vio_2016_2 <- data.frame(read_csv('violation-2016-2.csv')) 
vio_2016_3 <- data.frame(read_csv('violation-2016-3.csv'))
vio_2016_4 <- data.frame(read_csv('violation-2016-4.csv')) 
vio_2017_1 <- data.frame(read_csv('violation-2017-1.csv')) 
vio_2017_2 <- data.frame(read_csv('violation-2017-2.csv')) 
vio_2017_3 <- data.frame(read_csv('violation-2017-3.csv'))
vio_2017_4 <- data.frame(read_csv('violation-2017-4.csv')) 
vio_2018_1 <- data.frame(read_csv('violation-2018-1.csv')) 
vio_2018_2 <- data.frame(read_csv('violation-2018-2.csv')) 
vio_2018_3 <- data.frame(read_csv('violation-2018-3.csv'))
vio_2018_4 <- data.frame(read_csv('violation-2018-4.csv')) 
violation <- rbind (vio_2016_1, vio_2016_2, vio_2016_3, vio_2016_4, vio_2017_1, vio_2017_2, vio_2017_3, vio_2017_4, vio_2018_1, vio_2018_2, vio_2018_3, vio_2018_4) %>% setNames(tolower(names(.)))
water_2016_1 <- data.frame(read_csv('water-2016-1.csv')) 
water_2016_2 <- data.frame(read_csv('water-2016-2.csv')) 
water_2016_3 <- data.frame(read_csv('water-2016-3.csv'))
water_2016_4 <- data.frame(read_csv('water-2016-4.csv')) 
water_2017_1 <- data.frame(read_csv('water-2017-1.csv')) 
water_2017_2 <- data.frame(read_csv('water-2017-2.csv')) 
water_2017_3 <- data.frame(read_csv('water-2017-3.csv'))
water_2017_4 <- data.frame(read_csv('water-2017-4.csv')) 
water_2018_1 <- data.frame(read_csv('water-2018-1.csv')) 
water_2018_2 <- data.frame(read_csv('water-2018-2.csv')) 
water_2018_3 <- data.frame(read_csv('water-2018-3.csv'))
water_2018_4 <- data.frame(read_csv('water-2018-4.csv')) 
system <- rbind (water_2016_1, water_2016_2, water_2016_3, water_2016_4, water_2017_1, water_2017_2, water_2017_3, water_2017_4, water_2018_1, water_2018_2, water_2018_3, water_2018_4) %>% setNames(tolower(names(.)))


# Set Table
vio_health <- filter(violation, is.health.based=="Y")
vio_mcl <- filter(violation, violation.category.code=="MCL")
vio_tt <- filter(violation, violation.category.code=="TT")
vio_situation <- data.frame(matrix(ncol = 6, nrow = 7))
colnames(vio_situation) <- c("population","0~500","501~3300","3301~10000","10001~100000",">100000")
vio_situation[1,1] <- "health based violation"
vio_situation[2,1] <- "maximum contaminant levels violation"
vio_situation[3,1] <- "treatment technolgy violation"
vio_situation[4,1] <- "systems with health based violation"
vio_situation[5,1] <- "systems with maximum contaminant levels violation"
vio_situation[6,1] <- "systems with treatment technolgy violation"
vio_situation[7,1] <- "total community water systems"

# Violation
vio_situation[1,2] <- sum(vio_health$population.served.count<=500) 
vio_situation[1,3] <- sum(500<vio_health$population.served.count & vio_health$population.served.count<=3300) 
vio_situation[1,4] <- sum(3300<vio_health$population.served.count & vio_health$population.served.count<=10000) 
vio_situation[1,5] <- sum(10000<vio_health$population.served.count & vio_health$population.served.count<=100000) 
vio_situation[1,6] <- sum(vio_health$population.served.count>100000) 
vio_situation[2,2] <- sum(vio_mcl$population.served.count<=500) 
vio_situation[2,3] <- sum(500<vio_mcl$population.served.count & vio_mcl$population.served.count<=3300) 
vio_situation[2,4] <- sum(3300<vio_mcl$population.served.count & vio_mcl$population.served.count<=10000) 
vio_situation[2,5] <- sum(10000<vio_mcl$population.served.count & vio_mcl$population.served.count<=100000) 
vio_situation[2,6] <- sum(vio_mcl$population.served.count>100000) 
vio_situation[3,2] <- sum(vio_tt$population.served.count<=500) 
vio_situation[3,3] <- sum(500<vio_tt$population.served.count & vio_tt$population.served.count<=3300) 
vio_situation[3,4] <- sum(3300<vio_tt$population.served.count & vio_tt$population.served.count<=10000) 
vio_situation[3,5] <- sum(10000<vio_tt$population.served.count & vio_tt$population.served.count<=100000) 
vio_situation[3,6] <- sum(vio_tt$population.served.count>100000) 

# Systems With Health Base Violations
vio_health_1 <- vio_health[vio_health$population.served.count<=500,]
vio_health_pws_1 <- unique(vio_health_1$pws.id)
vio_situation[4,2] <- length(vio_health_pws_1)
vio_health_2 <- vio_health[500<vio_health$population.served.count & vio_health$population.served.count<=3300,]
vio_health_pws_2 <- unique(vio_health_2$pws.id)
vio_situation[4,3] <- length(vio_health_pws_2)
vio_health_3 <- vio_health[3300<vio_health$population.served.count & vio_health$population.served.count<=10000,]
vio_health_pws_3 <- unique(vio_health_3$pws.id)
vio_situation[4,4] <- length(vio_health_pws_3)
vio_health_4 <- vio_health[10000<vio_health$population.served.count & vio_health$population.served.count<=100000,]
vio_health_pws_4 <- unique(vio_health_4$pws.id)
vio_situation[4,5] <- length(vio_health_pws_4)
vio_health_5 <- vio_health[vio_health$population.served.count>100000,]
vio_health_pws_5 <- unique(vio_health_5$pws.id)
vio_situation[4,6] <- length(vio_health_pws_5)

# Systems With Maximum Contaminant Levels Violation
vio_mcl_1 <- vio_mcl[vio_mcl$population.served.count<=500,]
vio_mcl_pws_1 <- unique(vio_mcl_1$pws.id)
vio_situation[5,2] <- length(vio_mcl_pws_1)
vio_mcl_2 <- vio_mcl[500<vio_mcl$population.served.count & vio_mcl$population.served.count<=3300,]
vio_mcl_pws_2 <- unique(vio_mcl_2$pws.id)
vio_situation[5,3] <- length(vio_mcl_pws_2)
vio_mcl_3 <- vio_mcl[3300<vio_mcl$population.served.count & vio_mcl$population.served.count<=10000,]
vio_mcl_pws_3 <- unique(vio_mcl_3$pws.id)
vio_situation[5,4] <- length(vio_mcl_pws_3)
vio_mcl_4 <- vio_mcl[10000<vio_mcl$population.served.count & vio_mcl$population.served.count<=100000,]
vio_mcl_pws_4 <- unique(vio_mcl_4$pws.id)
vio_situation[5,5] <- length(vio_mcl_pws_4)
vio_mcl_5 <- vio_mcl[vio_mcl$population.served.count>100000,]
vio_mcl_pws_5 <- unique(vio_mcl_5$pws.id)
vio_situation[5,6] <- length(vio_mcl_pws_5)

# Systems With Treatment Technolgy Violation
vio_tt_1 <- vio_tt[vio_tt$population.served.count<=500,]
vio_tt_pws_1 <- unique(vio_tt_1$pws.id)
vio_situation[6,2] <- length(vio_tt_pws_1)
vio_tt_2 <- vio_tt[500<vio_tt$population.served.count & vio_tt$population.served.count<=3300,]
vio_tt_pws_2 <- unique(vio_tt_2$pws.id)
vio_situation[6,3] <- length(vio_tt_pws_2)
vio_tt_3 <- vio_tt[3300<vio_tt$population.served.count & vio_tt$population.served.count<=10000,]
vio_tt_pws_3 <- unique(vio_tt_3$pws.id)
vio_situation[6,4] <- length(vio_tt_pws_3)
vio_tt_4 <- vio_tt[10000<vio_tt$population.served.count & vio_tt$population.served.count<=100000,]
vio_tt_pws_4 <- unique(vio_tt_4$pws.id)
vio_situation[6,5] <- length(vio_tt_pws_4)
vio_tt_5 <- vio_tt[vio_tt$population.served.count>100000,]
vio_tt_pws_5 <- unique(vio_tt_5$pws.id)
vio_situation[6,6] <- length(vio_tt_pws_5)

# Total System
system_1 <- system[system$population.served.count<=500,]
system_pws_1 <- unique(system_1$pws.id)
vio_situation[7,2] <- length(system_pws_1)
system_2 <- system[500<system$population.served.count & system$population.served.count<=3300,]
system_pws_2 <- unique(system_2$pws.id)
vio_situation[7,3] <- length(system_pws_2)
system_3 <- system[3300<system$population.served.count & system$population.served.count<=10000,]
system_pws_3 <- unique(system_3$pws.id)
vio_situation[7,4] <- length(system_pws_3)
system_4 <- system[10000<system$population.served.count & system$population.served.count<=100000,]
system_pws_4 <- unique(system_4$pws.id)
vio_situation[7,5] <- length(system_pws_4)
system_5 <- system[system$population.served.count>100000,]
system_pws_5 <- unique(system_5$pws.id)
vio_situation[7,6] <- length(system_pws_5)

# Output
write.csv(x = vio_situation,file = "Texas Violation.csv")
