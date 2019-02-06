# Read Shapefile
library(tidyverse)
library(dplyr)
library(ggplot2)
library(sf)
service <- st_read("Service/Water_System_Service_area/shape_file/service_areas.shp")
service <- filter(service, st_dimension(st_sfc(service$geometry))==2)
service <- service %>% select("pwsid","d_populati","geometry")
service$pwsid <- as.character(service$pwsid)
for (i in c(3:nrow(service))){
  k=i-1
  for (j in c(1:k)){
    if(service$pwsid[j] == service$pwsid[i]){
      service$geometry[i] <- service$geometry[j]
    }
    }
}
service<-unique(service)

# Shortest Distance
service_g <- st_geometry(service)
service_c <- st_centroid(service_g)
distance = st_distance(service_c)
diag(distance) <- NA
min_dist <- data.frame(matrix(ncol = 3, nrow = nrow(service)))
names(min_dist) <- c("X","Y","distance")
for (i in c(1:nrow(service))){
    min_dist$distance[i] <- min(distance[i,], na.rm = TRUE)
    min_dist$X[i] <- i
    min_dist$Y[i]<- which.min(distance[i,])
}
ggplot(min_dist,aes(x=distance/1000)) + geom_histogram(binwidth=0.5,fill="lightblue", colour="black") + xlim(0,30)+ xlab("Distance/KM") + ylab("Amount") 

# Geographical Distribution 
dist_c <- cbind(min_dist, service_c)
dist_c$nearest_system <- "Less than 3000 M"
for (i in c(1:nrow(service))){
  ifelse(dist_c$distance[i]>3000, dist_c$nearest_system[i]<-"More than 3000 M", NA)
}
ggplot()+geom_sf(data = dist_c,aes(color = nearest_system),show.legend = "point")+guides(color=guide_legend(title="Nearest System"))

# Design Flow
design_flow <- function(x){
  y <- ifelse(25 <= x & x <=100, 0.030,
  ifelse(101 <=x & x <=500, 0.124,
    ifelse(501 <=x & x <=1000, 0.305,
      ifelse(1001 <=x & x <=3300, 0.740,
        ifelse(3301 <=x & x <=10000, 2.152,
          ifelse(10001<=x & x <=50000, 7.365,
            ifelse(50001<=x & x <=100000, 22.614, 75.072)))))))
return(y)
  }

for (i in c(1:nrow(service))){
  service$design.flow[i] <- design_flow(service$d_populati[i])
}
ggplot(service, aes(x=as.character(service$design.flow))) + geom_bar(width=0.5, fill="lightblue", colour="black") + xlab("Design Flow (MGD)") + ylab("Amount") 


# Average Flow
average_flow <- function(x){
  y <- ifelse(25 <= x & x <=100, 0.007,
    ifelse(101 <=x & x <=500, 0.035,
      ifelse(501 <=x & x <=1000, 0.094,
        ifelse(1001 <=x & x <=3300, 0.251,
          ifelse(3301 <=x & x <=10000, 0.819,
            ifelse(10001<=x & x <=50000, 3.200,
              ifelse(50001<=x & x <=100000, 11.087, 37.536)))))))
return(y)
  }

for (i in c(1:nrow(service))){
  service$average.flow[i] <- average_flow(service$d_populati[i])
}
ggplot(service, aes(x=as.character(service$average.flow))) + geom_bar(width=0.5, fill="lightblue", colour="black") + xlab("Average Flow (MGD)") + ylab("Amount") 


# Current Cost
service$operation.cost <- (0.3093*service$average.flow^0.3921+0.4167*service$average.flow^0.2307+0.4384*service$average.flow^0.2946+0.2414*service$average.flow^0.4658+0.3965*service$average.flow^0.3349+0.5941*service$average.flow^0.1643+0.4441*service$average.flow^0.4323+0.3889*service$average.flow^0.376+0.693*service$average.flow^0.3122+2.9129*service$average.flow^0.6484+1.2576*service$average.flow^1.0549+3.121*service$average.flow^0.9384+1.8653*service$average.flow^0.9808+0.686*service$average.flow^0.638)/14
service$operation.cost <- round(service$operation.cost,2)
service$operation.cost = factor(service$operation.cost, levels=c("0.08","0.15","0.24","0.4","0.86","2.34","6.51","19.08"))
ggplot(service, aes(x=as.factor(service$operation.cost))) + geom_bar(width=0.5, fill="lightblue", colour="black") + xlab("Current Annual Cost (Million)") + ylab("Amount") 

# Consolidation Cost
con_cost <- min_dist
con_cost$X_popu <- service$d_populati
con_cost$X_pwsid <- service$pwsid
for (i in c(1:nrow(con_cost))){
  con_cost$Y_popu[i] <- service$d_populati[con_cost$Y[i]]
}
con_cost$sum_popu <- con_cost$X_popu + con_cost$Y_popu
for (i in c(1:nrow(con_cost))){
  con_cost$design.flow[i] <- design_flow(con_cost$sum_popu[i])
}
for (i in c(1:nrow(con_cost))){
  con_cost$average.flow[i] <- average_flow(con_cost$sum_popu[i])
}
con_cost$capital.cost.t <- 0.0453*(7.4222*con_cost$design.flow^0.6139+6.6669*con_cost$design.flow^0.6286+8.7684*con_cost$design.flow^0.5957+5.7927*con_cost$design.flow^0.6876+9.506*con_cost$design.flow^0.5623+9.506*con_cost$design.flow^0.5623+7.1052*con_cost$design.flow^0.8302+7.7144*con_cost$design.flow^0.6392+12.839*con_cost$design.flow^0.5763+12.612*con_cost$design.flow^0.7177+31.05*con_cost$design.flow^0.6097+20.622*con_cost$design.flow^0.907+43.577*con_cost$design.flow^0.6739+15.212*con_cost$design.flow^0.7271)/14
con_cost$capital.cost.c <- 0.0259*(con_cost$distance*2.65^(-4))  
con_cost$operation.cost.t <- (0.3093*con_cost$average.flow^0.3921+0.4167*con_cost$average.flow^0.2307+0.4384*con_cost$average.flow^0.2946+0.2414*con_cost$average.flow^0.4658+0.3965*con_cost$average.flow^0.3349+0.5941*con_cost$average.flow^0.1643+0.4441*con_cost$average.flow^0.4323+0.3889*con_cost$average.flow^0.376+0.693*con_cost$average.flow^0.3122+2.9129*con_cost$average.flow^0.6484+1.2576*con_cost$average.flow^1.0549+3.121*con_cost$average.flow^0.9384+1.8653*con_cost$average.flow^0.9808+0.686*con_cost$average.flow^0.638)/14
con_cost$operation.cost.c <- con_cost$distance*6.63^(-7)
con_cost$future.cost <- con_cost$capital.cost.t+con_cost$capital.cost.c+con_cost$operation.cost.t+con_cost$operation.cost.c
ggplot(con_cost,aes(x=future.cost)) + geom_histogram(binwidth=0.5,fill="lightblue", colour="black") + xlim(0,40)+ xlab("Future Annual Cost (million)") + ylab("Amount") 

# Cost Comparison
service$operation.cost <- (0.3093*service$average.flow^0.3921+0.4167*service$average.flow^0.2307+0.4384*service$average.flow^0.2946+0.2414*service$average.flow^0.4658+0.3965*service$average.flow^0.3349+0.5941*service$average.flow^0.1643+0.4441*service$average.flow^0.4323+0.3889*service$average.flow^0.376+0.693*service$average.flow^0.3122+2.9129*service$average.flow^0.6484+1.2576*service$average.flow^1.0549+3.121*service$average.flow^0.9384+1.8653*service$average.flow^0.9808+0.686*service$average.flow^0.638)/14
service$operation.cost <- round(service$operation.cost,2)
con_cost$X_operation_cost <- service$operation.cost
for (i in c(1:nrow(con_cost))){
  con_cost$Y_operation_cost[i] <- service$operation.cost[con_cost$Y[i]]
}
for (i in c(1:nrow(con_cost))){
  con_cost$saving[i] <- ifelse(
    con_cost$future.cost[i] < con_cost$X_operation_cost[i] + con_cost$Y_operation_cost[i], "Cheaper", "No Cheaper")
}
con_cost$e_saving <- con_cost$future.cost-(con_cost$X_operation_cost+con_cost$Y_operation_cost)
con_cost_saving_1 <- con_cost[con_cost$e_saving<=0,]
con_cost_saving_2 <- con_cost[con_cost$e_saving>0&con_cost$e_saving<0.5,]
con_cost_saving_3 <- con_cost[con_cost$e_saving>0.5&con_cost$e_saving<1,]
con_cost_saving_4 <- con_cost[con_cost$e_saving>1&con_cost$e_saving<10,]
con_cost_saving_5 <- con_cost[con_cost$e_saving>10,]
saving <- data.frame(matrix(ncol = 3, nrow = 5))
colnames(saving) <- c("saving","number","value")
saving$saving <- c("<0", "0-0.5", "0.5-1", "1-10", ">10")
saving[1,2] <- count(con_cost_saving_1)
saving[2,2] <- count(con_cost_saving_2)
saving[3,2] <- count(con_cost_saving_3)
saving[4,2] <- count(con_cost_saving_4)
saving[5,2] <- count(con_cost_saving_5)
saving[1,3] <- 0.069
saving[2,3] <- 0.235
saving[3,3] <- 0.185
saving[4,3] <- 0.472
saving[5,3] <- 0.039
ggplot(saving, aes(x="", y=number, fill=saving)) + geom_bar(stat="identity", width=1) + coord_polar("y", start=0) + geom_text(aes(label = paste0(value*100, "%")), position = position_stack(vjust = 0.3)) + scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999")) + labs(x = NULL, y = NULL, fill = "Extra Expense (million)") + theme_classic() + theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

# Violation
violation <- data.frame(read_csv('Violation.csv'))
violation_n <- merge(con_cost, violation, by.x = "X_pwsid", by.y = "PWSId")
violation_s <- filter(violation, SeriousViolator == "Yes")
ggplot(violation_n,aes(x=Vioremain)) + geom_histogram(binwidth=10,fill="lightblue", colour="black") + xlim(0,220)+ ylim(0,50)+ xlab("Remaining Uncorrected Violation Points") + ylab("Amount") 
ggplot(violation_n,aes(x=Viopaccr)) + geom_histogram(binwidth=10,fill="lightblue", colour="black") + xlim(0,220)+ ylim(0,50)+ xlab("Violation Points Accrued (5 years)")+ ylab("Amount") 
violation_n$QtrsWithVio <- as.character(violation_n$QtrsWithVio)
violation_n$QtrsWithVio = factor(violation_n$QtrsWithVio, levels=c("1","2","3","4","5","6","7","8","9","10","11","12"))
ggplot(violation_n, aes(x=QtrsWithVio)) + geom_bar(width=0.5, fill="lightblue", colour="black") + xlab("Quarters with Violation (3 years)") + ylab("Amount") 

# Correct Violation
for (i in c(1:nrow(con_cost))){
  for (j in c(1:nrow(violation_s))){
    if(con_cost$X_pwsid[i] == violation_s$PWSId[j]){
      violation_s$saving[j] <- con_cost$e_saving[i]
    }
  }
}
violation_nn <- violation_n %>% filter(Vioremain>0)
violation_nn_saving_1 <- violation_nn[violation_nn$e_saving<=0,]
violation_nn_saving_2 <- violation_nn[violation_nn$e_saving>0&violation_nn$e_saving<0.5,]
violation_nn_saving_3 <- violation_nn[violation_nn$e_saving>0.5&violation_nn$e_saving<1,]
violation_nn_saving_4 <- violation_nn[violation_nn$e_saving>1&violation_nn$e_saving<10,]
violation_nn_saving_5 <- violation_nn[violation_nn$e_saving>10,]
violation_nn_saving <- data.frame(matrix(ncol = 3, nrow = 5))
colnames(violation_nn_saving) <- c("saving","number","value")
violation_nn_saving$saving <- c("<0", "0-0.5", "0.5-1", "1-10", ">10")
violation_nn_saving[1,2] <- count(violation_nn_saving_1)
violation_nn_saving[2,2] <- count(violation_nn_saving_2)
violation_nn_saving[3,2] <- count(violation_nn_saving_3)
violation_nn_saving[4,2] <- count(violation_nn_saving_4)
violation_nn_saving[5,2] <- count(violation_nn_saving_5)
violation_nn_saving[1,3] <- 0.042
violation_nn_saving[2,3] <- 0.208
violation_nn_saving[3,3] <- 0.216
violation_nn_saving[4,3] <- 0.515
violation_nn_saving[5,3] <- 0.019
ggplot(violation_nn_saving, aes(x="", y=number, fill=saving)) + geom_bar(stat="identity", width=0.01) + coord_polar("y", start=0) + geom_text(aes(label = paste0(value*100, "%")), position = position_stack(vjust = 0.5)) + scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999")) + labs(x = NULL, y = NULL, fill = "Extra Expense (million)") + theme_classic() + theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

       