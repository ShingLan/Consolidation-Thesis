# Read Data
library(tidyverse)
library(dplyr)
library(ggplot2)
library(sf)
service <- st_read("Service/Water_System_Service_area/shape_file/service_areas.shp")
service <- filter(service, st_dimension(st_sfc(service$geometry))==2)
service <- service %>% select("pwsid","d_populati","geometry")
colnames(service) <- c("pwsid","population","geometry")
service$pwsid <- as.character(service$pwsid)
for (i in c(3:nrow(service))){
  k=i-1
  for (j in c(1:k)){
    if(service$pwsid[j] == service$pwsid[i]){
      service$geometry[i] <- service$geometry[j]
    }
  }
}
service <- unique(service)
violation <- data.frame(read_csv('violation-2018-4.csv')) 


# Sick and Healthy Systems
violation <- violation %>% select("PWS.ID","Population.Served.Count")
violation <- unique(violation)
service_sick <- data.frame(matrix(ncol = 1,nrow = 2660))
service_heal <- data.frame(matrix(ncol = 1,nrow = 2139))
colnames(service_sick) <- c("pwsid")
colnames(service_heal) <- c("pwsid")
k <- 1
for (i in c(1:nrow(service))){
  for (j in c(1:nrow(violation))){
    if(service$pwsid[i] == violation$PWS.ID[j]){
      service_sick$pwsid[k] <- service$pwsid[i]
      k <- k +1
    }
  }
}
service_heal$pwsid <- setdiff(service$pwsid,service_sick$pwsid)
service_sick <- inner_join(service,service_sick,by="pwsid")
service_heal <- inner_join(service,service_heal,by="pwsid")
service_sick_small <- filter(service_sick,population<10000)


# Distance with Healthy Systems
service_sick_small_g <- st_geometry(service_sick_small)
service_heal_g <- st_geometry(service_heal)
service_sick_small_c <- st_transform(service_sick_small_g, 3311) %>% st_centroid() %>% st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') 
service_heal_c <- st_transform(service_heal_g, 3311) %>% st_centroid() %>% st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') 
distance_all_heal <- st_distance(service_sick_small_c,service_heal_c)
distance_heal <- data.frame(matrix(ncol = 3,nrow = 2303))
names(distance_heal) <- c("pwsid_sick","pwsid_heal","distance")
for (i in c(1:nrow(distance_all_heal))){
  distance_heal$distance[i] <- min(distance_all_heal[i,], na.rm = TRUE)
  distance_heal$pwsid_sick[i] <- service_sick_small$pwsid[i]
  distance_heal$pwsid_heal[i] <-service_heal$pwsid[which.min(distance_all_heal[i,])]
}
ggplot(distance_heal,aes(x=distance/1000)) + geom_histogram(binwidth=0.2,fill="lightblue", colour="black") + xlim(0,30)+ xlab("Distance/KM") + ylab("Amount") + labs(title="Distance with Nearest Systems without Violations in California") + theme(plot.title = element_text(hjust = 0.65,lineheight=.8, face="bold")) + geom_vline(aes(xintercept=3), colour="#BB0000", linetype="dashed")+ geom_vline(aes(xintercept=10), colour="#BB0000", linetype="dashed")  
dist_c_heal <- cbind(distance_heal, service_sick_small_c)
dist_c_heal$nearest_system <- "Larger than 10 KM"
for (i in c(1:nrow(dist_c_heal))){
  ifelse(dist_c_heal$distance[i]<3000, dist_c_heal$nearest_system[i]<-"Smaller than 3 KM", 
         ifelse(dist_c_heal$distance[i]<10000, dist_c_heal$nearest_system[i]<-"Between 3KM and 10 KM", NA))
}
ggplot()+geom_sf(data = dist_c_heal,aes(color = nearest_system),show.legend = "point")+guides(color=guide_legend(title="Nearest System")) + labs(title="Distribution with Systems without Violations in California") + theme(plot.title = element_text(hjust = 0.3,lineheight=.8, face="bold")) 
sum(dist_c_heal$nearest_system=="Larger than 10 KM")
sum(dist_c_heal$nearest_system=="Smaller than 3 KM")
sum(dist_c_heal$nearest_system=="Between 3KM and 10 KM")


# Distance with Any Systems
service_g <- st_geometry(service)
service_c <- st_transform(service_g, 3311) %>% st_centroid() %>% st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') 
distance_all_any <- st_distance(service_sick_small_c,service_c)
is.na(distance_all_any) <- !distance_all_any
distance_any <- data.frame(matrix(ncol = 3,nrow = 2303))
names(distance_any) <- c("pwsid_sick","pwsid","distance")
for (i in c(1:nrow(distance_all_any))){
  distance_any$distance[i] <- min(distance_all_any[i,], na.rm = TRUE)
  distance_any$pwsid_sick[i] <- service_sick_small$pwsid[i]
  distance_any$pwsid[i] <-service$pwsid[which.min(distance_all_any[i,])]
}
ggplot(distance_any,aes(x=distance/1000)) + geom_histogram(binwidth=0.2,fill="lightblue", colour="black") + xlim(0,30)+ xlab("Distance/KM") + ylab("Amount") + labs(title="Distance with Nearest Systems in California") + theme(plot.title = element_text(hjust = 0.65,lineheight=.8, face="bold")) + geom_vline(aes(xintercept=3), colour="#BB0000", linetype="dashed")+ geom_vline(aes(xintercept=10), colour="#BB0000", linetype="dashed")  
dist_c_any <- cbind(distance_any, service_sick_small_c)
dist_c_any$nearest_system <- "Larger than 10 KM"
for (i in c(1:nrow(dist_c_any))){
  ifelse(dist_c_any$distance[i]<3000, dist_c_any$nearest_system[i]<-"Smaller than 3 KM", 
         ifelse(dist_c_any$distance[i]<10000, dist_c_any$nearest_system[i]<-"Between 3KM and 10 KM", NA))
}
ggplot()+geom_sf(data = dist_c_any,aes(color = nearest_system),show.legend = "point")+guides(color=guide_legend(title="Nearest System")) + labs(title="Distribution with Nearest Systems in California") + theme(plot.title = element_text(hjust = 0.3,lineheight=.8, face="bold")) 
sum(dist_c_any$nearest_system=="Larger than 10 KM")
sum(dist_c_any$nearest_system=="Smaller than 3 KM")
sum(dist_c_any$nearest_system=="Between 3KM and 10 KM")


# Distance with Small Systems
service_small <- filter(service,population<10000)
service_small_g <- st_geometry(service_small)
service_small_c <- st_transform(service_small_g, 3311) %>% st_centroid() %>% st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') 
distance_all_small <- st_distance(service_sick_small_c,service_small_c)
is.na(distance_all_small) <- !distance_all_small
distance_small <- data.frame(matrix(ncol = 3,nrow = 2303))
names(distance_small) <- c("pwsid_sick","pwsid_small","distance")
for (i in c(1:nrow(distance_all_small))){
  distance_small$distance[i] <- min(distance_all_small[i,], na.rm = TRUE)
  distance_small$pwsid_sick[i] <- service_sick_small$pwsid[i]
  distance_small$pwsid_small[i] <-service_small$pwsid[which.min(distance_all_small[i,])]
}
ggplot(distance_small,aes(x=distance/1000)) + geom_histogram(binwidth=0.2,fill="lightblue", colour="black") + xlim(0,30)+ xlab("Distance/KM") + ylab("Amount") + labs(title="Distance with Nearest Small Systems in California") + theme(plot.title = element_text(hjust = 0.65,lineheight=.8, face="bold")) + geom_vline(aes(xintercept=3), colour="#BB0000", linetype="dashed")+ geom_vline(aes(xintercept=10), colour="#BB0000", linetype="dashed")  
dist_c_small <- cbind(distance_small, service_sick_small_c)
dist_c_small$nearest_system <- "Larger than 10 KM"
for (i in c(1:nrow(dist_c_any))){
  ifelse(dist_c_small$distance[i]<3000, dist_c_small$nearest_system[i]<-"Smaller than 3 KM", 
         ifelse(dist_c_small$distance[i]<10000, dist_c_small$nearest_system[i]<-"Between 3KM and 10 KM", NA))
}
ggplot()+geom_sf(data = dist_c_small,aes(color = nearest_system),show.legend = "point")+guides(color=guide_legend(title="Nearest System")) + labs(title="Distribution with Nearest Small Systems in California") + theme(plot.title = element_text(hjust = 0.3,lineheight=.8, face="bold")) 
sum(dist_c_small$nearest_system=="Larger than 10 KM")
sum(dist_c_small$nearest_system=="Smaller than 3 KM")
sum(dist_c_small$nearest_system=="Between 3KM and 10 KM")


# Distance with Large Systems
service_large <- filter(service,population>=10000)
service_large_g <- st_geometry(service_large)
service_large_c <- st_transform(service_large_g, 3311) %>% st_centroid() %>% st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') 
distance_all_large <- st_distance(service_sick_small_c,service_large_c)
distance_large <- data.frame(matrix(ncol = 3,nrow = 2303))
names(distance_large) <- c("pwsid_sick","pwsid_large","distance")
for (i in c(1:nrow(distance_all_large))){
  distance_large$distance[i] <- min(distance_all_large[i,], na.rm = TRUE)
  distance_large$pwsid_sick[i] <- service_sick_small$pwsid[i]
  distance_large$pwsid_large[i] <-service_large$pwsid[which.min(distance_all_large[i,])]
}
ggplot(distance_large,aes(x=distance/1000)) + geom_histogram(binwidth=0.2,fill="lightblue", colour="black") + xlim(0,30)+ xlab("Distance/KM") + ylab("Amount") + labs(title="Distance with Nearest Large Systems in California") + theme(plot.title = element_text(hjust = 0.65,lineheight=.8, face="bold")) + geom_vline(aes(xintercept=3), colour="#BB0000", linetype="dashed")+ geom_vline(aes(xintercept=10), colour="#BB0000", linetype="dashed")  
dist_c_large <- cbind(distance_large, service_sick_small_c)
dist_c_large$nearest_system <- "Larger than 10 KM"
for (i in c(1:nrow(dist_c_large))){
  ifelse(dist_c_large$distance[i]<3000, dist_c_large$nearest_system[i]<-"Smaller than 3 KM", 
         ifelse(dist_c_large$distance[i]<10000, dist_c_large$nearest_system[i]<-"Between 3KM and 10 KM", NA))
}
ggplot()+geom_sf(data = dist_c_large,aes(color = nearest_system),show.legend = "point")+guides(color=guide_legend(title="Nearest System")) + labs(title="Distribution with Nearest Large Systems in California") + theme(plot.title = element_text(hjust = 0.3,lineheight=.8, face="bold")) 
sum(dist_c_large$nearest_system=="Larger than 10 KM")
sum(dist_c_large$nearest_system=="Smaller than 3 KM")
sum(dist_c_large$nearest_system=="Between 3KM and 10 KM")
