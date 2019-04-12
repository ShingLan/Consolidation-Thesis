# Loading Package
library(tidyverse)

# Read Data
violation <- data.frame(read_csv('violation-2018-4.csv')) 
violation <- violation %>% select("PWS.ID","Population.Served.Count","Is.Health.Based","Violation.Category.Code")

# Health Violation
vio_health <- violation %>% select("PWS.ID","Population.Served.Count","Is.Health.Based") 
vio_health <- filter(vio_health, Is.Health.Based=="Y")
vio_health$Amount <- 1
vio_health_n <- vio_health %>% group_by(PWS.ID, Population.Served.Count,Is.Health.Based) %>% summarise(Amount = sum(Amount))
for (i in c(1:nrow(vio_health_n))){
  ifelse(0<vio_health_n$Population.Served.Count[i]&vio_health_n$Population.Served.Count[i]<500, vio_health_n$Size[i]<-"0-500", 
         ifelse(501<vio_health_n$Population.Served.Count[i]&vio_health_n$Population.Served.Count[i]<3000, vio_health_n$Size[i]<-"501-3300",
                ifelse(3301<vio_health_n$Population.Served.Count[i]&vio_health_n$Population.Served.Count[i]<10000, vio_health_n$Size[i]<-"3301-10000", 
                       ifelse(10001<vio_health_n$Population.Served.Count[i]&vio_health_n$Population.Served.Count[i]<100000, vio_health_n$Size[i]<-"10001-100000", vio_health_n$Size[i]<-">100000"))))}
vio_health_n %>% group_by(Size) %>% summarise(min(Amount))
vio_health_n %>% group_by(Size) %>% summarise(max(Amount))
vio_health_n<-within(vio_health_n,{
  Size<-factor(Size,levels=c("0-500","501-3300","3301-10000","10001-100000",">100000"))
})
ggplot(data=vio_health_n, aes(x=Size,y=Amount))+geom_boxplot(aes(fill=Size)) + labs(x = "System Size", y = "Quantity") + labs(title="System with Health Based Violations") + theme(plot.title = element_text(hjust = 0.5,lineheight=.8, face="bold"))+ scale_fill_discrete(limits = c('0-500','501-3300','3301-10000','10001-100000','>100000')) 

# MCL
vio_mcl <- filter(violation, Violation.Category.Code=="MCL")
vio_mcl$Amount <- 1
vio_mcl_n <- vio_mcl %>% group_by(PWS.ID, Population.Served.Count,Is.Health.Based,Violation.Category.Code) %>% summarise(Amount = sum(Amount))
for (i in c(1:nrow(vio_mcl_n))){
  ifelse(0<vio_mcl_n$Population.Served.Count[i]&vio_mcl_n$Population.Served.Count[i]<500, vio_mcl_n$Size[i]<-"0-500", 
         ifelse(501<vio_mcl_n$Population.Served.Count[i]&vio_mcl_n$Population.Served.Count[i]<3000, vio_mcl_n$Size[i]<-"501-3300",
                ifelse(3301<vio_mcl_n$Population.Served.Count[i]&vio_mcl_n$Population.Served.Count[i]<10000, vio_mcl_n$Size[i]<-"3301-10000", 
                       ifelse(10001<vio_mcl_n$Population.Served.Count[i]&vio_mcl_n$Population.Served.Count[i]<100000, vio_mcl_n$Size[i]<-"10001-100000", vio_mcl_n$Size[i]<-">100000"))))}
vio_mcl_n %>% group_by(Size) %>% summarise(min(Amount))
vio_mcl_n %>% group_by(Size) %>% summarise(max(Amount))
vio_mcl_n<-within(vio_mcl_n,{
  Size<-factor(Size,levels=c("0-500","501-3300","3301-10000","10001-100000",">100000"))
})
ggplot(data=vio_mcl_n, aes(x=Size,y=Amount))+geom_boxplot(aes(fill=Size)) + labs(x = "System Size", y = "Quantity") + labs(title="System with Maximum Contaminant Levels Violations") + theme(plot.title = element_text(hjust = 0.5,lineheight=.8, face="bold"))+ scale_fill_discrete(limits = c('0-500','501-3300','3301-10000','10001-100000','>100000')) 

#TT
vio_tt <- filter(violation, Violation.Category.Code=="TT")
vio_tt$Amount <- 1
vio_tt_n <- vio_tt %>% group_by(PWS.ID, Population.Served.Count,Is.Health.Based,Violation.Category.Code) %>% summarise(Amount = sum(Amount))
for (i in c(1:nrow(vio_tt_n))){
  ifelse(0<vio_tt_n$Population.Served.Count[i]&vio_tt_n$Population.Served.Count[i]<500, vio_tt_n$Size[i]<-"0-500", 
         ifelse(501<vio_tt_n$Population.Served.Count[i]&vio_tt_n$Population.Served.Count[i]<3000, vio_tt_n$Size[i]<-"501-3300",
                ifelse(3301<vio_tt_n$Population.Served.Count[i]&vio_tt_n$Population.Served.Count[i]<10000, vio_tt_n$Size[i]<-"3301-10000", 
                       ifelse(10001<vio_tt_n$Population.Served.Count[i]&vio_tt_n$Population.Served.Count[i]<100000, vio_tt_n$Size[i]<-"10001-100000", vio_tt_n$Size[i]<-">100000"))))}
vio_tt_n %>% group_by(Size) %>% summarise(min(Amount))
vio_tt_n %>% group_by(Size) %>% summarise(max(Amount))
vio_tt_n<-within(vio_tt_n,{
  Size<-factor(Size,levels=c("0-500","501-3300","3301-10000","10001-100000",">100000"))
})
ggplot(data=vio_tt_n, aes(x=Size,y=Amount))+geom_boxplot(aes(fill=Size)) + labs(x = "System Size", y = "Quantity") + labs(title="System with Treatment Technology Violations") + theme(plot.title = element_text(hjust = 0.5,lineheight=.8, face="bold"))+ scale_fill_discrete(limits = c('0-500','501-3300','3301-10000','10001-100000','>100000')) 


