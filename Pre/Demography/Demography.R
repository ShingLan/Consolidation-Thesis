# Read Data
library(tidyverse)
library(dplyr)
CWS_2014 <- data.frame(read_csv('California_2014.csv'))
CWS_2019 <- data.frame(read_csv('California_2019.csv'))
CWS_2014 <- CWS_2014 %>% select("PWS.ID","Population.Served.Count")
CWS_2019 <- CWS_2019 %>% select("PWS.ID","Population.Served.Count")
names(CWS_2014) <- c("PWS.ID","Population.2014")
names(CWS_2019) <- c("PWS.ID","Population.2019")
CWS_2014 <- CWS_2014[which(rowSums(CWS_2014==0)==0),]
CWS_2019 <- CWS_2019[which(rowSums(CWS_2019==0)==0),]

# Population Change
CWS <- inner_join(CWS_2014,CWS_2019,by="PWS.ID")
CWS$Change <- CWS$Population.2019-CWS$Population.2014
for (i in c(1:nrow(CWS))){
  ifelse(CWS$Change[i]==0, CWS$Variation[i]<-"Unchange", 
         ifelse(CWS$Change[i]<0, CWS$Variation[i]<-"Decrease",CWS$Variation[i]<-"Increase"))}
for (i in c(1:nrow(CWS))){
  ifelse(0<CWS$Population.2014[i]&CWS$Population.2014[i]<500, CWS$Size[i]<-"0-500", 
         ifelse(501<CWS$Population.2014[i]&CWS$Population.2014[i]<3000, CWS$Size[i]<-"501-3300",
                ifelse(3301<CWS$Population.2014[i]&CWS$Population.2014[i]<10000, CWS$Size[i]<-"3301-10000", 
                   ifelse(10001<CWS$Population.2014[i]&CWS$Population.2014[i]<100000, CWS$Size[i]<-"10001-100000", CWS$Size[i]<-">100000"))))}
ggplot(CWS,aes(x=Variation)) + geom_bar(aes(fill=Size),width = 0.3) + labs(x = "Population Served Variation", y = "Quantity") + labs(title="Population Variation At Public Community System Level In California") + theme(plot.title = element_text(hjust = 0.5,lineheight=.8, face="bold"))+ scale_fill_discrete(limits = c('0-500','501-3300','3301-10000','10001-100000','>100000')) 
ggplot(CWS,aes(x=Change)) + geom_histogram(fill="lightblue", colour="black") 
