Mynumber<-2+2
my_object<-(2/4*3)
my_vector<-c(1,2,3)
my_string_vector<-c("this","is","a","vector")
mean(my_vector)
#don't confuse numeric and string vestors!!!
my_vector[3] #this is how to index using []
#load(file path)
#to set working directory:
  #setwd(file/folder path)
#example to index/filter/search for a certain data:
  #file name$variable name{3} --> will only show the 3rd one

install.packages("tidyverse")

library(tidyverse)

go_weast<-read.csv(file.choose(),header=T)
view(go_weast)
go_weast$X<-NULL
go_weast$X.1<-NULL
go_weast$X.2<-NULL
go_weast$X.3<-NULL
go_weast$X.4<-NULL
go_weast$X.5<-NULL
go_weast$X.6<-NULL
go_weast$X.7<-NULL
go_weast$X.8<-NULL
go_weast$X.9<-NULL
go_weast$X.10<-NULL
go_weast$X.11<-NULL
go_weast$X.12<-NULL
go_weast$X.13<-NULL

SK_data<-filter(go_weast, COUNTRY_ID=="SK") %>%
  group_by(YEAR) %>%
    summarise(SK_average=mean(OVERNIGHTS, na.rm=TRUE))

#VISUALIZATION BAR
ggplot(SK_data, aes(YEAR, weight=SK_average))+
  geom_bar(fill="pink")+
    xlab("Average Overnights in Slovakia")+
      ylab("Overnights")
        theme_minimal()

#VISUALIZATION LINE (COMPARISON)
SK_CZK_data<-go_weast %>%
  filter(MONTH=="5") %>%
    group_by(COUNTRY_ID, YEAR) %>%
      summarise(SK_CZK_average=OVERNIGHTS, na.rm=TRUE)

ggplot(SK_CZK_data, aes(x=YEAR, y=SK_CZK_average, group = COUNTRY_ID, colour = COUNTRY_ID))+
  geom_line()
    theme_minimal()
      ylab("Overnights in Slovakia and Czechia")
        
COUNTRY_ID<-select(go_weast, COUNTRY_ID)
HOTELS<-count(go_weast, NUMBER_OF_HOTELS)
alpha_order<-arrange(go_weast, COUNTRY_ID)

#MODELLING
ggplot(go_weast,aes(y=TOURIST_PER_MONTH, x=HICP))+
  geom_point()+
    geom_smooth(method="lm", formula = y~x)+
      theme_light()

#REGRESSION
summary(lm(OVERNIGHTS~TREAT+POST+TREAT.POST+HICP+
             NUMBER_OF_HOTELS+TOURIST_PER_MONTH,
              data = go_weast))
