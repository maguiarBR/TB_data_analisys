# TB_data_analisys
I'm trying to improve the data visualization for my graphic. Something is off.

#we need first of all get tidyverse
library(tidyverse)

#here I am doing some modifications on the original database "who" so i can use
#the functions on tidyverse

b <- who %>%
  gather(code,value,new_sp_m014:newrel_f65,na.rm=TRUE)%>%
  mutate(
    code=stringr::str_replace(code,"newrel","new_rel")
  )%>%
  separate(code,c("new","var","sexage"))%>%
  select(-new,-iso2,-iso3)%>%
  separate(sexage,c("sex","age"),sep=1)
  )

#here I am preparing only the database that I need for my graph
#I want to make a graph relating all the cases on all countries per year
#And per sex

a <- b%>%
  group_by(year,sex)%>%
  summarize(casos=sum(value))
a
ggplot(data=a,mapping=aes(x=year,y=casos))+
  geom_point(aes(color=sex))+
  geom_smooth(se=FALSE)+
  labs(title = "Casos de tubercolose por ano em diversos países")+
  xlab("anos")+
  ylab("número de casos")

