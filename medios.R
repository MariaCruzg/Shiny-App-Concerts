library(odbc)
library(sqldf)
library(dplyr)

library(ggplot2)
library(tidyverse)
library(DBI)
con <- dbConnect(odbc(), Driver = "PostgreSQL", Server = "54.245.36.29", 
                 Database = "ssm", UID = "pubadm", PWD = "password", Port = 5432)


list_tables<-dbListObjects(con)
x1<-dbListTables(con)

total_tables<-nrow(list_tables)

for (i in 1:total_tables) {
  
  evento<-tbl(con,as.character(x1[i]))
  evento1<-tbl_df(evento)
  
  names<-colnames(evento1)
  
  Descripcion_evento<-summary(evento1)
}



evento1<-tbl_df(evento)

names<-colnames(evento1)
Descripcion_evento<-summary(evento1)





ssm_eventos<-tbl(con,"ssmordins_eventos")
total<-left_join(evento,ssm_eventos)
graph1<- total%>% group_by(id)%>%summarise(montos=sum(monto))
ploty<-as.data.frame(graph1)
plot(ploty)



