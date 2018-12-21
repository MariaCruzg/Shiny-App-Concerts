


library(odbc)

con <- dbConnect(odbc(),
                 Driver = "SQLServer",
                 Server = "54.245.36.29",
                 Database = "ocesa_dwh",
                 UID = "ocesa_da",
                 PWD = "P4ssw0rd",
                 Port = 1433)
library(glue)
library(sqldf)
library(dplyr)
library(ggplot2)
library(RWeka)
library(zoo)


x1<-dbListTables(con)

Fac_audreps <- tbl(con,"fact_audreps") #dia a dia
#Fac_completa<-tbl(con,"fact_ocerep_ut") #acumulados 
con %>% tbl("fact_audreps")

completa1z<-tbl(con,"fact_completa")
#completa , transacciones, fecha de transacción,  #pep 

dim_precio<-tbl(con,"dim_nivel_precio")
dim_evento<-tbl(con,"dim_evento")
dim_evento_cat<-tbl(con,"dim_evento_cat")
dim_metodo<-tbl(con,"dim_metodo_pago") 
dim_inmueble<-tbl(con,"dim_inmueble")
dim_inmueble_desz<-tbl(con,"dim_inmueble_des")
dim_evento_des<-tbl(con,"dim_evento_des")
ljsp <- left_join(completa1z, dim_evento)
com3<-left_join(ljsp,dim_metodo)
boleto_tipo<-tbl(con,"dim_boleto_tipo")

com4<-left_join(com3,boleto_tipo)
dim_zona<-tbl(con,"dim_zona")
dim_evento_propiedad<-tbl(con,"dim_evento_propiedad")

dim_canal<-tbl(con,"dim_canal")
#evento<-inner_join(dim_evento_cat,dim_evento)
com5<-left_join(com4,dim_inmueble_desz)
com6<-left_join(com5,dim_evento_cat)
com7<-left_join(com6,dim_canal)
dim_acredor<-tbl(con,"dim_acreedor")

dim_origen_outlet<-tbl(con,"dim_origen_outlet")
com8<-left_join(com7,dim_origen_outlet)



com9<-left_join(com8,dim_evento_propiedad)
com10<-left_join(com9,dim_evento_des)
com11<-left_join(com10,dim_evento)
#dim_evento_propiedad<-tbl(con,"dim_evento_propiedad")
dim_canal_suz<-tbl(con,"dim_canal_sub")
#com12<-left_join(com11,dim_evento_propiedad)
#completa<-inner_join(completa1z,com9)
com12<-left_join(com11,dim_canal_suz)

com13<-left_join(com12,dim_zona)
a<- com12 %>% select(zona_pk,canal_sub_pk,evento_cve_pk,evento_des_pk,evento_cat_ind,evento_des_ind, evento_cat_sub_ind,inmueble_pk,evento_fecha,boleto_tipo_cve,registro_tipo_pk,transaccion_diaria_importe_comision_outlet,transaccion_diaria_importe_costo_tc,transaccion_diaria_importe_costo_boleto,transaccion_diaria_importe_cargo_interno,transaccion_diaria_importe_ingreso_tc,inmueble_des_pk,transaccion_diaria_importe_total,transaccion_diaria_fecha,boleto_tipo_des,evento_pk,transaccion_diaria_importe_ingreso_tc,transaccion_diaria_importe_bruto,transaccion_diaria_cantidad_devolucion,evento_cat_sub,registro_tipo_pk,inmueble_des, evento_cat, origen_outlet_des,evento_cat_pk,evento_fecha,boleto_tipo_pk,transaccion_diaria_cantidad_devolucion,inmueble_des_pk,inmueble_des,transaccion_diaria_cantidad_bruto,evento_cat_ind,nivel_precio_pk,transaccion_diaria_importe_neto,canal_des,transaccion_diaria_cantidad_neto)
a2<- a%>% group_by(zona_pk,evento_fecha,evento_des_ind,evento_cat_ind,evento_des_pk,inmueble_des) %>% summarise(Boletos=sum(transaccion_diaria_cantidad_neto),Boleto.Tran.com_outlet=sum(transaccion_diaria_importe_comision_outlet),Trans.importe_total=sum(transaccion_diaria_importe_total)) 


#### Lista de Talentos ##### 

a3<- a%>%filter(evento_des_ind=="TIMBIRICHE")

negativos<-a3 %>% filter(transaccion_diaria_cantidad_devolucion>1)
curva<-NULL
curva1<-NULL
curva<-data.frame(a3)

########## plot Curva para dividir ####################### 
curva1<-curva %>%group_by(evento_fecha,transaccion_diaria_fecha) %>% summarise(total_boletos=sum(transaccion_diaria_cantidad_neto))

c<-hist(curva1$total_boletos,labels = TRUE)

#### Negativos ######## 


negativos.devoluciones<-subset(curva1, total_boletos<0)
##########################################

## FIltrado por evento###  

     Eventos.Diferentes<-as.data.frame(table(curva$evento_fecha))
     n.eventos<-nrow(Eventos.Diferentes)
     prefix<-"data.evento"
    
     subfix<-seq(1:n.eventos)
     my.names<-paste(prefix,subfix,sep=".")
    
     lapply(my.names,function(x){assign(x,NA,envir = .GlobalEnv)})
     
     for (i in 1:n.eventos) {
       
       evento.CV<-curva %>%filter(evento_fecha==as.Date(Eventos.Diferentes$Var1[i]))
       lapply(my.names[i],function(x){assign(x,evento.CV,envir = .GlobalEnv)})
       
     }
      
     
    
  
#secuencia1<-c("1")
# tenemos la base de de Datos ### 
     secuencia<-NULL
prefix<-"y.evento"
subfix<-seq(1:n.eventos)
my.names.y<-paste(prefix,subfix,sep=".")
Suma<-NULL
y<-NULL
y1<-NULL
datalist = list()
df_total = data.frame()

add.col<-function(df, new.col) {n.row<-max(length(df),length(new.col))[1]
length(new.col)<-n.row
cbind(df, new.col)
}

new.df<-add.col(secuencia,y)
    for(k in 1:n.eventos ){
   
   
    
   
      y<-cumsum(get(my.names[k])$transaccion_diaria_cantidad_neto)
      #y1[j]<-sum(data.evento.2$transaccion_diaria_cantidad_neto[1:j])
        
      lapply(my.names.y[k],function(x){assign(x,y,envir = .GlobalEnv)})
      
      datalist[[k]] <- y 
      df <- data.frame(y)
      df_total <- rbind(df_total,df)
   #y<-data.evento.1$transaccion_diaria_importe_total 
     if (length(y)>5) {
      secuencia<-add.col(y,secuencia)
     }
  # secuencia1<- do.call(rbind,secuencia)
    }
#secuencia<-data.frame(t(sapply(datalist,c)))
     ###############################)################################ Salida SECUENCIA ######################################
secuencia1<- as.numeric(as.character((secuencia)))

     
     ################################################## Grafica de Curvas de llenado ####################################### 
      
      lim.inf<-  n.eventos-5
       lim.sup<- n.eventos
     matplot(secuencia[,lim.inf:lim.sup],  type = "o", col = rainbow(ncol(secuencia[,lim.inf:lim.sup])))
     ######################################################## Grafica de llenado con todos los registros ###############################3
     
     matplot(secuencia,  type = "o", col = rainbow(ncol(secuencia)))
     
     ##################################### PRedicción de la PROXIMA CURVA DE LLENADO ######################################
     
     #autoplot(secuencia)
     #plot
     library(forecast)
     library(ggplot2)
  
     fit <- HoltWinters(secuencia, beta=FALSE, gamma=FALSE)
     fit <- auto.arima(secuencia)
     #fit <- naive(curva1$total_boletos)
     autoplot(forecast(fit,h=25))
     RowM <- rowMeans(secuencia)
     ########################################################################################################################
     
     ## Transpuesta ## 
     #legend("topleft", legend = 1:4, col=1:4, pch=1) #
    
    