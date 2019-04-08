library(forecast)
library(readxl)
library(urca)
library(vars)
library(tibble)

var.data<-read_excel('/Users/JwR-MBP15/Desktop/R/Pronosticos con R/Datos/Datos Ultimas Clase.xlsx', sheet = 'FULL')
var.data$Fecha<-as.Date(var.data$Fecha)

colnames(var.data)
no_diff<-c("CETES28", "DGS1M")

var.data<-var.data %>% subset(Fecha<=as.Date("2019-02-01") & Fecha>=as.Date("2001-07-01"))

ts.data<-ts(var.data[,-1], start = c(2001, 07), frequency = 12)
ts.tasas<-ts.data[, no_diff]
x1<-colnames(var.data)[-1][!(colnames(var.data)[-1] %in%  no_diff)]
ts.notasas<-diff(log(ts.data), 12)[,x1]
ts.notasas<-ts.notasas*100


ts.var<-ts.union(ts.tasas, ts.notasas)
colnames(ts.var)<-gsub('ts.tasas.', '',  colnames(ts.var))
colnames(ts.var)<-gsub('ts.notasas.', '',  colnames(ts.var))

sample.ts<- window(ts.var,start=c(2002,7), end=c(2018, 1))
sample.ts.2<-window(ts.var, start=c(2018, 2), end=c(2018,12))

#Reproducción del paper de Espadas
#Estimación del modelo VAR
INPCS<-c("INPC","Subyacente1", "Subyacente2", "Subyacete", 
         "Nsubyacente","Nsubyacente1","Nsubyacente2")

exogenas<-c( 'INDPRO','DGS1M', 'CPIAUCSL', 'COMM')
endogenas<-c('IGAE_TRC', "CETES28" ,'USDMXN')

ts.exogenas<-sample.ts[, exogenas]

all.vars.inpc<-lapply(1:length(INPCS), function(ipc){

  index.prices<-INPCS[ipc]
  endogenas<-c(endogenas, index.prices)
  ts.endogenas<-sample.ts[, endogenas]
  
  selection<-VARselect(ts.endogenas, lag.max = 6, type = c("const"),exogen = ts.exogenas)
  order.var<-selection$selection[1]
  fit.var<-VAR(ts.endogenas, p=order.var, exogen = ts.exogenas, type = 'const')
  impulse.response<-irf(fit.var, impulse='USDMXN', response=index.prices, boot=T)
  
  resume<-tibble(INPC=index.prices, VAR=list(fit.var), IR=impulse.response)
  return(resume)  
  
  })

all.vars.inpc<-do.call('rbind', all.vars.inpc)

ts.exogenas.h<-sample.ts.2[, exogenas]
var.forecast<-predict(all.vars.inpc$VAR[[1]], dumvar=ts.exogenas.h, n.ahead = nrow(ts.exogenas.h))
target.var<-var.forecast$fcst[[INPCS[1]]]

pronosticado=ts(target.var[,1], start=start(sample.ts.2), frequency = 12)
observado=sample.ts.2[, INPCS[1]]

vars.backtesting<-ts.union(pronosticado, observado)
forecast::autoplot(vars.backtesting)



