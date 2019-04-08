library(dplyr)
library(forecast)
library(reshape2)
library(readxl)

datos<-read_excel('/Users/JwR-MBP15/Desktop/Transgenia/devs/e-model/data/timeline.xlsx', sheet = 'FULL')
datos$Fecha<-as.Date(datos$Fecha)

#datos<-subset(datos, Fecha<=)
colnames(datos)
quitar<-c("Subyacente1", "Subyacente2", "Subyacete", "Nsubyacente","Nsubyacente1","Nsubyacente2")

datos<-datos %>% select(-one_of(quitar)) %>% subset(Fecha<=as.Date("2019-02-01") & Fecha>=as.Date("2001-07-01"))
datos_scaled<-datos

maxs <- apply(datos_scaled[,-1], 2, max, na.rm=T) 
mins <- apply(datos_scaled[,-1], 2, min, na.rm=T)
scaled<-scale(datos_scaled[, -1], center = mins, scale = maxs - mins)

datos_scaled[, -1]<-scaled

autoplot(ts.data)


ts.data<-ts(datos_scaled[,-1], start = c(2001, 07), frequency = 12)
ts.data<-diff(ts.data, 12)

sample.ts<- window(ts.data, end=c(2018, 1))
sample.ts.2<-window(ts.data, start=c(2018, 2), end=c(2018,12))

winners_step<-step(lm(INPC~., data = sample.ts), trace = 0) %>% coef() %>% names()
winners_step[-1]
exogena<-winners_step[-1]


target<-'INPC'
h=nrow(sample.ts.2)
# Linear ------------------------------------------------------------------

exogenas<-paste0(exogena, collapse='+')
modelo_ts<-as.formula(paste0(target ,'~',exogenas ))

ts_SPX<-tslm(modelo_ts, data = sample.ts)
ts_f_spx<-forecast(ts_SPX, newdata = data.frame(sample.ts.2))

# Univariantes ------------------------------------------------------------

SPX_ARIMA<-auto.arima(y = sample.ts[, target])
f_arima<-forecast(SPX_ARIMA, h=h)

SPX_NN<-nnetar(sample.ts[, target])
f_NN<-forecast(SPX_NN, h=h)

# SPX Exogenas ------------------------------------------------------------

SPX_ARIMAX<-auto.arima(y = sample.ts[, target], xreg =sample.ts[, exogena])
yh<-forecast(SPX_ARIMAX, xreg = sample.ts.2[, exogena])

SPX_NNX<-nnetar(y = sample.ts[, target],xreg =sample.ts[, exogena])
y_nnx<-forecast(SPX_NNX, xreg = sample.ts.2[, exogena])

compare_forecast<-ts.union(sample.ts.2[, target], yh$mean,f_arima$mean, f_NN$mean, y_nnx$mean, ts_f_spx$mean)
colnames(compare_forecast)<-c('Observado','ARIMA-X' , 'ARIMA', 'NN', 'NN-X', 'TS_LM')

compare_forecast<-data.frame(Fecha=seq.Date(from=as.Date('2018-02-01'), length.out = 11, by='month') ,
                             compare_forecast, row.names = NULL)
colnames(compare_forecast)[-1]<-c('Observado','ARIMA-X' , 'ARIMA', 'NN', 'NN-X','TS_LM')

data_ggplot<-melt(compare_forecast, id='Fecha')

library(ggplot2)

ggplot(data_ggplot, aes(x=Fecha, y=value*100, col=variable))+
  geom_line(size=1.5)+
  scale_y_continuous(position = "right")+
  scale_color_discrete(name='Metodos')+
  ylab('')+xlab('')+ggtitle(paste0('Forecast ', target), 
                            subtitle = '2018-02-2018-12')

errores<-compare_forecast$Observado-compare_forecast[,-c(1,2)]
media.errores<-colMeans(errores^2)
rmse<-sqrt(media.errores)

write.table( data.frame(rmse), "clipboard", sep="\t", na="") 
