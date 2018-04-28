library(readxl)
library(rCharts)
library(zoo)
library(dplyr)
library(dynlm)
#Preparadar caminhos de diretórios
main<-getwd()
sub1<-"line"
sub2<-"scatter"
sub3<-"reg"

#Carregar fotes de dados
bd <- read_excel("BalancoDengue.xlsx")
bd$Data<-as.Date(bd$Data)
od <- read_excel("Orcamento Dengue.xlsx")
od$Data<-as.Date(od$Data)
odchart<-od
odchart$VLIQ<-od$VLIQ/1000
#pmg<-gtrends("sintomas dengue",geo = "BR-MG", time = "2015-01-01 2017-12-31")
pmg<-readRDS("pmg.rds")
df<-pmg$interest_over_time
df$date<-as.yearmon(df$date, "%Y-%m")
gt<-df %>% group_by(date) %>%   summarise(Minimo = min(hits), Media = mean(hits), Maximo = max(hits))
gtmed<-gt[,c(1,3)]
gtmin<-gt[,c(1,2)]
gtmax<-gt[,c(1,4)]
gtmed$Data<-as.Date(gtmed$date)
gtmed<-gtmed[,c(3,2)]

#Salvar objetos RDS para gráficos de linha
setwd(file.path(sub1))
saveRDS(bd,"bd.rds")
saveRDS(odchart,"od.rds")
saveRDS(gtmed,"gtmed.rds")
setwd(main)
saveRDS(bd,"bd.rds")
saveRDS(odchart,"od.rds")
saveRDS(gtmed,"gtmed.rds")

#Construir objetos para gráficos scatter chart, fazendo transformações logarítmicas
scatterod<-inner_join(gtmed,od,by="Data")
scatterod$VLIQ<-log(scatterod$VLIQ)
scatterod$Media<-log(scatterod$Media)
scatterbd<-inner_join(gtmed,bd,by="Data")
scatterbd$Casos<-log(scatterbd$Casos)
scatterbd$Media<-log(scatterbd$Media)
scatterbo<-inner_join(od,bd,by="Data")
scatterbo$VLIQ<-log(scatterbo$VLIQ)
scatterbo$Casos<-log(scatterbo$Casos)
setwd(file.path(sub2))
saveRDS(scatterod,"scatterod.rds")
saveRDS(scatterbd,"scatterbd.rds")
saveRDS(scatterbo,"scatterbo.rds")
setwd(main)
saveRDS(scatterod,"scatterod.rds")
saveRDS(scatterbd,"scatterbd.rds")
saveRDS(scatterbo,"scatterbo.rds")

#Estimar modelos de regressão
reg<-inner_join(bd,od,by="Data")
reg<-inner_join(reg,gtmed,by="Data")
reg<-as.ts(read.zoo(reg,FUN=as.yearmon))
score<-dynlm(VLIQ~Casos,data=reg)
summary(score)
score2<-dynlm(Casos~Media,data=reg)
summary(score2)
setwd(file.path(sub3))
saveRDS(reg,"reg.rds")
setwd(main)
saveRDS(reg,"reg.rds")

p1 <- nPlot(Media ~ Casos, data = scatterbd, type = 'scatterChart')
p1$xAxis(axisLabel = 'Log (Casos Confirmados)')
p1$yAxis(axisLabel = 'Log (Interesse Google Trends)')
p1$chart(forceY = c(0,5))
p1$chart(forceX = c(0,12))
p1$chart(showControls = FALSE)
p1$chart(showLegend = FALSE)
p1

p2 <- nPlot(VLIQ ~ Casos, data = scatterbo, type = 'scatterChart')
p2$xAxis(axisLabel = 'Log (Casos Confirmados)')
p2$yAxis(axisLabel = 'Log (Valor Liquidado)')
p2$chart(forceY = c(8,18))
p2$chart(forceX = c(0,12))
p2$chart(showControls = FALSE)
p2$chart(showLegend = FALSE)
p2

p3 <- nPlot(VLIQ ~ Media, data = scatterod, type = 'scatterChart')
p3$xAxis(axisLabel = 'Log (Interesse Google Trends)')
p3$yAxis(axisLabel = 'Log (Valor Liquidado)')
p3$chart(forceY = c(8,18))
p3$chart(forceX = c(0,5))
p3$chart(showControls = FALSE)
p3$chart(showLegend = FALSE)
p3

q1 <- nPlot(Casos ~ Data, data = bd, type = 'lineChart')
q1$xAxis(tickFormat="#!function(d) {return d3.time.format.utc('%Y-%m')(new Date(d * 24 * 60 * 60 * 1000));}!#")
q1$chart(forceY = c(0,60000))
q1$yAxis(axisLabel = 'Casos Confirmados de Dengue',width=60)
q1$chart(showLegend = FALSE)
q1

q2 <- nPlot(VLIQ ~ Data, data = odchart, type = 'lineChart')
q2$xAxis(tickFormat="#!function(d) {return d3.time.format.utc('%Y-%m')(new Date(d * 24 * 60 * 60 * 1000));}!#")
q2$chart(forceY = c(0,4000))
q2$yAxis(axisLabel = 'Valor Liquidado - R$ mil',width=60)
q2$chart(showLegend = FALSE)
q2

q3 <- nPlot(Media ~ Data, data = gtmed, type = 'lineChart')
q3$xAxis(tickFormat="#!function(d) {return d3.time.format.utc('%Y-%m')(new Date(d * 24 * 60 * 60 * 1000));}!#")
q3$chart(forceY = c(0,100))
q3$yAxis(axisLabel = 'Google Trends - Interesse Sintomas Dengue',width=60)
q3$chart(showLegend = FALSE)
q3




