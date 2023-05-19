###############TRABAJO FINAL DE FUTUROS Y OPCIONES##########

######Valoracion de opciones financieras########

# la cobertura será para un mes

ls()
getwd()
setwd("C:/Users/Valentina/Documents/Universidad/Futuros y Opciones")
setwd("Trabajo Final")
getwd()

datos=read.csv("TRM.csv",header=T,dec = ".", sep = ";")
precios=datos$TRM
precios
s= tail(precios,1)
# Precio So al día de cobertura 21 febrero de 2020
s

rendimientos=diff(log(precios))
#Rendimiento continuos mensuales

vol=sd(rendimientos)
vol
#Volatilidad continua mensual

mu=mean(rendimientos)
mu
#Rendimientos continuos mesuales

#Datos de mercado
r=0.04251 
#(IBR E.A a un mes)
rf=0.0143 
#Trsy E.A a un mes)
  
#Tasas Continuas mensuales
r=log(1+r)/12
rf= log(1+rf)/12
r
rf

T=1 #mes
deltaT=1 #todo lo mensual lo multiplico x 1 para que quede continuo mensual

 
# 1. Estrategia tradicional: Largo en call

k=s #Las opciones ATM tendrán un precio strike igual a S0
k

#VALORACIÓN OPCIÓN CALL ATM
st_prima=vector()
vp_compensacion_call_1=vector()

for(i in 1:50000){
  #ST para valoracion de opciones
  st_prima[i]=s*exp((r-rf-vol^2/2)*deltaT+vol*sqrt(deltaT)*rnorm(1)) 
  vp_compensacion_call_1[i]=max(st_prima[i]-k,0)*exp(-(r-rf)*deltaT)
  
}

call_1=mean(vp_compensacion_call_1)  #Prima opcion call
call_1

#PRECIOS CON COBERTURA ESTRATEGIA LARGO EN CALL
st=vector()
precio_cobertura_comprador_noprima_largocall= vector()
precio_cobertura_comprador_prima_largocall=vector()

for(i in 1:50000){
  #ST para el subyacente
  st[i]=s*exp((mu-vol^2/2)*deltaT+vol*sqrt(deltaT)*rnorm(1))

  precio_cobertura_comprador_noprima_largocall[i]=abs(-st[i]+max(-k+st[i],0))

  precio_cobertura_comprador_prima_largocall[i]=abs(-st[i]+max(-k+st[i],0)-(call_1))
  
}

mean_noprima_largocall= mean(precio_cobertura_comprador_noprima_largocall)
mean_noprima_largocall
mean_prima_largocall=mean(precio_cobertura_comprador_prima_largocall)
mean_prima_largocall

#PERCENTILES ESTRATEGIA LARGO CALL
qv1_noprima_largocall=quantile(precio_cobertura_comprador_noprima_largocall,0.95)
qv2_noprima_largocall=quantile(precio_cobertura_comprador_noprima_largocall,0.05)
qv1_noprima_largocall

qv2_noprima_largocall

qv1_prima_largocall=quantile(precio_cobertura_comprador_prima_largocall,0.95)
qv2_prima_largocall=quantile(precio_cobertura_comprador_prima_largocall,0.05)
qv1_prima_largocall
qv2_prima_largocall


#HISTOGRAMA PRECIO SIN COBERTURA

#Percentiles ST[i]

mean_sin_cobertura=mean(st)
mean_sin_cobertura
qv1_sin_cobertura_st= quantile(st,0.95)
qv1_sin_cobertura_st
qv2_sin_cobertura_st=quantile(st, 0.05)
qv2_sin_cobertura_st

hist(st,breaks=60,col=5,xlab="Precio sin cobertura",ylab="Frecuencia", 
     main="Histograma de precios sin cobertura",
     xlim= c(3100,3800))
abline(v=mean(st),lwd=3)
abline(v=quantile(st,0.05),lwd=3)
abline(v=quantile(st,0.95),lwd=3)

ganar=0
for(i in 1:length(st)){
  if (st[i]<3403.5){
    ganar=1+ganar
  }
}
print(ganar)

prob_ganar=(ganar/length(st))*100

#HISTOGRAMA PRECIO CON ESTRATEGIA LARGO CALL SIN PRIMA
hist(precio_cobertura_comprador_noprima_largocall,breaks=60,col=5,
     xlab="Precio estrategia largo call",ylab="Frecuencia", 
     main="Histograma de precios estrategia largo call sin prima",
     xlim=c(3090,3400))
abline(v=mean(precio_cobertura_comprador_noprima_largocall),lwd=3)
abline(v=quantile(precio_cobertura_comprador_noprima_largocall,0.05),lwd=3)
abline(v=quantile(precio_cobertura_comprador_noprima_largocall,0.95),lwd=3)

#HISTOGRAMA PRECIO CON ESTRATEGIA LARGO CALL CON PRIMA
hist(precio_cobertura_comprador_prima_largocall,breaks=60,col=5,
     xlab="Precio estrategia largo call",ylab="Frecuencia", 
     main="Histograma de precios estrategia largo call con prima",
     xlim=c(3190,3500))
abline(v=mean(precio_cobertura_comprador_prima_largocall),lwd=3)
abline(v=quantile(precio_cobertura_comprador_prima_largocall,0.05),lwd=3)
abline(v=quantile(precio_cobertura_comprador_prima_largocall,0.95),lwd=3)

##################################################### VALENTINA ###################################################


   #2. ForwardParticipativo

#Largo Call + Corto Put
#K1 = K2

vp_compensacion_call_2=vector()
vp_compensacion_put_2=vector()

for (i in 1:50000){
  vp_compensacion_call_2[i]= max(st_prima[i]-k,0)*exp(-(r-rf)*deltaT)
  
  vp_compensacion_put_2[i]= max(k-st_prima[i],0)*exp(-(r-rf)*deltaT)
 
}
#Tenemos la simulación de la compensación call
call_2= mean(vp_compensacion_call_2)
call_2
#Tenemos la simulación de la compensación put
put_2=mean(vp_compensacion_put_2)
put_2

#Ahora vamos calcular el precio de la opción

#Comprador de divisas

#Forward Participativo

precio_cobertura_comprador_noprima_fwdpart=vector()
precio_cobertura_comprador_prima_fwdpart=vector()

for(i in 1:50000){
  precio_cobertura_comprador_noprima_fwdpart[i]=abs(-st[i]+max(st[i]-k,0)+min(st[i]-k,0))
  precio_cobertura_comprador_prima_fwdpart[i]=abs(-st[i]+max(st[i]-k,0)-call_2+min(st[i]-k,0)+put_2)
}

mean_noprima_forward_participativo=mean(precio_cobertura_comprador_noprima_fwdpart)
mean_noprima_forward_participativo
mean_prima_forward_participativo=mean(precio_cobertura_comprador_prima_fwdpart)
mean_prima_forward_participativo

#PERCENTILES ESTRATEGIA FORWARD PARTICIPATIVO
qv1_noprima_forward_participativo=quantile(precio_cobertura_comprador_noprima_fwdpart,0.95)
qv2_noprima_forward_participativo=quantile(precio_cobertura_comprador_noprima_fwdpart,0.05)
qv1_noprima_forward_participativo
qv2_noprima_forward_participativo

qv1_prima_forward_participativo=quantile(precio_cobertura_comprador_prima_fwdpart,0.95)
qv2_prima_forward_participativo=quantile(precio_cobertura_comprador_prima_fwdpart,0.05)
qv1_prima_forward_participativo
qv2_prima_forward_participativo


#HISTOGRAMA PRECIO CON ESTRATEGIA FORWARD PARTICIPATIVO SIN PRIMA
hist(precio_cobertura_comprador_noprima_fwdpart,breaks=60,col="gold",
     xlab="Precio estrategia largo call",ylab="Frecuencia", 
     main="Histograma de precios estrategia forward participativo sin prima")
abline(v=mean(precio_cobertura_comprador_noprima_fwdpart),lwd=3)
abline(v=quantile(precio_cobertura_comprador_noprima_fwdpart,0.05),lwd=3)
abline(v=quantile(precio_cobertura_comprador_noprima_fwdpart,0.95),lwd=3)

#HISTOGRAMA PRECIO CON ESTRATEGIA FORWARD PARTICIPATIVO CON PRIMA
hist(precio_cobertura_comprador_prima_fwdpart,breaks=60,col="gold",
     xlab="Precio estrategia forward participativo con prima",ylab="Frecuencia", 
     main="Histograma de precios estrategia forward participativo con prima",
     xlim=c(3400,3420))
abline(v=mean(precio_cobertura_comprador_prima_fwdpart),lwd=3)
abline(v=quantile(precio_cobertura_comprador_prima_fwdpart,0.05),lwd=3)
abline(v=quantile(precio_cobertura_comprador_prima_fwdpart,0.95),lwd=3)

ganar_forward=0
for(i in 1:length(st)){
  if (st[i]>3411.32){
    ganar_forward=1+ganar_forward
  }
}
print(ganar_forward)

prob_ganar_forward=(ganar_forward/length(st))*100

# 3. Estrategia collar

k2=k+10 

#Valoración opciones
vp_compensacion_call_3=vector()
vp_compensacion_put_3=vector()

for(i in 1:50000){
    vp_compensacion_call_3[i]=max(st_prima[i]-k2,0)*exp(-(r-rf)*deltaT)
  
    vp_compensacion_put_3[i]= max(k-st_prima[i],0)*exp(-(r-rf)*deltaT)
}

call_3=mean(vp_compensacion_call_3)  #Prima opcion call
call_3

put_3=mean(vp_compensacion_put_3)  #Prima opcion call
put_3

precio_cobertura_comprador_noprima_collar= vector()
precio_cobertura_comprador_prima_collar=vector()

for(i in 1:50000){
  precio_cobertura_comprador_noprima_collar[i]=abs(-st[i]+max(-k2+st[i],0)+min(st[i]-k,0))
  
  precio_cobertura_comprador_prima_collar[i]=abs(-st[i]+max(-k2+st[i],0)-call_3+min(st[i]-k,0)+put_3)
  
}

mean_noprima_collar= mean(precio_cobertura_comprador_noprima_collar)
mean_noprima_collar
mean_prima_collar=mean(precio_cobertura_comprador_prima_collar)
mean_prima_collar

#PERCENTILES ESTRATEGIA LARGO CALL
qv1_noprima_collar=quantile(precio_cobertura_comprador_noprima_collar,0.95)
qv2_noprima_collar=quantile(precio_cobertura_comprador_noprima_collar,0.05)
qv1_noprima_collar
qv2_noprima_collar

qv1_prima_collar=quantile(precio_cobertura_comprador_prima_collar,0.95)
qv2_prima_collar=quantile(precio_cobertura_comprador_prima_collar,0.05)
qv1_prima_collar
qv2_prima_collar

#HISTOGRAMA PRECIO CON ESTRATEGIA COLLAR SIN PRIMA
hist(precio_cobertura_comprador_noprima_collar,breaks=60,col=5,
     xlab="Precio estrategia collar",ylab="Frecuencia", 
     main="Histograma de precios estrategia collar sin prima")
abline(v=mean(precio_cobertura_comprador_noprima_collar),lwd=3)
abline(v=quantile(precio_cobertura_comprador_noprima_collar,0.05),lwd=2)
abline(v=quantile(precio_cobertura_comprador_noprima_collar,0.95),lwd=2)

#HISTOGRAMA PRECIO CON ESTRATEGIA COLLAR CON PRIMA
hist(precio_cobertura_comprador_prima_collar,breaks=60,col=5,
     xlab="Precio estrategia collar",ylab="Frecuencia", 
     main="Histograma de precios estrategia collar con prima")
abline(v=mean(precio_cobertura_comprador_prima_collar),lwd=3)
abline(v=quantile(precio_cobertura_comprador_prima_collar,0.05),lwd=2)
abline(v=quantile(precio_cobertura_comprador_prima_collar,0.95),lwd=2)

ganar_collar=0
for(i in 1:length(st)){
  if (st[i]>3416.375){
    ganar_collar=1+ganar_collar
  }
}
print(ganar_collar)

prob_ganar_collar=(ganar_collar/length(st))*100

# 4. Estrategia gaviota

k3=k-10#este va a ser el k1 del taller, el k2 es el k y el k3 es el k2 

#Valoración opciones
vp_compensacion_call_4=vector() #compra opción call
vp_compensacion_call_5=vector() #venta opción call
vp_compensacion_put_4=vector()


for(i in 1:50000){
  vp_compensacion_call_4[i]=max(st_prima[i]-k,0)*exp(-(r-rf)*deltaT)
  vp_compensacion_call_5[i]=max(-k2+st_prima[i],0)*exp(-(r-rf)*deltaT)
  vp_compensacion_put_4[i]= max(k3-st_prima[i],0)*exp(-(r-rf)*deltaT)
}

call_4=mean(vp_compensacion_call_4)  #Prima opcion call
call_4

call_5=mean(vp_compensacion_call_5)  #Prima opcion call
call_5

put_4=mean(vp_compensacion_put_4)  #Prima opcion call
put_4

precio_cobertura_comprador_noprima_gaviota= vector()
precio_cobertura_comprador_prima_gaviota=vector()

for(i in 1:50000){
  precio_cobertura_comprador_noprima_gaviota[i]=abs(-st[i]+min(k2-st_prima[i],0)+max(-k+st[i],0)+min(st[i]-k3,0))
  
  precio_cobertura_comprador_prima_gaviota[i]=abs(-st[i]+min(k2-st_prima[i],0)+call_5+max(-k+st[i],0)-call_4+min(st[i]-k3,0)+put_4)
  
}

mean_noprima_gaviota= mean(precio_cobertura_comprador_noprima_gaviota)
mean_noprima_gaviota
mean_prima_gaviota=mean(precio_cobertura_comprador_prima_gaviota)
mean_prima_gaviota

#PERCENTILES ESTRATEGIA LARGO CALL
qv1_noprima_gaviota=quantile(precio_cobertura_comprador_noprima_gaviota,0.95)
qv2_noprima_gaviota=quantile(precio_cobertura_comprador_noprima_gaviota,0.05)
qv1_noprima_gaviota
qv2_noprima_gaviota

qv1_prima_gaviota=quantile(precio_cobertura_comprador_prima_gaviota,0.95)
qv2_prima_gaviota=quantile(precio_cobertura_comprador_prima_gaviota,0.05)
qv1_prima_gaviota
qv2_prima_gaviota

ganar_gaviota=0
for(i in 1:length(st)){
  if (st[i]>3997.977){
    ganar_gaviota=1+ganar_gaviota
  }
}
print(ganar_gaviota)

prob_ganar_gaviota=(ganar_gaviota/length(st))*100

#HISTOGRAMA PRECIO CON ESTRATEGIA gaviota SIN PRIMA
hist(precio_cobertura_comprador_noprima_gaviota,breaks=60,col=5,
     xlab="Precio estrategia gaviota",ylab="Frecuencia", 
     main="Histograma de precios estrategia gaviota sin prima")
abline(v=mean(precio_cobertura_comprador_noprima_gaviota),lwd=3)
abline(v=quantile(precio_cobertura_comprador_noprima_gaviota,0.05),lwd=2)
abline(v=quantile(precio_cobertura_comprador_noprima_gaviota,0.95),lwd=2)

#HISTOGRAMA PRECIO CON ESTRATEGIA gaviota CON PRIMA
hist(precio_cobertura_comprador_prima_gaviota,breaks=60,col=5,
     xlab="Precio estrategia gaviota",ylab="Frecuencia", 
     main="Histograma de precios estrategia gaviota con prima")
abline(v=mean(precio_cobertura_comprador_prima_gaviota),lwd=3)
abline(v=quantile(precio_cobertura_comprador_prima_gaviota,0.05),lwd=3)
abline(v=quantile(precio_cobertura_comprador_prima_gaviota,0.95),lwd=3)

#COMPARACIÓN ESTRATEGIAS
Sin_prima=data.frame(sin_cobertura=st,largocall=precio_cobertura_comprador_noprima_largocall,
                   fwd_participativo=precio_cobertura_comprador_noprima_fwdpart,
                   collar=precio_cobertura_comprador_noprima_collar,
                   gaviota=precio_cobertura_comprador_noprima_gaviota)

boxplot(Sin_prima, xlab="Estrategias", ylab="Precios", col=c("deepskyblue1","pink1","darkslategrey","cyan3","yellow"))
legend("top",
       c("Sin cobertura",
         "Tradicional", 
         "Fwd Participativo", 
         "Collar", 
         "Gaviota"), fill=c("deepskyblue1","pink1","darkslategrey","cyan3","yellow"),
       text.font = 1, y.intersp = 0.4, bty = 'n', x.intersp = 0.1, text.width=1)

con_prima=data.frame(sin_cobertura=st,largocall=precio_cobertura_comprador_prima_largocall,
                     fwd_participativo=precio_cobertura_comprador_prima_fwdpart,
                     collar=precio_cobertura_comprador_prima_collar,
                     gaviota=precio_cobertura_comprador_prima_gaviota)

boxplot(con_prima, xlab="Estrategias", ylab="Precios", col=c("deepskyblue1","pink1","darkslategrey","cyan3","yellow"))
legend("top",
       c("Sin cobertura",
         "Tradicional", 
         "Fwd Participativo", 
         "Collar", 
         "Gaviota"), fill=c("deepskyblue1","pink1","darkslategrey","cyan3","yellow"),
       text.font = 1, y.intersp = 0.4, bty = 'n', x.intersp = 0.1, text.width=1)

hist(st,freq=F,ylim=c(0,0.003))
lines(density(precio_cobertura_comprador_prima_largocall))
lines(density(precio_cobertura_comprador_prima_fwdpart))
lines(density(precio_cobertura_comprador_prima_collar))
lines(density(precio_cobertura_comprador_prima_gaviota))

