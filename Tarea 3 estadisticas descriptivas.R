rm(list = ls())
## ----------------- TAREA 3 EST-383 ------------------------------------
#Crear una función que dado un vector de números:

#1. Detecte si es numérico o no, si no es numérico saque un mensaje de error
#2. Si es numérico calcule sus medidas de tendencia central: media, mediana, media geométrica, media armónica y media cuadrática, 
#3. Dispersión: varianza, desviación estándar, rango, rango intercuartil, coeficiente de variación y desviación media 
#4. Forma: asimetría Pearson y Fisher, Kurtosis de Pearson y Fisher y los deciles 
#5. Devuelva en una lista las distintas medidas.



tendencia<-function(x){
  if(is.numeric(x)){
    n<-length(x)
    # media
    media<-sum(x)/n
    # mediana
    y<-sort(x)
    if(n%%2==0){
      mediana<-(y[n/2]+y[n/2+1])/2
    } else {
      mediana<-y[ceiling(n/2)]
    }
    # media geometrica
    mg<-prod(x)^(1/n)
    # media armonica
    ma<-n/sum(1/x)
    # media cuadratica
    mc<-sum((x^2)/n)^(1/2)
    
    ### Dispercion
    # varianza 
    vari<-var(x)
    # desviacion estandar
    de<-sd(x)
    # rango
    rg<-max(x)-min(x)
    # rango intercuartil
    y<-sort(x)
    q<-quantile(y,c(0.25,0.5,0.75))
    rq<-q[3]-q[1]
    # coeficiente de variacion
    cv<-sd(x)/mean(x)
    # desviacion media
    dm<-sum(abs(x-mean(x)))/n
    ### forma
    
    #asimetria de pearson
    ap<-(3*(mean(x)-median(x)))/sd(x)
    
    # asimetria de fisher
    af<-((sum((x-mean(x)^3)))/n)/(sd(x)^3)
    
    # curtosis
    curt<-(1/n)*sum(((x-mean(x))/sd(x))^4)
    
    # deciles
    y<-sort(x)
    dec<-quantile(y,seq(0,1,0.1))
    
  } else{
    print("El vector introducido no es un vector numerico")
  }
  return(list(media=media,mediana=mediana,media_geometrica=mg,media_armonica=ma,
              media_cuadratica=mc,varianza=vari,desviacion_estandar=de,
              rango=rg,rango_intercuartil=rq,coeficiente_variacion=cv,
              desviacion_media=dm,asimetria_pearson=ap,asimetria_fisher=af,
              curtosis=curt,deciles=dec))
}
x<-1:100
tendencia(x)
