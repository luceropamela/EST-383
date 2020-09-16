###### TAREA 1 EST-383 ########

# De los reciprocos  de los enteros impares, con signos alterados
i<-1
c<-2
k<-0.0001
sy<-1
vec<-NULL
while(sy>k){
  if(i%%2!=0){
    n<-((1/i)*(-1)^c)
    vec<-c(vec,n)
    s<-sum(vec)
    sy<-abs(s-pi/4)
    c<-c+1
  }
  i<-i+1
}
cat("la suma total de la conevregencia es:",fill = T)
s
cat("con una tolerancia de k:",fill = T)
k
cat("esta converge a:",fill = T)
pi/4

# De los reciprocos de los numeros triangulares 
j<-0
vect<-NULL
c<-1
s<-0
k<-0.01
l<-1
while (l>k) {
  j<-j+c
  vect<-c(vect,(1/j))
  s<-sum(vect)
  c<-c+1
  l<-abs(2-s)
}
cat("la suma total de la conevregencia es:",fill = T)
s
cat("esta converge a:",fill = T)
2

# De los reciprocos de los sucesivos factoriales n!
i<-0
vec<-NULL
k<-0.00001
cond<-1
while (cond>k) {
  vec<-c(vec,(1/factorial(i)))
  s<-sum(vec)
  i<-i+1
  cond<-abs(s-exp(1))
}
cat("la suma total de la conevergencia es:",fill = T)
s
cat("esta converge a:",fill = T)
exp(1)
# De los reciprocos de los sucesivos cuadrados perfectos
i<-0
l<-0
cond<-1
vec<-NULL
k<-0.00001
while(cond>k){
  c<-(2*l)+1
  i<-i+c
  vec<-c(vec,1/i)
  s<-sum(vec)
  l<-l+1
  cond<-abs(s-(pi^2)/6)
}
cat("la suma total de la conevergencia es:",fill = T)
s
cat("esta converge a:",fill = T)
(pi^2)/6

# De los reciprocos de las potencias 2
i<-1
vect<-NULL
k<-0.00001
cond<-1
while (cond>k) {
  vect<-c(vect,1/i)
  i<-i+i
  s<-sum(vect)
  cond<-abs(2-s)
}
cat("la suma total de la conevergencia es:",fill = T)
s
cat("esta converge a:",fill = T)
2


# De los reciprocos de las potencias de 2 con signos alterados 
i<-1
vect<-NULL
k<-0.00001
cond<-1
c<-2
while (cond>k) {
  vect<-c(vect,((1/i)*(-1)^c))
  i<-i+i
  c<-c+1
  s<-sum(vect)
  cond<-abs((2/3)-s)
}
cat("la suma total de la conevergencia es:",fill = T)
s
cat("esta converge a:",fill = T)
2/3

# De los reciprocos de los naturales con signos alterados
i<-1
vect<-NULL
k<-0.00001
cond<-1
c<-2
while (cond>k) {
  vect<-c(vect,((1/i)*(-1)^c))
  s<-sum(vect)
  i<-i+1
  c<-c+1
  cond<-abs(log(2)-s)
}

cat("la suma total de la conevergencia es:",fill = T)
s
cat("esta converge a:",fill = T)
log(2)
