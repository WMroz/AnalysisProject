#--------------------- PROJEKT 1 STATYSTYCZNA ANALIZA DANYCH----------------------

# przy wczytywaniu danych nalezy pamietac by nazwy wierszy odpowiadaly pierwszej kolumnie z pliku

dane <- WiktoriaMroz_pr_SAD_dane
# STATYSTYKI OPISOWE
summary(dane)
odch_stand <- c(sd(dane$age), sd(dane$goals), sd(dane$efficiency), sd(dane$shots_7m), sd(dane$steals))
odch_stand


# KORELACJA
round(cor(dane),3)


# WSPOLCZYNNIK ZMIENNOSCI
for(i in 1:ncol(dane)){
  print(colnames(dane[i]))
  print(sd(dane[,i]/mean(dane[,i])*100))
}


# OBSERWACJE ODSTAJACE
par(mfrow=c(3,2))
boxplot(dane$age, xlab="Age", col="peachpuff2",main="Wykres pudelkowy z wasem - wiek")
boxplot(dane$goals, xlab="Goals", col="peachpuff2",main="Wykres pudelkowy z wasem - gole")
boxplot(dane$efficiency, xlab="Efficiency", col="peachpuff2",main="Wykres pudelkowy z wasem - skutecznosc")
boxplot(dane$shots_7m, xlab="7m shots", col="peachpuff2",main="Wykres pudelkowy z wasem - rzuty na 7m")
boxplot(dane$steals, xlab="Steals", col="peachpuff2",main="Wykres pudelkowy z wasem - przechwyty")
par(mfrow=c(1,1))


# MODA
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
moda <- getmode(dane$age)
moda


# ZAMIANA ZMIENNEJ AGE NA STYMULANTE
for (j in 1:nrow(dane)){
  if(dane$age[j]==30){
    dane$age[j]=1
  }else if(dane$age[j]>30){
    dane$age[j]=1/(dane$age[j]-30+1)
  }else
    dane$age[j]=-1/(dane$age[j]-30-1)
}
dane$age <- round(dane$age,3)


# STANDARYZACJA DANYCH
dane_st <-as.data.frame(scale(dane))


#-------------------- PORZADKOWANIE LINIOWE -----------------------------------


# METODA STANDARYZOWANYCH SUM

dane_mss <- dane_st
for (j in 1:nrow(dane)){
  dane_mss$suma_rang[j] = (rowSums(dane_mss[j,1:5]))/5
}
minL=min(dane_mss$suma_rang)
maxL=max(dane_mss$suma_rang)
for (j in 1:nrow(dane)){
  dane_mss$wsk[j]=(dane_mss$suma_rang[j]-minL)/(maxL-minL)
}

dane_mss <- dane_mss[order(dane_mss$wsk, decreasing = TRUE),,drop=FALSE]


# METODA HELLWIGA

# wyznaczenie wzorca
wzorzec<-c(max(dane_st$age),max(dane_st$goals),max(dane_st$efficiency),max(dane_st$shots_7m),max(dane_st$steals))

# wyznaczenie odleglosci od wzorca
dane_wz <- dane_st
for(j in 1:nrow(dane)){
  dane_wz$age[j]=(dane_st$age[j]-wzorzec[1])^2
  dane_wz$goals[j]=(dane_st$goals[j]-wzorzec[2])^2
  dane_wz$efficiency[j]=(dane_st$efficiency[j]-wzorzec[3])^2
  dane_wz$shots_7m[j]=(dane_st$shots_7m[j]-wzorzec[4])^2
  dane_wz$matches_played[j]=(dane_st$steals[j]-wzorzec[5])^2
}

dane_wz$odleglosc <- 0
for(j in 1:nrow(dane)){
  dane_wz$odleglosc[j]=sqrt(rowSums(dane_wz[j,1:5]))
}


# odleglosc "mozliwie daleka"
odl_daleka <-c("srednia"=mean(dane_wz$odleglosc),"odchylenie"=sd(dane_wz$odleglosc),"d0"=(mean(dane_wz$odleglosc)+2*sd(dane_wz$odleglosc)))

wart_miary <- data.frame("hellwig"=dane_wz$odleglosc, row.names = rownames(dane))

# wartosc miary wg Hellwiga
for (j in 1:nrow(dane)){
  wart_miary$hellwig[j]=1-(dane_wz$odleglosc[j]/odl_daleka[3])
}

# sortowanie wynikow
wart_miary <- wart_miary[order(wart_miary$hellwig, decreasing = TRUE),,drop=FALSE]


# GRUPOWANIE WG SREDNIEJ

# grupowanie wg sredniej hellwig
df <- c("srednia"=mean(wart_miary$hellwig),"odchylenie"=sd(wart_miary$hellwig), "roznica"=mean(wart_miary$hellwig)-sd(wart_miary$hellwig),"suma"=sd(wart_miary$hellwig)+mean(wart_miary$hellwig) )
wart_miary$grupa <- 0

for (j in 1:nrow(dane)){
  if(wart_miary$hellwig[j]>=df[4]){
    wart_miary$grupa[j]=1
  }else if(wart_miary$hellwig[j]<df[4]&&wart_miary$hellwig[j]>=df[1]){
    wart_miary$grupa[j]=2
  }else if(wart_miary$hellwig[j]<df[1]&&wart_miary$hellwig[j]>=df[3]){
    wart_miary$grupa[j]=3
  }else
    wart_miary$grupa[j]=4
}


# grupowanie wg sredniej mss
df2 <- c("srednia"=mean(dane_mss$wsk),"odchylenie"=sd(dane_mss$wsk), "roznica"=mean(dane_mss$wsk)-sd(dane_mss$wsk),"suma"=sd(dane_mss$wsk)+mean(dane_mss$wsk) )
dane_mss$grupa <- 0

for (j in 1:nrow(dane)){
  if(dane_mss$wsk[j]>=df2[4]){
    dane_mss$grupa[j]=1
  }else if(dane_mss$wsk[j]<df2[4]&&dane_mss$wsk[j]>=df2[1]){
    dane_mss$grupa[j]=2
  }else if(dane_mss$wsk[j]<df2[1]&&dane_mss$wsk[j]>=df2[3]){
    dane_mss$grupa[j]=3
  }else
    dane_mss$grupa[j]=4
}


# STABILONOSC PORZADKOWANIA

cor(wart_miary$grupa, dane_mss$grupa, use = "everything",method = "kendall")



#--------------------------------ANALIZA SKUPIEN -------------------------------

# GRUPOWANIE PODZIALOWE

# METODA KMEANS

# wybór optymalnej liczby grup - metoda Lokciowa
library(purrr)

withinss<-map_dbl(1:10,function(k){
  gr2<-kmeans(x=dane_st,center=k,nstart=20)
  gr2$tot.withinss
})

library(ggplot2)

ggplot(data=data.frame(x=c(1:10), y=withinss), aes(x=x,y=y))+
  geom_line()+
  geom_point()



# K-MEANS 5 GRUP
library(psych)
dane_p <- dane
dane_p2 <- dane
p_gr5 <- kmeans(dane_st,5,nstart=20)
sort(p_gr5$cluster)


# porównanie statystyk w 5 grupach
dane_p$p_gr5 <- as.factor(p_gr5$cluster)
describeBy(dane_p[,-6],group=dane_p$p_gr5)


# K-MEANS 6 GRUP
p_gr6 <- kmeans(dane_st,6,nstart=20)
sort(p_gr6$cluster)


# porównanie statystyk w 6 grupach
dane_p$p_gr6 <- as.factor(p_gr6$cluster)
describeBy(dane_p[,1:5],group=dane_p$p_gr6)


# METODA PAM
library(cluster)

# PAM 5 GRUP
p_podz_p <- pam(dane_p2,5)

# porownanie statystyk w 5 grupach
dane_p2$p_podz_p <- as.factor(p_podz_p$clustering)
describeBy(dane_p2[,-6],group=dane_p2$p_podz_p)


# wykres goli od skutecznosci
ggplot(dane_p2,aes(x=goals,y=efficiency))+
  geom_text(aes(label=rownames(dane_p2), color=p_podz_p))+
  theme(legend.position = "None")


# GRUPOWANIE HIERARCHICZNE

# odleg³oœci euklidesa i minkowskiego
dl_eu <- dist(dane_st,method = "euclidean")
dl_mink <- dist(dane_st,method = "minkowski", 3)

# METODA WARDA
w_hier1 <- hclust(dl_eu,method = "ward.D")

w_hier2 <- hclust(dl_mink,method = "ward.D")

# METODA SREDNIEJ
w_hier3 <- hclust(dl_eu,method = "average")

w_hier4 <- hclust(dl_mink,method = "average")

# DENDOGRAMY
par(mfrow=c(2,2))
plot(w_hier1,main="odl. euklidesowa, metoda Warda")
plot(w_hier2,main="odl. minkowski, metoda Warda")
plot(w_hier3, main="odl. euklidesowa, metoda centroidy")
plot(w_hier4,main="odl. minkowski, metoda centroidy")
par(mfrow=c(1,1))


# wybór optymalnej ilosci grup - indeksy ocen jakosci klasyfikacji - odl. euklidesowa, metoda sredniej
library(clusterSim)
index2 <- data.frame(G1=0,G2=0,G3=0,S=0)
for (i in 2:10){
  cluster <- cutree(w_hier3,k=i)
  g1 <- index.G1(dane_st,cluster)
  g2 <- index.G2(dl_eu,cluster)
  g3 <- index.G3(dl_eu,cluster)
  s <- index.S(dl_eu,cluster)
  wiersz <- data.frame(G1=g1,G2=g2,G3=g3,S=s)
  index2 <- rbind(index2,wiersz)
}
index2 <- index2[-1,]
index2$k <- 2:10

par(mfrow=c(2,2))
plot(x=index2$k,y=index2$G1,main="G1",type="b",xlab="",ylab="")
plot(x=index2$k,y=index2$G2,main="G2",type="b",xlab="",ylab="")
plot(x=index2$k,y=index2$G3,main="G3",type="b",xlab="",ylab="")
plot(x=index2$k,y=index2$S,main="S",type="b",xlab="",ylab="")
par(mfrow=c(1,1))

# DENDOGRAMY - metoda Warda, odl. euklidesowa
library(dendextend)
dend_ward_euclidean <- color_branches(w_hier1,k=5)

dend_ward_euclidean %>%
  set("branches_lwd",2) %>%
  plot(main = "metoda warda, odl. euklidesowa - 5 grup")
abline(h=5.8,lty=2)


dend_ward_euclidean2 <- color_branches(w_hier1,k=6)

dend_ward_euclidean2 %>%
  set("branches_lwd",2) %>%
  plot(main = "metoda warda, odl. euklidesowa - 6 grup")
abline(h=3,lty=2)


# DENDOGRAM - metoda sredniej, odl. euklidesowa
dend_avg_euclidean <- color_branches(w_hier3,k=4)

dend_avg_euclidean %>%
  set("branches_lwd",2) %>%
  plot(main = "metoda sredniej, odl. euklidesowa - 4 grupy")
abline(h=2.85,lty=2)



#------------------------- SKALOWANIE WIELOWYMIAROWE ---------------------------

library(graphics)
library(rgl)

# macierz odl
odl <- dist(dane_st)


# KLASYCZNE SKALOWANIE - 2 wymiary
sww2 <- cmdscale(odl,k=2)
plot(sww2,xlab="wymiar 1", ylab="wymiar 2", xlim = c(-3,5),ylim = c(-2,3))
text(sww2, labels=rownames(dane),pos=1, col="coral4",cex=0.6)

# funkcja sSTRESS
stress <- function(d1,d2){
  sqrt(sum((d1-d2)^2)/sum(d1^2))
}

odl2 <- dist(sww2)

# STRESS dla klasycznego skalowania - 2 wymiary
stress(odl,odl2)


# KLASYCZNE SKALOWANIE - 2 wymiary
sww3 <-cmdscale(odl,k=3)
plot3d(sww3,xlab="wymiar 1", ylab="wymiar 2", zlab="wymiar 3")
text3d(sww3,texts=c("Nora Mork", "Jovanka Radicevic", "Anna Vyakhireva","Ekaterina Iliana", "Migyeong Lee", "Veronica Kristiansen", 
                    "Elin Hansson", "Laura Flippes", "Stine Bredal Oftedal", "Shio Fujii", "Nathalie Hagman", "Marta Lopez Herrero", 
                    "Estelle Nze Minko", "Danick Snelder", "Kyungmin Kang", "Camilla Herrem", "Lara Gonzalez Ortega", "Adriana Cardoso",
                    "Marit Jacobsen","Lois Abbingh", "Itana Grbic", "Ema Ramusovic", "Paline Coatanea", "Haruno Sasaki", "Dijana Mugosa",
                    "Liliana da Silva Venancio", "Reka Bordas", "Polina Kuznetsova", "Igner Smits", "Jinyi Kim"),col="coral4")

odl3 <- dist(sww3)
# STRESS dla klasycznego skalowania - 3 wymiary
stress(odl, odl3)


# METODA SAMMONA
library(MASS)

# 2 wymiary
sammon2 <- sammon(odl,k=2)
sammon2$points
sammon2$stress

plot(sammon2$points,xlab="wymiar 1", ylab="wymiar 2",xlim = c(-4,5),ylim = c(-2,3))
text(sammon2$points, labels=rownames(dane), pos=2,cex=0.6, col="coral4")


# SKALOWANIE A WYNIKI GRUPOWANIA
gr5 <- pam(dane_p2,5)
gr5$cluster

dane_wykres <- data.frame(sww2)
colnames(dane_wykres) <-c("x","y")
dane_wykres$gr5 <- gr5$cluster
dane_wykres
plot(dane_wykres[,1:2],xlab="wymiar 1", ylab="wymiar 2",xlim = c(-3,4),ylim = c(-2,3),pch=16)
text(dane_wykres[,1:2],col = dane_wykres$gr5, labels=rownames(dane), pos=2,cex=0.6)
