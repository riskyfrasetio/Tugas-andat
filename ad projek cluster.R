library(readxl)
library(psych) ## factor analysis
library(DMwR) ## imputation
library(paran) ## determine number factor

## fungsi menghitung SST
SST=function(data,center){
  data=as.matrix(data)
  SS=matrix()
  for(i in 1:nrow(data)){
    SS[i]=(dist(rbind(data[i,],center)))^2
  }
  SST=sum(SS)
  return(SST)
}

## fungsi menghitung SSW
SSW=function(data.cluster,centroid.matrix){
  Within=function(data,centroid){
    data=as.matrix(data)
    W=c()
    for(i in 1:nrow(data)){
      W[i]=(dist(rbind(data[i,-ncol(data)],centroid)))^2
    }
    hasil=sum(W)
    return(hasil)
  }
  datacluster=vector("list",max(data.cluster[,ncol(data.cluster)]))
  Within.cluster=c()
  for (i in 1:max(data.cluster[,ncol(data.cluster)])){
    datacluster[[i]]=data.cluster[data.cluster[,ncol(data.cluster)]==i,]  
    Within.cluster[i]=Within(data=datacluster[[i]],centroid=centroid.matrix[i,])
  }
  hasil=sum(Within.cluster)
  return(hasil)
}

## menentukan metode cluster yang lebih baik
icd.rate=function(data,data.cluster,center,centroid.matrix){
  nilai.SSW=SSW(data.cluster,centroid.matrix)
  nilai.SST=SST(data,center)
  nilai.SSB=nilai.SST-nilai.SSW
  icd=1-nilai.SSB/nilai.SST
  return(icd)
}

## menentukan jumlah kluster optimal
pseudo.F=function(data,data.cluster,center,centroid.matrix){
  nilai.SSW=SSW(data.cluster,centroid.matrix)
  nilai.SST=SST(data,center)
  nilai.SSB=nilai.SST-nilai.SSW
  R2=nilai.SSB/nilai.SST
  n.cluster=max(data.cluster[,ncol(data.cluster)])
  n=nrow(data)
  pseudoF=(R2/(n.cluster-1))/((1-R2)/(n-n.cluster))
  return(pseudoF)
}


data=read_excel(choose.files(),sheet="Sheet1",col_names = F,col_types = c(rep("numeric",33)))
data=as.data.frame(data)
data=data[,-c(1,2)]
str(data)
row.names(data)=c(paste0("lab",1:nrow(data)))
names(data)=c(paste0("x",1:ncol(data)))

## scalling
scalling <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))}
dataku <- as.data.frame(lapply(data,scalling))
dataku$fakultas=c(rep(1,30),rep(2,16),rep(3,2),rep(4,10),rep(5,49),6)
dataku$fakultas=as.factor(dataku$fakultas)
levels(dataku$fakultas)=c("FTSP","FTIF","FBMT","FV","FTI","FTE")
dataku=dataku[-c(71,100,106),]
datakuFTSP=dataku[dataku$fakultas=="FTSP",]
datakuFTIF=dataku[dataku$fakultas=="FTIF",]
datakuFBMT=dataku[dataku$fakultas=="FBMT",]
datakuFV=dataku[dataku$fakultas=="FV",]
datakuFTI=dataku[dataku$fakultas=="FTI",]
datakuFTE=dataku[dataku$fakultas=="FTE",]

## cek missing value
sum(is.na(datakuFTSP))
sum(is.na(datakuFTIF))
sum(is.na(datakuFBMT))
sum(is.na(datakuFV))
sum(is.na(datakuFTI))
sum(is.na(datakuFTE))

## imputasi
datakuFTSP.input=knnImputation(datakuFTSP[,-ncol(datakuFTSP)],k=5,scale=F)
datakuFTIF.input=knnImputation(datakuFTIF[,-ncol(datakuFTIF)],k=5,scale=F)
datakuFTI.input=knnImputation(datakuFTI[,-ncol(datakuFTI)],k=5,scale=F)

data.olah=rbind(datakuFTSP.input,datakuFTIF.input,datakuFBMT[,-32],datakuFV[,-32],datakuFTI.input,datakuFTE[,-32])


## factor analysis
library(paran)
library(corrplot)
corrplot(cor(data.olah), tl.col='black', tl.cex=.75) 

## metode parallel
paran(data.olah, iterations = 100, centile = 0, quietly = FALSE, 
      status = TRUE, all = TRUE, cfa = TRUE, graph = TRUE, color = TRUE, 
      col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE, 
      file = "", width = 640, height = 640, grdevice = "png", seed = 0)

# berdasarkan kriteria MAP
VSS(cor(data.olah),n.obs=nrow(data.olah))
## digunakan 5 faktor

##menggunakan fungsi fa package psych
faktor=fa(cor(data.olah),nfactors=5,n.obs=nrow(data.olah),fm="ml",rotate="varimax")
faktor$loadings

faktor2=factanal(data.olah,factors=5,scores="regression",rotation="varimax")
faktor2$loadings

data.faktor=faktor2$scores

## clustering menggunakan kmeans
pseudoF=c()
icd=c()
for (i in 2:10){
klus=kmeans(data.faktor,i,iter.max = 1000)
data.cluster=cbind(data.faktor,klus$cluster)
centroid.matrix=klus$centers
center=apply(data.faktor,2,mean)
pseudoF[i-1]=pseudo.F(data.faktor,data.cluster,center,centroid.matrix)
icd[i-1]=icd.rate(data.faktor,data.cluster,center,centroid.matrix)
}
plot(2:5,pseudoF,main="jumlah kluster optimum")


klus=kmeans(data.faktor,2)
pairs(data.faktor,col=klus$cluster)

##k medoids
library(cluster)
set.seed(11)
pseudoF=c()
icd=c()
for (i in 2:10){
  k.med=pam(data.faktor,diss=F,k=i,metric="euclidean")
  data.cluster=cbind(data.faktor,k.med$clustering)
  centroid.matrix=k.med$medoids
  center=pam(data.faktor,diss=F,k=1,metric="euclidean")$medoids
  pseudoF[i-1]=pseudo.F(data.faktor,data.cluster,center,centroid.matrix)
  icd[i-1]=icd.rate(data.faktor,data.cluster,center,centroid.matrix)
}

k.med=pam(data.faktor,diss=F,k=3,metric="euclidean")
k.med$clustering
k.med$medoids
pairs(data.faktor,col=k.med$clustering)


## clustering menggunakan fuzzy
## fuzzy k means 1.1
library(e1071)
pseudoF=c()
icd=c()
for (i in 2:10){
  fklus=cmeans(data.faktor,2,iter.max=1000,method="cmeans",m=1.2)
  data.cluster=cbind(data.faktor,fklus$cluster)
  centroid.matrix=fklus$centers
  center=apply(data.faktor,2,mean)
  pseudoF[i-1]=pseudo.F(data.faktor,data.cluster,center,centroid.matrix)
  icd[i-1]=icd.rate(data.faktor,data.cluster,center,centroid.matrix)
}
plot(2:5,pseudoF,main="jumlah kluster optimum")




library(fclust)
pseudoF=c()
icd=c()
for (i in 2:10){
  f=FKM(data.faktor,k=i,m=2)
  data.cluster=cbind(data.faktor,f$clus[,1])
  centroid.matrix=Hraw(f$X,f$H)
  pseudoF[i-1]=pseudo.F(data.faktor,data.cluster,centroid.matrix)
  icd[i-1]=icd.rate(data.faktor,data.cluster,centroid.matrix)
}

f=FKM(data.faktor,k=3,m=2)
f$clus[,1]
pairs(data.faktor,col=f$clus[,1])
##calculate centroid
f$Hraw=Hraw(f$X,f$H)

## fuzzy k-medoids
for (i in 2:10){
  g=FKM.med(data.faktor,k=i,m=1.2)
  data.cluster=cbind(data.faktor,g$clus[,1])
  centroid.matrix=Hraw(g$X,g$H)
  pseudoF[i-1]=pseudo.F(data.faktor,data.cluster,centroid.matrix)
  icd[i-1]=icd.rate(data.faktor,data.cluster,centroid.matrix)
}

g=FKM.med(data.faktor,k=3,m=2)
g$clus[,1]
pairs(data.faktor,col=g$clus[,1])
g$Hraw=Hraw(g$X,g$H)


## polinomial fuzzy k means
for (i in 2:10){
  h=FKM.pf(data.faktor,k=i)
  data.cluster=cbind(data.faktor,h$clus[,1])
  centroid.matrix=Hraw(h$X,h$H)
  pseudoF[i-1]=pseudo.F(data.faktor,data.cluster,centroid.matrix)
  icd[i-1]=icd.rate(data.faktor,data.cluster,centroid.matrix)
}

h=FKM.pf(data.faktor,k=3)
h$clus[,1]
h$Hraw=Hraw(h$X,h$H)
pairs(data.faktor,col=h$clus[,1])
plot(h,v1v2=c(1,3),colclus=c("black","red","green"))

### simulasi data
library(clusterGeneration)
set.seed(1000)
simul.data=genRandomClust(numClust = 2,sepVal = 0.01,numNonNoisy = 5,numOutlier=0,numReplicate = 1,clustszind = 3,clustSizes = c(25,25))
data.pake=simul.data[[3]][[1]]

#kmean
klus=kmeans(data.pake,2,iter.max = 1000)
data.cluster=cbind(data.pake,klus$cluster)
centroid.matrix=klus$centers
center=apply(data.pake,2,mean)
WSS=SSW(data.cluster,centroid.matrix)
WSS
icd=icd.rate(data.pake,data.cluster,center,centroid.matrix)
icd
pseudo.f=pseudo.F(data.pake,data.cluster,center,centroid.matrix)
pseudo.f

## k medoid
k.med=pam(data.pake,diss=F,k=2,metric="euclidean")
data.cluster=cbind(data.pake,k.med$clustering)
centroid.matrix=k.med$medoids
center=pam(data.pake,diss=F,k=1,metric="euclidean")$medoids
WSS=SSW(data.cluster,centroid.matrix)
WSS
icd=icd.rate(data.pake,data.cluster,center,centroid.matrix)
icd
pseudo.f=pseudo.F(data.pake,data.cluster,center,centroid.matrix)
pseudo.f

 ## fuzzy k mean m=1.2
fu=cmeans(data.pake,2,iter.max=1000,method="cmeans",m=1.2)
data.cluster=cbind(data.pake,fu$cluster)
centroid.matrix=fu$center
center=apply(data.pake,2,mean)
WSS=SSW(data.cluster,centroid.matrix)
WSS
icd=icd.rate(data.pake,data.cluster,center,centroid.matrix)
icd
pseudo.f=pseudo.F(data.pake,data.cluster,center,centroid.matrix)
pseudo.f

## fuzzy k mean m=2.5
fu=cmeans(data.pake,2,iter.max=1000,method="cmeans",m=2.5)
data.cluster=cbind(data.pake,fu$cluster)
centroid.matrix=fu$center
center=apply(data.pake,2,mean)
WSS=SSW(data.cluster,centroid.matrix)
WSS
icd=icd.rate(data.pake,data.cluster,center,centroid.matrix)
icd
pseudo.f=pseudo.F(data.pake,data.cluster,center,centroid.matrix)
pseudo.f

## fuzzy k mean m=5
fu=cmeans(data.pake,2,iter.max=1000,method="cmeans",m=5)
data.cluster=cbind(data.pake,fu$cluster)
centroid.matrix=fu$center
center=apply(data.pake,2,mean)
WSS=SSW(data.cluster,centroid.matrix)
WSS
icd=icd.rate(data.pake,data.cluster,center,centroid.matrix)
icd
pseudo.f=pseudo.F(data.pake,data.cluster,center,centroid.matrix)
pseudo.f
 


## fuzzy c mean m=1.2
 f=FKM(data.pake,k=2,m=1.2)
 data.cluster=cbind(data.pake,f$clus[,1])
 centroid.matrix=Hraw(f$X,f$H)
 WSS=SSW(data.cluster,centroid.matrix)
 WSS
 simul.data$memList
 f$clus[,1]
 table(f$clus[,1],simul.data$memList[[1]])
 
 ## fuzzy c mean m=2.5
 f=FKM(data.pake,k=2,m=2.5)
 data.cluster=cbind(data.pake,f$clus[,1])
 centroid.matrix=Hraw(f$X,f$H)
 WSS=SSW(data.cluster,centroid.matrix)
 WSS
 simul.data$memList
 f$clus[,1]
 table(f$clus[,1],simul.data$memList[[1]])

 ## fuzzy k medoids
 g=FKM.med(data.pake,k=2,m=1.2)
 data.cluster=cbind(data.pake,g$clus[,1])
 centroid.matrix=Hraw(g$X,g$H)
 WSS=SSW(data.cluster,centroid.matrix)
 WSS
 simul.data$memList
 g$clus[,1]
 table(g$clus[,1],simul.data$memList[[1]])
 
 g=FKM.med(data.pake,k=2,m=2.5)
 data.cluster=cbind(data.pake,g$clus[,1])
 centroid.matrix=Hraw(g$X,g$H)
 WSS=SSW(data.cluster,centroid.matrix)
 WSS
 simul.data$memList
 g$clus[,1]
 table(g$clus[,1],simul.data$memList[[1]])
 
 ## polinomial fuzzy
 h=FKM.pf(data.pake,k=2,b=0.1)
 data.cluster=cbind(data.pake,h$clus[,1])
 centroid.matrix=Hraw(h$X,h$H)
 WSS=SSW(data.cluster,centroid.matrix)
 WSS
 simul.data$memList
 h$clus[,1]
 table(h$clus[,1],simul.data$memList[[1]])
