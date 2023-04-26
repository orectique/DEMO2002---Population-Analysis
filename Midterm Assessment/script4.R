
## DEMO2002

## Wen's version

## In this program we will be creating a life table

## we download the HMD data for Australian males life tables 
## read qx and try to reproduce the original lx and dx 

library(ggplot2)

options(scipen=5)



#### or everything as a function

LifeTable<-function(qx){
  
  lx<-100000
  
  px<-1-qx
  
  for(y in 1:110){
    lx[y+1]<- lx[y] * px[y]
  }
  
  dx <- lx*qx
  
  Age<-0:110
  
  ALL<-data.frame(Age = Age, qx = qx,
                  lx = lx,dx = dx)
  
  return(ALL)
}
