
## R tutorial 3 - Fertility
## student's version
## DEMO2002 - Population dynamics
## 17-Mar-2021

#### good practice is to load the package first ####

library(ggplot2)

#### data preparation ####

# complete by yourself
Mx<-read.table("../Assignment 1/DNKasfrRR.txt", header = TRUE, skip = 2) 

# complete by yourself
Mx$Age[Mx$Age=="12-"] <- "12"
  # complete by yourself
Mx$Age[Mx$Age=="55+"] <- "55"
  
Mx$Age <- as.numeric(Mx$Age)

#### calculate TFR for any given year ####

i <- 2019

Mx2019 <- Mx[Mx$Year==i, ]

TFR2019 <- 0

# complete by yourself, how to calculate TFR?
TFR2019 <- sum(Mx2019$ASFR)

#### loop to calculate TFR ####

Year <- unique(Mx$Year) 

TFR <- c()

for(i in Year){
  Fx <- Mx[Mx$Year==i,] 
  #Again how to calculate TFR?
  tfr <- sum(Fx$ASFR)
  TFR <- c(TFR,tfr)
}

TFR <- data.frame(Year=Year,TFR=TFR)

# For the TFR we just created

# add the x and y axis yourself
ggplot(TFR,aes(x=Year,y=TFR))+geom_line()

#### Period & Cohort Comparison ####

Mx2<-read.table("../Assignment 1/DNKasfrVV.txt", skip = 2, header = T)

# you can do this!
Mx2$ARDY[Mx2$ARDY=="12-"] <- "12"
  Mx2$ARDY[Mx2$ARDY=="55+"] <- "55"
  Mx2$ARDY <- as.numeric(Mx2$ARDY)

# make the birth year of the cohort numeric
Mx2$Cohort <- gsub("[+]","",Mx2$Cohort)
Mx2$Cohort <- gsub("[-]","",Mx2$Cohort)

Mx2$Cohort <- as.numeric(Mx2$Cohort)

#### We are gonna look at the Cohort TFR ####

Year2 <- 1916:2021

CTFR <- c()

for (i in Year2) {
  Fx <- Mx2[Mx2$Cohort==i,]
  #again how to calculate TFR?
  ctfr<- sum(Fx$ASFR)
  CTFR <- c(CTFR,ctfr)
}

CTFR <- data.frame(Year=1916:2021,CTFR=CTFR)

#### Plotting time! ####

ggplot(TFR,aes(x=Year,y=TFR,color="Period"))+
  geom_line()+
  # we have two data so we need to add a new data to plot. 
  geom_line(CTFR,mapping=aes(x=Year,y=CTFR,color="Cohort"))+
  scale_color_manual(values = c("navy","red"))

