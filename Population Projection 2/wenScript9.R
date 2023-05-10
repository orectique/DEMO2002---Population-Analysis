
## R tutorial 9 - Population projection
## DEMO2002 
## 2023

library(ggplot2)

#### Parameters ####

Names<-c("USA")
Names2<-c("United States of America")

YEAR<-2018

#### Mortality ####

l<-read.table("fltper_1x1.txt",
              header=TRUE,fill=TRUE,skip=1)  # female life table
lm<-read.table("mltper_1x1.txt",
               header=TRUE,fill=TRUE,skip=1) # male life table

### Sx and Sxm: survival ratios for females and males respectively 
### to run the program they should have a length of (110 not 111)!!! 
### you should get them from the corresponding life tables from the 
### Human Mortality Database 

l<-l[l$Year==YEAR,]
lm<-lm[lm$Year==YEAR,]

### Here you need to complete this yourself.
Sx<-ifelse()
Sxm<-ifelse()

### L111 and L111m the survival for the last two age groups 
### (one number each)

### Here you need to complete this yourself.
L111<-ifelse()      
L111m<-ifelse()

### (replace the last value in Sx and Sxm)
Sx[110]<-L111
Sxm[110]<-L111m

#### Fertility and Births ####

f0<-read.table("Week 10 Projection II/asfrRR.txt",
               header=TRUE,fill=TRUE,skip=2) #period asfr by year and age (Lexis squares) for all countries

f<-f0[f0$Year==YEAR,]  

### Here you need to complete this yourself.
Fe<-c()

### line1: a combination of fertility and survival information
### for the first row of the matrix which returns the number 
### of babies when multiplied by the population it is also (110 long ### i.e. skip the last value)

B<-read.table("Week 10 Projection II/Births.txt",header=TRUE,fill=TRUE,skip=1,as.is=TRUE) # birth
B<-B[B$Year==YEAR,] # birth in YEAR(2000)

### Here you need to complete this yourself.
SRB

### Here you need to complete this yourself.
L1
L1m

### Here you need to complete this yourself.
k # this is for female babies

km # for estimating male babies

line1# this is for female survivor and baby survivors

#### Leslie matrix ####

### Here you need to complete this yourself.

Mf

Mm

#### Population ####

Pop<-read.table("Week 10 Projection II/Population.txt",
                header=TRUE,fill=TRUE,skip=1) # population

#### Migration ####

Migration_est <- function(Pop,mlt,flt,year){
  
  Sx_m <- mlt[mlt$Year==year,]
  Sx_m <- ifelse(Sx_m$Lx[-111]>0,Sx_m$Lx[-1]/Sx_m$Lx[-111],0)
  
  Sx_f <- flt[flt$Year==year,]
  Sx_f <- ifelse(Sx_f$Lx[-111]>0,Sx_f$Lx[-1]/Sx_f$Lx[-111],0)
  
  Pop_diff_f <- 
    Pop[Pop$Year==year+1,]$Female[-1] - 
    Pop[Pop$Year==year,]$Female[-111]*Sx_f
  Pop_diff_f <- c(Pop_diff_f,0)
  Pop_diff_f <- (Pop_diff_f[-1]+Pop_diff_f[-111])/2
  Pop_diff_f <- c(Pop_diff_f,0)
  
  Pop_diff_m <- 
    Pop[Pop$Year==year+1,]$Male[-1] - 
    Pop[Pop$Year==year,]$Male[-111]*Sx_m
  Pop_diff_m <- c(Pop_diff_m,0)
  Pop_diff_m <- (Pop_diff_m[-1]+Pop_diff_m[-111])/2
  Pop_diff_m <- c(Pop_diff_m,0)
  
  Mig <- data.frame(
    age = 0:110,
    Female = Pop_diff_f,
    Male = Pop_diff_m
  )
  
  return(Mig)
}

Mig <- Migration_est(Pop,mlt=lm,flt=l,YEAR)

### Ipop and Ipopm the base population of females and males 
### respectively (111 entries each)

Ipop<-as.numeric(Pop[Pop$Year==YEAR,]$Female)
Ipopm<-as.numeric(Pop[Pop$Year==YEAR,]$Male)

#### Projection ####

Proj<-100			# let's look at a projection for 50 years

pop<-matrix(0,Proj,length(Ipop)) 	
# creates a matrix whit each new line 
# for each new year of the female population

popm<-matrix(0,Proj,length(Ipopm))   # same for males

pop[1,]<- Ipop  	 #initialize the first row of female population
colnames(pop)=0:110
rownames(pop)=1:Proj

popm[1,]<- Ipopm   #initialize the first row of male population
colnames(popm)=0:110
rownames(popm)=1:Proj

Proj_Pop <- c()

for (year in 2:Proj){
  
  pop[(year-1),] <- pop[(year-1),] # The migration part comes here
  pop[year,]<-t(Mf%*%pop[(year-1),])
  tot_birth <- pop[year,1]
  pop[year,1] <- tot_birth*k
  pop[year,] <- pop[year,] # The migration part comes here
  
  pop[(year-1),] <- popm[(year-1),] # The migration part comes here
  popm[year,]<-c(tot_birth*km,t(Mm%*%popm[(year-1),])[-1])
  popm[year,] <- popm[year,] # The migration part comes here
  
  
  To<-pop[year,]+popm[year,]
  
  Proj_Pop_temp <- data.frame() # Compile data.frame HMD style
  
  Proj_Pop <- rbind(Proj_Pop, Proj_Pop_temp)
}

#### Plot the results as a pop pyramid ####