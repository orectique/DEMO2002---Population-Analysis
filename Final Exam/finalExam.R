marriage <- read.table("Marriage.txt", skip = 1, header = T, sep = ',')

marriage$Mid.points <- c(17.5, 22, 27, 32, 37, 42, 47, 75)

MAm.Female <- marriage$Females %*% marriage$Mid.points / sum(marriage$Females)

MA.m.FemaleSS <- marriage$Same.sex.females %*% marriage$Mid.points / sum(marriage$Same.sex.females)

MA.m.Male <- marriage$Males %*% marriage$Mid.points / sum(marriage$Males)

MA.m.MaleSS <- marriage$Same.sex.males %*% marriage$Mid.points / sum(marriage$Same.sex.males)

marriage[c("Age.group", "Mid.points")]

mig <- read.table('femalesNSW2018.txt', header = T)

mig$joint <- mig$NSW.to.RAU + mig$Mx

LifeTableMx<-function(mx,sex){
  
  N<-length(mx)
  
  ax<-rep(0.5,N) #this 
  
  if(sex=="m"){
    ax[1]<-ifelse(mx[1]<0.107,0.045+mx[1]*2.684,0.330)}
  if(sex=="f"){
    ax[1]<-ifelse(mx[1]<0.107,0.053+2.800*mx[1],0.350)
  }
  
  # We need the functions to calculate ex from qx
  
  qx <- mx/(1 + (1 - ax)*mx)
  qx[N] <- 1
  
  px<-1-qx
  
  lx<-100000
  
  for(y in 1:(N-1)){          
    
    lx[y+1] <- lx[y]*px[y]
  }
  
  dx <- lx*qx
  
  Lx <- lx[-1] + ax[-N]*dx[-N] 
  
  Lx[N]<-ifelse(mx[N]>0,lx[N]/mx[N],0)                  
  
  Tx<-c()
  
  for(y in 1:N){
    # Calculate Tx
    Tx[y] <- sum(Lx[y:N])
  }
  
  ex <- Tx/lx 
  
  Age<-0:100              
  ALL<-data.frame(Age,mx,lx,dx,Lx,Tx,ex)
  return(ALL)
}

ltMx <- LifeTableMx(mig$Mx, 'f')

ltJoint <- LifeTableMx(mig$joint, 'f')

library(ggplot2)

ggplot() + 
  geom_line(aes(x = ltMx$Age, y = ltMx$Lx/100000, color = "Death")) + 
  geom_line(aes(x = ltJoint$Age, y = ltJoint$Lx/100000, color = "Migration + Death")) + 
  labs(x = "Age", y = "Survivors", title = "Survivorship (Death and Migration)", colour = "Measure") +
  xlim(c(0, 99))

ggplot() + 
  geom_line(aes(x = ltMx$Age, y = ltMx$ex + ltMx$Age, color = "Death")) + 
  geom_line(aes(x = ltJoint$Age, y = ltJoint$ex + ltJoint$Age, color = "Migration + Death")) + 
  labs(x = "Age", y = "Number of Years", title = "Life Expectancy at Birth - NSW (Death and Migration)", colour = "Measure") +
  xlim(c(0, 99))



asfr <- read.table("AUSfertilityRates1975.txt", header = T)

Names<-c("USA")
Names2<-c("United States of America")

YEAR<-1975

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
Sx<-ifelse(l$Lx[-111] > 0, l$Lx[-1]/l$Lx[-111], 0)
Sxm<-ifelse(lm$Lx[-111] > 0, lm$Lx[-1]/lm$Lx[-111], 0)

### L111 and L111m the survival for the last two age groups 
### (one number each)

### Here you need to complete this yourself.
L111<-ifelse(l$Tx[110] > 0, l$Tx[111]/l$Tx[110], 0)      
L111m<-ifelse(lm$Tx[110] > 0, lm$Tx[111]/lm$Tx[110], 0)

### (replace the last value in Sx and Sxm)
Sx[110]<-L111
Sxm[110]<-L111m

#### Fertility and Births ####

### Here you need to complete this yourself.
Fe<-c(rep(0, 15), asfr$ASFR, rep(0, (110-49)))

### line1: a combination of fertility and survival information
### for the first row of the matrix which returns the number 
### of babies when multiplied by the population it is also (110 long ### i.e. skip the last value)

B<-read.table("Births.txt",header=TRUE,fill=TRUE,skip=1,as.is=TRUE) # birth
B<-B[B$Year==YEAR,] # birth in YEAR(2000)

### Here you need to complete this yourself.
SRB = B$Male/B$Female

### Here you need to complete this yourself.
L1 = l$Lx[1]/(2 * l$lx[1])
L1m = lm$Lx[1]/(2 * lm$lx[1])

### Here you need to complete this yourself.
k = L1/(1 + SRB)# this is for female babies

km = (L1m*SRB)/(1 + SRB)# for estimating male babies

line1 = (Fe[-1]*Sx + Fe[-111])# this is for female survivor and baby survivors

#### Leslie matrix ####

### Here you need to complete this yourself.

Mf = cbind(rbind(line1, diag(Sx)), c(rep(0, 110), L111))

Mm = cbind(rbind(rep(0, length(line1)), diag(Sxm)), c(rep(0, 110), L111m))

#### Population ####

Pop<-read.table("Population.txt",
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
  
  pop[(year-1),] <- pop[(year-1),] + Mig$Female*0.5# The migration part comes here
  pop[year,]<-t(Mf%*%pop[(year-1),])
  tot_birth <- pop[year,1]
  pop[year,1] <- tot_birth*k
  pop[year,] <- pop[year,] + Mig$Female*0.5# The migration part comes here
  
  popm[(year-1),] <- popm[(year-1),] + Mig$Male*0.5# The migration part comes here
  popm[year,]<-c(tot_birth*km,t(Mm%*%popm[(year-1),])[-1])
  popm[year,] <- popm[year,] + Mig$Male*0.5# The migration part comes here
  
  
  To<-pop[year,]+popm[year,]
  
  Proj_Pop_temp <- data.frame(Year=(year+YEAR-1),Age=0:110,
                              Female=pop[year,],Male=popm[year,],
                              Total=To) # Compile data.frame HMD style
  
  Proj_Pop <- rbind(Proj_Pop, Proj_Pop_temp)
}



#### Plot the results as a pop pyramid ####

Pop_total = c()

for (year in unique(Proj_Pop$Year)){
  # write the loop yourself. 
  
  Pop1 <- Proj_Pop[Proj_Pop$Year==year,]
  
  tot <- sum(Pop1$Total)
  
  Pop1$pctf <- Pop1$Female/tot*100# equation to calculate data for female
  Pop1$pctm <- Pop1$Male/tot*-100# equation to calculate data for male
  
  PopF <- Pop1[,c(1,2,6)]
  PopF$Sex <- "Female"
  names(PopF)[3] <- "Percentage"
  
  PopM <- Pop1[,c(1,2,7)]
  PopM$Sex <- "Male"
  names(PopM)[3] <- "Percentage"
  
  Pop1 <- rbind(PopF,PopM)
  
  Pop_total <- rbind(Pop_total, Pop1)
}



