##
## DEMO2002 - Life table II
##

#### Data preparation ####

Mx <- read.table("mltper_1x1.txt",
                 header=TRUE,fill=TRUE,skip=2)[,1:3]

Mx$Age[Mx$Age == "110+"] <- 110# change the "110+" to "110"
  Mx$Age <- as.numeric(Mx$Age)# convert to numeric value
  
  Mx2 <-read.table("mltcoh_1x1.txt",
                   header=TRUE,fill=TRUE,skip=2)[,1:3]

Mx2$Age[Mx2$Age == "110+"] <- 110# same for cohort data
  Mx2$Age <- as.numeric(Mx2$Age)# same for cohort data
  
  # change mx to numeric as well
  Mx2$mx <- as.numeric(ifelse(Mx2$mx==".","0",Mx2$mx))
  
Mx$Year <- as.numeric(Mx$Year)
Mx2$Year <- as.numeric(Mx2$Year)

#### We do it for one year #####

mx <- Mx[Mx$Year==2020,]$mx

sex <- "m"

N<-length(mx)

ax<-rep(0.5,N) 

if(sex=="m"){
  ax[1]<-ifelse(mx[1]<0.107,0.045+mx[1]*2.684,0.330)}
if(sex=="f"){
  ax[1]<-ifelse(mx[1]<0.107,0.053+2.800*mx[1],0.350)
}

# how to calculate qx for each age
# qx for last age group?
qx <- mx/(1 + (1 - ax)*mx)
qx[N] <- 1

px<-1-qx

lx<-100000

for(y in 1:(N-1)){          
  # write this loop yourself, we covered it last week
  lx[y+1] <- lx[y]*px[y]
}

dx <- lx*qx# calculate dx

Lx <- lx[-1] + ax[-N]*dx[-N] #calculate Lx, think in terms of vectors in calculation

Lx[N]<-ifelse(mx[N]>0,lx[N]/mx[N],0)                  

Tx<-c()

for(y in 1:N){
  # Calculate Tx
  Tx[y] <- sum(Lx[y:N])
}

ex <- Tx/lx # do it yourself

ex[1]

#### Putting it into a function ####

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
  
  Age<-0:110              
  ALL<-data.frame(Age,mx,lx,dx,Lx,Tx,ex)
  return(ALL)
}

#### loop for period life expectancy ####

LT_p <- c()

#You need to complete the loop function before running it
for (x in unique(Mx$Year)){
  mx <- Mx[Mx$Year == x,]$mx# specify mx within the loop  
  lt_x <- LifeTableMx(mx, 'm')
  lt_x$Year <- x
  LT_p <- rbind(LT_p,lt_x)
}

#### loop for cohort life expectancy ####

LT_c <- c()

#You need to complete the loop function before running it
for (y in unique(Mx2$Year)){
  mx2 <- Mx2[Mx2$Year == y,]$mx# specify mx2 within the loop  
  lt_y <- LifeTableMx(mx2, 'm')
  lt_y$Year <- y
  LT_c <- rbind(LT_c,lt_y)
}

#### Visualization ####

library(ggplot2)

# visualize 1) comparison across e(0), e(30), and e(80)
#           2) cohort and period e(0) together


ggplot() + 
  geom_line(data = LT_p[LT_p$Age == 0,], mapping = aes(x = Year, y = ex, color = "Period")) + 
  geom_line(data = LT_c[LT_c$Age == 0,], mapping = aes(x = Year, y = ex, color = "Completed Cohort")) + 
  labs(colour = "Legend", y = "Expected Number of Years") + ggtitle("Life Expectancy at birth, Period Vs. Cohort")

ggplot() + 
  geom_line(data = LT_p[LT_p$Age == 70,], mapping = aes(x = Year, y = 70 + ex, color = "Period")) + 
  geom_line(data = LT_c[LT_c$Age == 70,], mapping = aes(x = Year, y = 70 + ex, color = "Completed Cohort")) + 
  labs(colour = "Legend", y = "Expected Number of Years") + ggtitle("Life Expectancy at age 70, Period Vs. Cohort")

