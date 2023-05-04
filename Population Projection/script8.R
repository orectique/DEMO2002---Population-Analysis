
## R tutorial 9 - Population projection
## DEMO2002 
## 2023

library(ggplot2)

Names <- "USA"

YEAR <- 2019

Proj <- 100

#### Population and Births ####

Pop<-read.table("Population.txt", skip =2, header = T) # population

#### Leslie matrix ####

Mf<-as.matrix(read.csv("Leslie_Matrix_Female_Shock.csv")[,-1])

Mm<-as.matrix(read.csv("Leslie_Matrix_Male.csv")[,-1])

SRB <- 1.048 # I calculated this so you don't have to
L1m <- 0.4966
L1 <- 0.4972
km <- SRB*L1m/L1
## factor for calculating and surviving the male babies 
# SRB is for the number of male births, compared to the female
# L1m/L1 is to survive the male births to age 0, 
# comparing male age 0-1 survival ratio to female age 0-1 survival ratio

### Ipop and Ipopm the base population of females and males 
### respectively (111 entries each)

Ipop<-as.numeric(Pop$Female[Pop$Year == YEAR]) #Set initial population here
Ipopm<-as.numeric(Pop$Male[Pop$Year == YEAR])


#### Projection ####

Proj<-100			# let's look at a projection for 100 years

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
  
  pop[year,]<-t(Mf%*%pop[(year-1),])
  popm[year,]<-c(pop[year,1]*km,t(Mm%*%popm[(year-1),])[-1])
  To<-pop[year,]+popm[year,]
  
  Proj_Pop_temp <- data.frame(Year=(year+YEAR-1),Age=0:110,
                              Female=pop[year,],Male=popm[year,],
                              Total=To)
  
  Proj_Pop <- rbind(Proj_Pop, Proj_Pop_temp)
}

#### Plot the results as a pop pyramid 
#### and present it with a gif

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

Pop1 = Pop_total[Pop_total$Year == 2025, ]

ggplot(data = Pop1, aes(x = Age,  y = Percentage, fill = Sex)) + 
  #Fill in the variables
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = function(x){paste0(x,"%")}, 
                     limits = max(abs(Pop1$Percentage)) * c(-1.1,1.1)) +
  scale_x_continuous(n.breaks = 10)+
  labs(x = "Age", y = "Percentage of Population",
       title = paste("Population Pyramid, Shock to Female Population, USA"),
       subtitle = paste("Year:",2025),
       caption = "Data source: HMD")+
  # We are going to do magic here which flips x and y axis
  coord_flip() + 
  scale_fill_brewer(palette= "Set1")+
  theme_minimal()

ggsave(paste0("population pyramid_",Names,"_Fshock",".png"),
       width=15, height=12,units="cm")


library(gganimate)
library(gifski)

p1 <- ggplot(Pop_total, aes(x = Age,  y = Percentage, fill = Sex)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = function(x){paste0(x,"%")}, 
                     limits = max(abs(Pop_total$Percentage)) * c(-1.1,1.1)) +
  labs(x = "Age", y = "Percentage of Population",
       title = paste("Population pyramid of",Names),
       subtitle = "{closest_state}",
       caption = "Data source: HMD")+
  coord_flip()+
  scale_fill_brewer(palette= "Set1")+
  theme_minimal()

p2 <- p1 + transition_states(Year, transition_length = 2) + 
  enter_fade() +
  exit_fade() + 
  ease_aes("cubic-in-out")

# Generate gif (this will take a few minutes to run)
animate(p2,width = 10, height = 10, units = "cm",res=150, 
        renderer = gifski_renderer(loop = FALSE), 
        nframes= length(unique(Pop_total$Year))*2)

# save the gif file 
anim_save(paste0("population pyramid_Mshocked_",Names,"_",
                 min(Pop_total$Year),"_",max(Pop_total$Year),
                 ".gif"))

