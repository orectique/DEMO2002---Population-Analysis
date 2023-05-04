## R tutorial 6 - Population Growth
## DEMO2002 
## 30-Mar-2022


## We need to install and use the following packages

library(ggplot2) # for data visualization


## 1. DOWNLOAD DATA ####

## Download single-age group population size data for a selected population from HMD https://www.mortality.org/ 
## Select country (Australia) -> Complete Data Series -> Period data -> Population size -> 1-year
## Save the "Population.txt" file in your working directory

## Specify which country you choose to plot
Names<-c("Finland")

## 2. PREPARE DATA ####

# Or, if you read data from the txt file

Pop <-read.table('FinPopulation.txt', skip = 2, fill = T, header = T) 
  
  str(Pop)
  unique(Pop$Age)
  
  Pop$Age[Pop$Age == '110+' ] <-  '110'#change age to numeric value
    Pop$Age <- as.numeric(Pop$Age)
  
  
  ## 3. PLOT SINGLE YEAR POPULATION PYRAMID ####
  
  ## With one year
  
  range(Pop$Year)
  
  Year1<-max(Pop$Year)
  
  Pop1 <- Pop[Pop$Year==Year1,]
  
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
  
  p <- ggplot(data = Pop1, aes(x = Age,  y = Percentage, fill = Sex)) + 
    #Fill in the variables
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = function(x){paste0(x,"%")}, 
                       limits = max(Pop1$Percentage) * c(-1.1,1.1)) +
    scale_x_continuous(n.breaks = 10)+
    labs(x = "Age", y = "Percentage of Population",
         title = paste("Population pyramid of",Names),
         subtitle = paste("Year:",Year1),
         caption = "Data source: HMD")+
    # We are going to do magic here which flips x and y axis
    scale_fill_brewer(palette= "Set1")+
    theme_minimal()
  p
  
  ggsave(paste0("population pyramid_",Names,"_",Year1,".png"),
         width=15, height=12,units="cm")
  
  ## 4. PLOT POPULATION PYRAMID ACROSS YEARS ####
  
  Pop_total <- c() #data.frame(matrix(ncol = 4, nrow = 0))
  #colnames(Pop_total) <- colnames(Pop1)
  
  for (year in unique(Pop$Year)){
    # write the loop yourself. 
    
    Pop1 <- Pop[Pop$Year==year,]
    
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
  
  ## 4.1 Present it across facets ###
  
  p <- 
    ggplot(Pop_total[Pop_total$Year %in% c(1921,1971,2021),],
           aes(x = Age,  y = Percentage, fill = Sex))+
    geom_bar(stat = "identity")+
    facet_wrap(~Year,ncol=3)+
    scale_y_continuous(labels = function(x){paste0(x,"%")}, 
                       limits = max(Pop_total$Percentage) * c(-1.1,1.1)) +
    scale_x_continuous(n.breaks = 10)+
    labs(x = "Age", y = "Percentage of Population",
         title = paste("Population pyramid of",Names),
         caption = "Data source: HMD")+
    coord_flip()+ # flip x and y axis
    scale_fill_brewer(palette= "Set1")+
    theme_minimal()
  
  p 
  
  ggsave(paste0("population pyramid_",Names,"_","comp",".png"),
         width=23, height=8,units="cm")
  
  ## Present it with a gif ###
  
  ## Alternatively, use gganimate package to generate and export a gif file
  ## Note: the gganimate package takes time to run.
  library(gganimate)
  library(gifski)
  
  p1 <- ggplot(Pop_total, aes(x = Age,  y = Percentage, fill = Sex)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = function(x){paste0(x,"%")}, 
                       limits = max(Pop_total$Percentage) * c(-1.1,1.1)) +
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
  anim_save(paste0("population pyramid_",Names,"_",
                   min(Pop_total$Year),"_",max(Pop_total$Year),
                   ".gif"))
  
  