
## R tutorial 2 - Student Version
## DEMO2002 - Visualising Population dynamics
## 2-Mar-2023

## three tips to success in R

## 1. It's Okay if you have to Google it
## 2. Use the help() function, a life saver!
## 3. There are always more than one way to do things

## download HFD data from 
## https://www.humanfertility.org/ 
## Select country (USA) -> Age-specific data

# Set working directory and load the file
# Or you can create a project

#### data preparation ####

Mx<-read.table("../Assignment 1/DNKasfrRR.txt", header = TRUE, skip = 2)

# when reading this data, how do we take off the title?
# How do we handle the column names, and empty cell?

## Age is a factor variables that needs to be 
## transformed into numeric 
## Let's start by changing the "problematic values"

Mx$Age <- as.character(Mx$Age)
Mx$Age[Mx$Age=="12-"] <- "12"
Mx$Age[Mx$Age=="55+"] <- "55"
Mx$Age <- as.numeric(Mx$Age)

Mx$ASFR <- as.numeric(Mx$ASFR)

# Select the year of interest
Mx2017 <- Mx[Mx$Year=='2017', ]


#### plotting the value using ggplot2 ####

#install.packages("ggplot2")

library(ggplot2)
# This is a package, it helps you do things faster
# without having to write a whole bunch of uneccessary codes

ggplot()

# we can plot age profile of fertility

ggplot(data = Mx2017)+
  geom_point(aes(x=Age, y = ASFR, color = ASFR)) +
  geom_line(aes(x = Age, y = ASFR)) +
  geom_smooth(aes(x = Age, y = ASFR)) + 
  scale_color_continuous(type = "viridis")
  ggtitle("Age Specific Fertility Rate")

## Now what if we need colors to tell the story across time?

# help("scale_color_continuous")

ggplot(data = Mx)+
  geom_line(aes(x=Age,y=ASFR,group=Year,color=Year))+
  scale_color_continuous(type = "viridis") + 
  ggtitle("ASFR Distribution (1933-2020)")

## color is for points and lines, fill is for areas!

ggplot(data = Mx)+
  geom_raster(aes(x=Year,y=Age,fill=ASFR))+
  scale_fill_gradientn(colors=c("navy","blue","skyblue",
                                      "yellow","gold",
                                      "orange","red")) + 
                                        ggtitle("ASFR Heatmap - Canada (1921 - 2021)") + 
                                        labs()
                                        
#### Recap! ####

## 1. What is a package?
## 2. What do you have to specify in ggplot every time?
## Answer: ggplot(), x and y, and your plotting function
## 3. How to control axis and  colors in ggplot2?