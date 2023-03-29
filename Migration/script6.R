
## R tutorial 6 - Migration
## DEMO2002

# Install circlize package before the tutorial

## Set working directory

# Prepare the data ####

## Load data
df0<-read.csv("interstate.csv") 
# skip the first 9 rows and columns 1 and 5 in the csv file 

str(df0)
unique(df0$STATE..5YR.)
unique(df0$STATE..UR.)

# keep only the eight main states and territories
state=c("New South Wales","Victoria","Queensland","South Australia",
        "Western Australia","Tasmania","Northern Territory","Australian Capital Territory")

df0<-df0[df0$STATE..5YR. %in% state & df0$STATE..UR. %in% state,] 
## What if we want to subset across multiple states?

# Shorten state and territory names
STATE=c("NSW","VIC","QLD","SA","WA","TAS","NT","ACT")

##  Try changing the name of the states with a loop and 
##  ifelse() function

for (i in 1:8){
  df0$STATE..UR.<- ifelse(df0$STATE..UR.== state[i], STATE[i],df0$STATE..UR.)
  df0$STATE..5YR.<- ifelse(df0$STATE..5YR.== state[i], STATE[i],df0$STATE..5YR.)
}

# Let's count flows in thousand
df0$Count <- df0$Count/1000

# 3. Plot internal migration using circle plot ####

## The circle plot is adapted from: 
## Abel, G.J., and Sander, N. (2014). Quantifying global international miggration flows. Science, 343(6178): 1520-22.
## And Abel's Github https://github.com/gjabel/migest/blob/master/demo/cfplot_reg2.R

#install.packages("circlize")

library(circlize)

## Default chord Diagram
chordDiagram(x = df0) # the majority of the flow are "stayers"

## exclude the "stayers" and keep only those who changed state/territory of usual residence between 2016 and 2021
##  We have learned to subset, how to exclude?
df1<-df0[df0$STATE..5YR. != df0$STATE..UR.,]

chordDiagram(x = df1) 

## Set parameters
## Every time you want to change default graphical settings, you need to use circos.clear() to reset it the plot.
## If you meet some errors when re-drawing the circular plot, try running circos.clear() and it will solve most of the problems.
circos.clear() 

# Step-wise plotting 

## Step 1: Add gaps between states and territories 
circos.par(gap.after = 5)

chordDiagram(x = df1, transparency = 0.01, directional = 1)

## Step 2: Change colors
#my.color = c("#F0E442", "#0072B2", "#D55E00","#CC79A7", "#999999", "#E69F00", "#56B4E9", "#009E73")

chordDiagram(x = df1, #grid.col = my.color
             )

## Step 3: Adjust transparency
chordDiagram(x =df1, #grid.col = my.color, 
             transparency = 0.05)

## Step 4: Add direction of flow
chordDiagram(x =df1, #grid.col = my.color, 
             transparency = 0.0001,
             directional = 1, 
             direction.type = c("arrows", "diffHeight"), 
             diffHeight  = -0.04)

## Second line indicates that chords should be directional. 
### The direction of the chords will be illustrated by both arrows and a difference in height. 
### The height difference is negative to make the chord shorter at the end (with the arrow head).

## Step 5: Change the type of arrows
chordDiagram(x =df1, #grid.col = my.color, 
             transparency = 0.001,
             directional = 1, 
             direction.type = c("arrows", "diffHeight"), 
             diffHeight  = -0.04,
             link.arr.type = "big.arrow")

## Third line indicates the plot should use big arrows, 


## Step 6: Sort the flow by size 
chordDiagram(x =df1, #grid.col = my.color, 
             transparency = 0.001,
             directional = 1, 
             direction.type = c("arrows", "diffHeight"), 
             diffHeight  = 0,
             link.arr.type = "big.arrow", 
             link.sort = TRUE, link.largest.ontop = TRUE)

## Fourth line sort the chords left to right in each sector and  plots the smallest chords first.


## Step 7: Add title and data source to plot
title(main = "Five-year interstate migration flows (in thousand), Australia, 2011-16 
      (source: 2021 Australian Census)",
      cex.main = 0.9, font.main= 2, col.main= "black",line = -0.65)


## Export the plot

##   Save the figure?

jpeg("./chordDiagramMigration.jpg")

chordDiagram(x =df1, #grid.col = my.color, 
             transparency = 0.05,
             directional = 1, 
             direction.type = c("arrows", "diffHeight"), 
             diffHeight  = -0.04,
             link.arr.type = "big.arrow", 
             link.sort = TRUE, link.largest.ontop = TRUE)

title(main = "Five-year interstate migration flows (in thousand), Australia, 2016-21
      (source: 2021 Australian Census)",
      cex.main = 0.9, 
      font.main= 2, 
      col.main= "black",line = -0.65)

dev.off()

