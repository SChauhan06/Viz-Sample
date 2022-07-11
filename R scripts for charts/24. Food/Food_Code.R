windowsFonts("Nunito Sans"=windowsFont("Nunito Sans"))
windowsFonts("Avenir Heavy"=windowsFont("Avenir Heavy"))
windowsFonts("Avenir Book"=windowsFont("Avenir Book"))






library(tidyverse)
library(wbgcharts)
library(wbgdata)
library(wbgmaps)
library(wbstats)
library(ggmosaic)
library(treemapify)
library(ggtreemap)
library(ggplot2)
library(wbggeo)
library(countrycode)
library(reshape2)
library(reshape)
library(ggrepel)
style = style_atlas_open()

source("C:\\Users\\wb551496\\Desktop\\Summer Assignment\\Graphs in R\\Codes\\styles_viz.R")
fig_Food <- function() {
  df <- read.csv("M:\\ICP Cycles\\6-Outreach and Knowledge\\6.01 ICP Results and Reports\\R scripts for charts\\24. Food\\Food_Data.csv")
  df <- df[c(1:177),-2] 
  
  
  #data releveling
  df$Income.Classification<-factor(df$Income.Classification,levels = c("Low income","Lower middle income","Upper middle income","High income"))
  
  levels(df$Income.Classification)
  

  df2 <- df[df$Country.Name %in%c("India","China","Japan","Zimbabwe","Qatar","Pakisthan","Angola","Central African Republic","Haiti"),]
  df$Country.Name <- factor(df$Country.Name)
  
  
  #We use a log scale for per capita inorder to see each country clearly 
  
  figure(
    data = df,
    plot = function(df, style = style_atlas(),aspect_ratio=1.5,quality="high") {
      
      
      a <-arrow(angle = 30, length = unit(0.1, "cm"), ends = "last", type = "closed")
      p<- ggplot(df)+
        geom_jitter(aes(x=PLI,y=EXPENDITURE.SHARES,fill= Income.Classification,size=POP),color="black",shape=21,alpha=0.70)+ 
        scale_x_continuous(name = "Food Price Level Index ( World=100 )")+
        scale_y_continuous(name= " Food Expenditure Share ( GDP = 100)")+
        geom_vline(xintercept = 100,linetype="dashed",color="black",size=0.5) +
        geom_text_repel(data = df2,aes(x=PLI,y=EXPENDITURE.SHARES,label=Country.Name),nudge_y = 0,nudge_x=0,fontface="bold",box.padding =  0.79,point.padding = 0.36,arrow = a)+
        scale_fill_manual(name="   ",values = c("gray30","#808080FF","#EC78A9FF","#cc0641"),labels=c("Low income  ","Lower middle income  ","Upper middle income  ","High income  ")) +
        guides(size=guide_legend(title = "Size of Population",order=2),fill=guide_legend(order=1))+
        style$theme() +
        style$theme_legend("right") 
      
      p +  scale_size_area(max_size = 15)
    } 
    ,
    aspect_ratio = 1.5,
    title = " In low-income countries,food prices are low, but expenditure on food accounts for a large share of consumption",
    subtitle = "Food  Expenditure Share (GDP=100) and Price Level Index (World =100) by Income group",
    source = "Source: ICP, http://icp.worldbank.org"
  )
}







fig_Food()
