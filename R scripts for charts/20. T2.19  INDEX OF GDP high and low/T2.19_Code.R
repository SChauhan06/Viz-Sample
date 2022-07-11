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
library(ggforce)
style = style_atlas()

source("C:\\Users\\wb551496\\Desktop\\Summer Assignment\\Graphs in R\\Codes\\styles_viz.R")


fig_t2.19 <- function(years=c(2011)) {
  df <- read.csv("M:\\ICP Cycles\\6-Outreach and Knowledge\\6.01 ICP Results and Reports\\R scripts for charts\\20. T2.19  INDEX OF GDP high and low\\Data_T2.19.csv")
  
  df <- df[c(1:20),]
  df$ten <- ifelse(df$X2011..YR2011.<10,"bottom","top")
  #df$X2011..YR2011.2 <- factor(df$)
  df$Country.Name<- factor(df$Country.Name,levels = df$Country.Name[order(df$X2011..YR2011.)])
  #df$Country.Name <- factor(df$Country.Name,levels = df$Country.Name[order(df$values)])
  df$ten <- factor(df$ten,levels = c("top","bottom"))
  
  
  figure(
    data = df,
    plot = function(df, style = style_atlas(),quality="high") {
      
      
      
      ggplot(df,aes(x=Country.Name,y=X2011..YR2011.,fill=ten))+
        geom_col(alpha=0.8)+ xlab(" ")+
        geom_text(aes(label = round(X2011..YR2011.,digits = 0)),nudge_y =35,size=3.5,color="black")+
        #geom_text(aes(label = Economy),nudge_y=-17,size=3.5,color="black",angle=90)+
        ylab("Index of GDP per capita (World=100)") +
        scale_x_discrete()+
        scale_y_continuous(breaks = c(0,100,300,600,900))+
        geom_hline(yintercept = 100,color="black",linetype="dashed")+
        scale_fill_manual(expand=c(0,0),name="  ",values = c("#009acc","grey68"),labels=c("10 Largest","10 Smallest"))+
        coord_flip()+style$theme()+ 
        theme(panel.grid.major.y   =element_blank(), 
               legend.box = "horizontal",
              legend.key = element_blank(),
              legend.title = element_blank(),
              legend.position = c(0.65,0.2))
    },
    title = "Countries with the Highest and Lowest GDP per capita",
    subtitle ="International Comparison Program 2011 Database",
    source = "Source: ICP, http://icp.worldbank.org"
  )
}





fig_t2.19()

