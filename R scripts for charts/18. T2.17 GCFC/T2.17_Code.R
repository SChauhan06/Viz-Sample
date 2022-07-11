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
style = style_atlas()

source("C:\\Users\\wb551496\\Desktop\\Summer Assignment\\Graphs in R\\Codes\\styles_viz.R")

fig_t2.17 <- function(years=c(2011)) {
  df <- read.csv("M:\\ICP Cycles\\6-Outreach and Knowledge\\6.01 ICP Results and Reports\\R scripts for charts\\18. T2.17 GCFC\\T2.17_Data.csv")
  df <- df[1:19,c(1:3,7)]
  df <- df[-c(1),]
  colnames(df)[4]<- "values"
  df$Classification.Name <- factor(df$Classification.Name)
  df$values <- as.numeric(paste(df$values))
  
#to reshape the data in R
  #df1 <- reshape(df, direction = "wide",idvar = c("Country.Name","Country.Code"),timevar = "Classification")
  #colnames(df1)[3]<- "Construction"
  #colnames(df1)[4]<- "Machinery"
  
  #this is used to relevel based on another column's order
  df$Country.Name <- factor(df$Country.Name,levels = df$Country.Name[order(df$values)])
  df$Country.Code <- factor(df$Country.Code)
  
  
 
  figure(
    data = df,
    plot = function(df, style = style_atlas(),quality="high") {
      
      ggplot(df,aes(x=Country.Name,y=values,fill="Gross fixed capital formation"))+
        geom_col(width = 0.8)+ xlab(" ")+ylab("  ")+
        scale_y_continuous(breaks = c(5,10,15,20,25))+
        scale_fill_manual(name=" ",values = "#009acc")+
        coord_flip()+
        theme(legend.box = "vertical",
              legend.key = element_blank(),
              legend.title = element_blank(),
              legend.position = "none")+
        style$theme() +
        style$theme_barchart() +
        style$theme_legend("none") 
    },
    title = "China accounts for more than a quarter of global expenditure on Gross Fixed Capital Formation (GFCF)",
    subtitle = "Share of global expenditure on GFCF, G20 countries (2011)",
    note = "European Union Excluded, Argentina not in ICP 2011 database",
    source = "Source: ICP, http://icp.worldbank.org"
  )
}





fig_t2.17()



