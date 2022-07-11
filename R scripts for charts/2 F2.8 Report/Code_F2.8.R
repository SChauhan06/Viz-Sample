library(tidyverse)
library(wbgcharts)
library(wbgdata)
library(wbstats)
library(treemapify)
library(ggplot2)
library(countrycode)
library(reshape2)
library(reshape)
library(ggrepel)
library(readxl)
style = style_atlas_open()

source("C:\\Users\\wb551496\\Desktop\\ICP 2020\\R scripts for charts\\styles_viz.R")

fig_F2.6 <- function() {
  
  df <- read_excel("C:\\Users\\wb551496\\Desktop\\ICP 2020\\R scripts for charts\\2 F2.8 Report\\F2.8_Data.xlsx",sheet=2)
  df <- df %>%
    gather(Region,value,-Expenditure.Category,-Type)
  
  df <- df %>%
    spread(Type,value)
  
  df$Expenditure.Category <- factor(df$Expenditure.Category,levels= c("Construction","Machinery and Equipment","Consumption Expenditure by Government","Individual Consumption Expenditure by Households","Miscellaneous Goods and Services", "Restaurants and Hotels", "Education","Recreation and Culture",
                                                                      "Communication","Transport","Health","Furnishings, Household Equipment and Routine Household Maintenance","Housing, Water, Electricity, Gas and Other Fuels",
                                                                      "Clothing and Footwear","Alcoholic Beverages, Tobacco and Narcotics","Food and Non-Alcoholic Beverages","Actual Individual Consumption","Gross Domestic Product"))
  
  label = wbgref$regions$labels
  
  df$Region <- factor(df$Region,levels=c("EAS","ECS","LCN","MEA","NAC","SAS","SSF"),
                      labels = label)
  df$Index1 <- (df$Index)-100
 
  df$New <- ifelse(df$Index>100,1,0)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas_open(),quality="high") {
      
      
      
      ggplot(df)+
        geom_col(data=subset(df,New==1),aes(x=Expenditure.Category,y=Index1),fill="#009acc",alpha=0.8,width = 0.5)+
        geom_text(data=subset(df,New==1),aes(x=Expenditure.Category,y=Index1,label=round((Index),digits = 0)),size=2.5,hjust=-0.2)+
        geom_col(data=subset(df,New==0),aes(x=Expenditure.Category,y=Index1),fill="#009acc",alpha=0.8,width = 0.5)+
        geom_text(data=subset(df,New==0),aes(x=Expenditure.Category,y=Index1,label=round((Index),digits = 0)),size=2.5,hjust=1.2)+
        #scale_x_discrete()+
        scale_y_continuous(limits = c(-100,175),breaks = c(-100,-50,0,50,100,150),labels = c(0,50,100,150,200,250))+
        facet_wrap(~ Region,nrow = 4,ncol = 2)+
        coord_flip()+style$theme()+ 
        xlab(" ") + ylab(" ")+
        theme(panel.grid.major.y = element_blank(),
              legend.box = "horizontal",
              strip.text.x = element_text(hjust = 0.5,size=9),
              legend.key = element_blank(),
              legend.title = element_blank(),
              legend.position = "bottom") 
    }
  )
}

fig_F2.6()

#,
#title = "Regional Average Price Level Indexes (World = 100) for GDP and 17 Aggregates",
#subtitle ="International Comparison Program 2017 Database",
#source = "Source: ICP, http://icp.worldbank.org"

