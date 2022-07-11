






library(extrafontdb)
loadfonts(device = "win")
library(dplyr)
library(ggrepel)
library(ggplot2)
library(wbgdata)
library(wbgcharts)
library(wbgmaps)
library(wbggeo)
library(countrycode)
library(gtable)
library(grid)
source("C:\\Users\\wb551496\\Desktop\\Summer Assignment\\Graphs in R\\Codes\\styles_viz.R")


fig_2.7 <- function() {
  
  df <- read.csv("M:\\ICP Cycles\\6-Outreach and Knowledge\\6.01 ICP Results and Reports\\R scripts for charts\\8. F2.7 Regional level PLI by Major agg\\F2.7_Data.csv")
  df <- df[1:28,]
  #df <- df[1:32,]
  colnames(df)[1]<- "Country.Name"
  
  #Assign levels to graph 
  df$Country.Name <-factor(df$Country.Name,levels = c("AFRICA","ASIA AND THE PACIFIC","WESTERN ASIA","COMMONWEALTH OF INDEPENDENT STATES","CARIBBEAN","LATIN AMERICA","WORLD","Eurostat-OECD"))
  #df$Country.Name <-factor(df$Country.Name,levels = c("AFRICA","ASIA AND THE PACIFIC","WESTERN ASIA","COMMONWEALTH OF INDEPENDENT STATES","CARIBBEAN","LATIN AMERICA","Eurostat-OECD"))
  
  
  df$Classification.Name<- factor(df$Classification.Name,levels=c("01. Gross domestic product","02. Actual individual consumption","18. Collective consumption expenditure by government","19. Gross fixed capital formation"))
  colnames(df)[7] <- "values"
  
  
  
  figure(
    data = df,
    plot = function(df, style = style_atlas_open(),quality="high",aspect_ratio=1) {
      facet_labeller= as_labeller(c("01. Gross domestic product"="GDP",
                                       "02. Actual individual consumption"="Actual Final Consumption",
                                       "18. Collective consumption expenditure by government"="Collective Consumption Expenditure By Government",
                                       "19. Gross fixed capital formation"="Gross Fixed Capital Formation"),multi_line = F)
      
      
      
      p.gdp <- ggplot(df, mapping = aes(x=Country.Name, y=values)) +
        geom_col(aes(fill = Country.Code)) + xlab(" ")+ylab(" ")+
        #scale_fill_manual(name = "legend",values = c("#002633","#00394c","#004d66","#00607f","black","#009acc","#007399","#002633"),labels= df$Country.Name)+
        scale_fill_manual(name = "legend",values = c("#004d66","#004d66","#004d66","#004d66","#004d66","#004d66","#004d66","black"),labels= df$Country.Name)+
        #scale_fill_manual(name = "legend",values = c("#004d66","#004d66","#004d66","#004d66","#004d66","#004d66","#004d66"),labels= df$Country.Name)+
        geom_text(aes(label=round(values)),size = 3,vjust=0.1,hjust=1.1,color="white")+
        scale_y_continuous(limits = c(0,150),breaks = c(50,100),labels = c(" ","100")) +
        geom_hline(yintercept = 100,linetype="dashed",color="black")+
        #scale_y_continuous(limits = c(0,150),breaks = 0,labels = " ")+
        coord_flip() +
        facet_wrap(~Classification.Name,ncol = 2,labeller = facet_labeller )+
        #labeller = facet_labeller
        style$theme() +
        style$theme_barchart() +
        theme(
          #axis.text.y = element_blank(),
          strip.text.x = element_text(hjust = 0.5,size=11,family = "Nuhito Sans"),
          plot.title = element_text(size = 12),
          plot.subtitle = element_text(size=9))
      
     
      
    },
    title = "Story ?",
    subtitle = "Regional average price level indices ( World=100 ) by GDP and Major Aggregates, ICP 2011",
    source = "Source: ICP, http://icp.worldbank.org"
  )
}


fig_2.7()







