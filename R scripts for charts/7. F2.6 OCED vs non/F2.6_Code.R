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
library(nls2)
library(ggpmisc)
style = style_atlas()

source("C:\\Users\\wb551496\\Desktop\\Summer Assignment\\Graphs in R\\Codes\\styles_viz.R")

fig_2.6 <- function(years=c(2011)) {
  df <- read.csv("M:\\ICP Cycles\\6-Outreach and Knowledge\\6.01 ICP Results and Reports\\R scripts for charts\\7. F2.6 OCED vs non\\F2.6_Data.csv")
  df <- df[-c(705:709),c(1:3,5,7:8)]
  df$Region1 <- ifelse(df$Region=="Eurostat-OECD",1,0)
  df$Region1 <- factor(df$Region1)
  df$Classification.Name <- factor(df$Classification.Name)
  df1 <- reshape(df, idvar = c("Country.Name","Country.Code","Classification.Name","Region","Region1"),timevar = "Series.Name",direction = "wide")
  colnames(df1)[6:7]<- c("PERCAP","PLI")
  
  
  
  figure(
    data = df1,
    plot = function(df1, style = style_atlas(),quality="high") {
      facet_labeller= as_labeller(c("01. Gross domestic product"= "GDP",
                                    "02. Actual individual consumption"="AIC"))
      
      
      
     p <- ggplot(df1,aes(x=PERCAP,y=PLI,color=Region1,fill=Region1))+
        geom_point(alpha=0.8,shape=22,size=2.5)+
        xlab("per capita, PPP ")+
       scale_x_log10(breaks=c(500,5000,50000))+
       
        stat_smooth(method="glm",method.args = list(family="quasipoisson"),se = F,color="grey50")+
       stat_poly_eq(formula=log(y)~log(x),aes(label=paste(..rr.label..,sep = "~~~")),parse=T,color="grey50")+
        facet_wrap(~Classification.Name,labeller = facet_labeller)+
       scale_color_manual(name=" ",values=c("#009acc","grey68"),labels=c("Non-Eurostat-OECD economies","Eurostat-OECD economies"))+
       scale_fill_manual(name=" ",values=c("#009acc","grey68"),labels=c("Non-Eurostat-OECD economies","Eurostat-OECD economies"))+
        ylab("PLI (World=100)")+
        theme(legend.box = "vertical",
              legend.key = element_blank(),
              legend.title = element_blank(),
              legend.position = "bottom")+
        style$theme() +
        style$theme_legend("bottom")+
          theme(
            #axis.text.y = element_blank(),
            strip.text.x = element_text(hjust = 0.5,size=11,family = "Nuhito Sans"),
            plot.title = element_text(size = 12),
            plot.subtitle = element_text(size=9))
  
      
    },
    title = "Price level Index versus Expenditure per capita with Trendlines",
    subtitle ="International Comparison Program 2011 Database",
    source = "Source: ICP, http://icp.worldbank.org"
  )
}

#uses glm to plot the line and not the lm method. 




fig_2.6()

