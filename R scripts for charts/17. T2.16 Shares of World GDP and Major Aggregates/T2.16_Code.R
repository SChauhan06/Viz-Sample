

windowsFonts("Nunito Sans"=windowsFont("Nunito Sans"))
windowsFonts("Avenir Heavy"=windowsFont("Avenir Heavy"))
windowsFonts("Avenir Book"=windowsFont("Avenir Book"))



library(extrafontdb)
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


fig_splitbar <- function(year=2011) {
  
  df <- read.csv("M:\\ICP Cycles\\6-Outreach and Knowledge\\6.01 ICP Results and Reports\\R scripts for charts\\17. T2.16 Shares of World GDP and Major Aggregates\\T2.16_data.csv")
  df <- df[1:7,5:12]
  
  #Might have to assign levels
  df$Country.Name <-factor(df$Country.Name,levels = c("CARIBBEAN","WESTERN ASIA","LATIN AMERICA","COMMONWEALTH OF INDEPENDENT STATES","AFRICA","ASIA AND THE PACIFIC","Eurostat-OECD"))
  

  
  df.1<- reshape(df, idvar=c("Country.Code","Country.Name"),
                 varying = c("GDP","Food","Housing","Health","Education","Construction"),
                 v.name=c("Classification"),
                 times=c("GDP","Food","Housing","Health","Education","Construction"),
                 direction="long")
  df.1$time <- factor(df.1$time,levels=c("GDP","Food","Housing","Health","Education","Construction"))
  
  figure(
    data = df.1,
    plot = function(df.1, style = style_atlas_open(),quality="high",aspect_ratio=1) {
      
      
      p.pop <- ggplot(
        df.1, mapping = aes(x=Country.Name, y=Classification)
      ) +
        geom_col(fill="#009acc") +xlab(" ")+ylab(" ")+
        #scale_fill_manual(name = "legend", values = c("#002633","#00394c","#004d66","#00607f","#002633","#009acc","#007399"),labels= df.1$Country.Name)+
        geom_text(aes(label=round(Classification)),size = 3,vjust=0.5,hjust=1.1,color="white")+
        scale_y_continuous(breaks = 0,labels = " ") +
        coord_flip() +
        facet_wrap(~ time,ncol = 3)+
        style$theme() +
        style$theme_barchart() +
        theme(
          #axis.text.y = element_blank(),
          strip.text.x = element_text(hjust =0.5,size=11,family = "Nuhito Sans"),
          plot.title = element_text(size = 12),
          plot.subtitle = element_text(size=9))
      
      
    },
    title = "EU-OECD accounts for greatest share of global GDP and most aggregates",
    subtitle = "Share of global GDP, PPP and other aggregates by ICP region, 2011",
    source = "Source: International Comparison Program, http://icp.worldbank.org"
  )
}




fig_splitbar()


