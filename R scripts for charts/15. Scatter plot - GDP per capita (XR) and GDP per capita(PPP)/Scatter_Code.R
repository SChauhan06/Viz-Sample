
windowsFonts("Nunito Sans"=windowsFont("Nunito Sans"))
windowsFonts("Avenir Heavy"=windowsFont("Avenir Heavy"))
windowsFonts("Avenir Book"=windowsFont("Avenir Book"))

library(ggplot2)
library(tidyverse)
library(countrycode)
library(wbstats)
library(wbgdata)
library(wbgcharts)
library(wbgmaps)
library(wbggeo)
library(scales)
library(ggpmisc)
source("C:\\Users\\wb551496\\Desktop\\Summer Assignment\\Graphs in R\\Codes\\styles_viz.R")

years=2017

#keep changing the year to producing the different graphs

fig_scatterplot <- function(years = 2017){
  indicators <- c("NY.GDP.PCAP.PP.CD","NY.GDP.PCAP.CD")
  
df <- wbgdata(
    wbgref$countries$iso3c, 
    indicators, years = years,
    indicator.wide = TRUE,
    removeNA = TRUE,
  )
  
df <- df %>%
    filter(complete.cases(.)) %>%
    group_by(iso3c) %>%
    filter(date == max(date)) %>%
    ungroup()


  df <- df %>% left_join(wbgref$countries$regions)
  
  figure(
    data = df, 
    plot = function(df, style = style_atlas_open(),quality="high") {
    ggplot(df,aes(y=NY.GDP.PCAP.PP.CD,x=NY.GDP.PCAP.CD)) +
    geom_point(aes(color = region_iso3c),size = 2,alpha=0.7)+
    geom_smooth(method = "lm",se=F,color = style$colors$reference,linetype=style$linetypes$reference)+
    stat_poly_eq(formula=y~x,aes(label=paste(..eq.label..,sep="~~~")),label.x = "center",label.y = "bottom",parse=T)+
    scale_color_manual(name="  ",values = style$colors$regions, labels = wbgref$regions$labels)+
        scale_y_log10(breaks=c(1000,3000,10000,30000,100000),labels=c(1000,3000,10000,30000,"100000"))+
        scale_x_log10(breaks=c(300,1000,3000,10000,30000,100000),labels=c(300,1000,3000,10000,30000,"100000"))+
        
    #scale_y_continuous(trans = "log10",breaks = trans_breaks('log10',function(x) 10^x))+
    #scale_x_continuous(trans = "log10",breaks = trans_breaks('log10',function(x) 10^x))+
    coord_equal()+
    style$theme()+ xlab("GDP per capita, Xr") + ylab("GDP per capita, PPP") +
  theme(panel.grid.major.x = NULL,legend.direction = "horizontal",legend.box = "horizontal",axis.title=NULL,axis.title.y = element_text(angle = 90))+
        style$theme_scatter()+style$theme_legend("bottom")
    
    },
    aspect_ratio = 0.9,
    title = "< Insert Title  >",
    subtitle =wbg_name(indicator = "GDP per capita using Purchasing Power Parities (PPP) and market exchange rates (Xr)", year = years),
    note= "The coefficient will be elasticity of nominal income vs real.",
    source = paste("Source: WDI: NY.GDP.PCAP.CD; NY.GDP.PCAP.PP.CD")
  )
  
}
fig_scatterplot()


