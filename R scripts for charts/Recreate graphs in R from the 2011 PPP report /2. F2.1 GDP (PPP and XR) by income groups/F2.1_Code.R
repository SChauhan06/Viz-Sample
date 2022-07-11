

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
library(reshape2)

source("C:\\Users\\wb551496\\Desktop\\R scripts for charts\\styles_viz.R")
year=c(2011:2017)
style = style_atlas_open()
quality = "high"
aspect_ratio = 1



fig_vis_1.2a<- function(year=c(2011:2017)) {
  
  indicators <- c("NY.GDP.MKTP.PP.KD","NY.GDP.MKTP.KD")
  df <- wbgdata(
    wbgref$incomes$iso3c,
    indicators,
    years = year,
    indicator.wide = TRUE
   )
  
  df <- melt(df,id.vars = c("iso3c","date"))
  df$iso3c <- factor(df$iso3c,levels = c("HIC","UMC","LMC","LIC"))
  df$variable <- factor(df$variable, levels = c("NY.GDP.MKTP.PP.KD","NY.GDP.MKTP.KD"))
  levels(df$variable)
  levels(df$iso3c)
  
  
  
  figure(
    data = df,
    plot = function(df, style = style_atlas_open()) {
      
      
      a <-  ggplot(df,aes(date,value,group=iso3c))+
        geom_area(aes(fill= iso3c),alpha=0.85) + 
        scale_y_continuous(name=" ",breaks = c(20000000000000,40000000000000,60000000000000,80000000000000,100000000000000,120000000000000),labels = billions()) +
        scale_x_continuous(name=" ",breaks = seq(min(df$date), max(df$date), 1)) +
        facet_wrap(~variable)+
        scale_fill_manual(name= " ", values = style$colors$incomes ,labels = wbgref$incomes$labels)+
        style$theme() + 
        style$theme_legend("bottom")+
        theme(strip.text.x = element_blank())
      
    },
    title = "Using PPPs, middle income countries contribute to around a half of global GDP, but just a third under exchange rate measures.",
    subtitle = wbg_name(indicator = "GDP,PPP (constant 2011 international $) and GDP (constant 2010 US$)", denom = "billions"),
    source = "Source: WDI: NY.GDP.MKTP.PP.KD; NY.GDP.MKTP.KD "
  )
}
fig_vis_1.2a()



#if you want labels
#facet_labeller= as_labeller(c("NY.GDP.MKTP.PP.CD"="GDP,PPP (constant 2011 international US$, biilions) ",
#                             "NY.GDP.MKTP.KD"="GDP (constant 2010 US$, billions)",multi_line = F))
#labeller = facet_labeller
