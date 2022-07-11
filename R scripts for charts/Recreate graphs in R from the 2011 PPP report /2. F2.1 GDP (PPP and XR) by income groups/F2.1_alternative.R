

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

source("C:\\Users\\wb551496\\Desktop\\Summer Assignment\\Graphs in R\\Codes\\styles_viz.R")
year=2017
style = style_atlas_open()
quality = "high"
aspect_ratio = 1



fig_stacked_a<- function(year=2017) {
  
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
      facet_labeller= as_labeller(c("NY.GDP.MKTP.PP.KD"="GDP, PPP",
                                    "NY.GDP.MKTP.KD"="GDP, exchange rate",multi_line = F))
      
      a <-  ggplot(df,aes(date,value,fill=iso3c))+
        geom_col(position = "fill",alpha=0.85)+ ylab(" ") +
        scale_x_continuous(name=" ",breaks = seq(min(df$date), max(df$date), 1),labels = " ") +
        facet_wrap(~variable,labeller = facet_labeller,strip.position= "bottom")+
        scale_fill_manual(name= " ", values = style$colors$incomes ,labels = wbgref$incomes$labels)+
        scale_y_continuous(breaks = c(0.25,0.50,0.75,1),labels = c("25%","50%","75%","100%")) +
        style$theme() + 
        style$theme_legend("right")+
        theme(strip.text.x = element_text(hjust=0.5,size=11)
              )
      
    },
    title = "Using PPPs, middle income countries contribute to around a half of global GDP, but just a third under exchange rate measures.",
    subtitle = wbg_name(indicator = "Share of GDP under PPP and exchange rate measures, 2017"),
    source = "Source: WDI: NY.GDP.MKTP.PP.KD; NY.GDP.MKTP.KD "
  )
}

fig_stacked_a()



#if you want labels
#facet_labeller= as_labeller(c("NY.GDP.MKTP.PP.CD"="GDP,PPP (constant 2011 international US$, biilions) ",
#                             "NY.GDP.MKTP.KD"="GDP (constant 2010 US$, billions)",multi_line = F))
#labeller = facet_labeller

############################################################################################################################################



fig_stacked_b<- function(year=2017) {
  
  indicators <- c("NY.GDP.MKTP.PP.KD","NY.GDP.MKTP.KD")
  df <- wbgdata(
    wbgref$regions$iso3c,
    indicators,
    years = year,
    indicator.wide = TRUE
  )
  
  df <- melt(df,id.vars = c("iso3c","date"))
  df$iso3c <- factor(df$iso3c,levels = c("EAS","ECS","NAC","SAS","MEA","LCN","SSF"))
  df$variable <- factor(df$variable, levels = c("NY.GDP.MKTP.PP.KD","NY.GDP.MKTP.KD"))
  levels(df$variable)
  levels(df$iso3c)
  
  
  
  figure(
    data = df,
    plot = function(df, style = style_atlas_open()) {
      facet_labeller= as_labeller(c("NY.GDP.MKTP.PP.KD"="GDP, PPP",
                                    "NY.GDP.MKTP.KD"="GDP, exchange rate",multi_line = F))
      
      a <-  ggplot(df,aes(date,value,fill=iso3c))+
        geom_col(position = "fill",alpha=0.85)+ ylab(" ") +
        scale_x_continuous(name=" ",breaks = seq(min(df$date), max(df$date), 1),labels = " ") +
        facet_wrap(~variable,labeller = facet_labeller,strip.position= "bottom")+
        scale_fill_manual(name= " ", values = style$colors$regions ,labels = wbgref$regions$labels)+
        scale_y_continuous(breaks = c(0.25,0.50,0.75,1),labels = c("25%","50%","75%","100%")) +
        style$theme() + 
        style$theme_legend("right")+
        theme(strip.text.x = element_text(hjust=0.5,size=11)
        )
      
    },
    title = "The contribution of Europe and Central Asia & North America to Global GDP is significantly less when measured using PPPs.",
    subtitle = wbg_name(indicator = "Share of GDP under PPP and exchange rate measures, 2017"),
    source = "Source: WDI: NY.GDP.MKTP.PP.KD; NY.GDP.MKTP.KD "
  )
}
fig_stacked_b()



#if you want labels
#facet_labeller= as_labeller(c("NY.GDP.MKTP.PP.CD"="GDP,PPP (constant 2011 international US$, biilions) ",
#                             "NY.GDP.MKTP.KD"="GDP (constant 2010 US$, billions)",multi_line = F))
#labeller = facet_labeller
