library(tidyverse)
library(wbgcharts)
library(wbgdata)
library(wbstats)
library(ggplot2)
library(countrycode)
library(reshape2)
library(reshape)
year=c(2011,2017)
style = style_atlas()

source("C:\\Users\\wb551496\\Desktop\\R scripts for charts\\styles_viz.R")

fig_vis1.2b <- function(years=c(2011,2017)) {
  indicators <- c("NY.GDP.MKTP.PP.KD")
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicators,
    years = year,
    indicator.wide = TRUE
  )
  df <- df %>% arrange(NY.GDP.MKTP.PP.KD)
  df <- cast(df, iso3c~date) #to reshape the data in R
  df$countryname <- wbgref$countries$labels
  df <- df[complete.cases(df),]
  wrd11 <- sum(df$`2011`)  
  wrd17 <- sum(df$`2017`)
  fe <- list("WLD",wrd11,wrd17,"WORLD")
  df <- rbind(df,fe)
  
  
  df$change <- df$`2017` - df$`2011`
  df$percentn <- ((df$`2017`-df$`2011`)/df$`2011`)*100 # %increase in National GDP 
  
  #to locate that world change inthe dataframe 
  world.change <- df[192,5]
  df$percentg <-  (df$change/world.change)*100 # Share of increase
  
  

  #G20economies - Top growers
  df <- df[df$countryname %in% c("South Africa","Argentina","Japan","Germany","Indonesia","India","China","Brazil","United Kingdom","France","Mexico","Russian Federation","Italy","Turkey","Korea, Rep.","Saudi Arabia","Canada","Australia","United States"),]
  df <- df[, c(1,4,6,7)]
  
  df.1<- reshape(data=df, idvar=c("countryname","iso3c"),
                           varying = c("percentn","percentg"),
                           v.names = "percent",
                           times=c("percentn","percentg"),
                           direction="long")
  
  #this is used to relevel based on another column's order
  df.1$countryname <- factor(df.1$countryname,levels = df.1$countryname[order(df$percentn)])
  df.1$time <- factor(df.1$time,levels = c("percentg","percentn"))

  
  figure(
    data = df.1,
    plot = function(df.1, style = style_atlas_open(),quality="high") {
      
      ggplot(df.1,aes(x=countryname,y=percent))+
        geom_col(mapping = aes(fill=time),width = 0.7,position = position_dodge())+
        xlab(" ")+ylab(" ")+
        scale_fill_manual(name="  ",values = c("#009acc","grey50"),labels=c("Share of Increase in Global GDP (World=100)","Increase in National GDP"))+
        guides(fill=guide_legend(reverse = T))+
        coord_flip()+
        theme(legend.box = "vertical",
              legend.key = element_blank(),
              legend.title = element_blank(),
              legend.position = "bottom")+
        style$theme() +
        style$theme_barchart() +
        style$theme_legend("top") 
    },
    title = "Over 50% growth in the national GDP of China and India accounted for nearly half of the increase in global GDP between 2011 and 2017",
    subtitle = wbg_name(indicator = "Share of global GDP increase, and national GDP increase (%), 2011-2017 G20 countries"),
    note="European Union is excluded",
    source = "Source: WDI: NY.GDP.MKTP.PP.KD"
  )
}





fig_vis1.2b()


