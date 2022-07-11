
windowsFonts("Nunito Sans"=windowsFont("Nunito Sans"))
windowsFonts("Avenir Heavy"=windowsFont("Avenir Heavy"))
windowsFonts("Avenir Book"=windowsFont("Avenir Book"))


library(extrafont)
library(ggplot2)
library(tidyverse)
library(countrycode)
library(wbstats)
library(wbgdata)
library(wbgcharts)

library(readxl)


source("C:\\Users\\wb551496\\Desktop\\ICP 2020\\R scripts for charts\\styles_viz.R")
aspect_ratio = 1
style = style_atlas_open()


fig_1.1_column_width <- function() {
  df <- read_excel("C:\\Users\\wb551496\\Desktop\\ICP 2020\\R scripts for charts\\1. Martini plot +AICpc\\Book2.xlsx")
  df <- df[,-5]
    
    df <- df %>% arrange(`2017.GDPpercap`)
    
    df$Geo.Region<- factor(df$Geo.Region)
    
    median <- median(df$`2017.GDPpercap`)
  
    
    figure(
      data = df,
      plot = function(df, style = style_atlas_open(), quality = "high", aspect_ratio = 3) {
        df <- df[complete.cases(df),]
        df$xmin <- cumsum(df$`2017.Pop`)- df$`2017.Pop`
        df$xmax <- cumsum(df$`2017.Pop`)
        
        df <- df %>% mutate(x = (xmin  + xmax)/2, label = df$CountryName)
        #labels <- labels[labels$label %in% c("Ethiopia","Bangladesh","Nigeria","India","China","Egypt, Arab Rep.","South Africa","Brazil","Mexico","Russian Federation","Japan","Germany","United States"),]
        
        
       ggplot(df, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = `2017.GDPpercap`,color=Geo.Region)) +
       geom_rect(fill="white",size=0.000001) + 
      geom_text(aes(x=x, y=`2017.GDPpercap`,label=ifelse(`2017.Pop`>quantile(`2017.Pop`,0.92),label," ")),color="black",hjust=-0.05,check_overlap = T)+
      scale_x_continuous(name= "Countries, scaled by total population",breaks = 0,labels = " ") +
      scale_color_manual(name="  ",
                           values= c("#DF7F2E","#CE1249","#3A943C","#7F3E83","#4D4D4C","#2078B6","#FFCB06"),
                           labels=c(" East Asia & Pacific "," Europe and Central Asia "," Latin America & Caribbean "," Middle East & North Africa "," North America "," South Aisa "," Sub-Saharan Africa "),
                           guide = guide_legend(nrow = 2,label.position = "right",keywidth = 1.0)) +
         geom_hline(yintercept = 16575.45, linetype ="dashed",color="black")+ 
          annotate("text", x= 665, y= 16575.45,label="World: US $16,575",hjust = -0.1,size=3) + #flipped
          style$theme() + 
         scale_y_continuous(name= " ",limits = c(0,70000),breaks = c(20000,40000,60000,80000,100000),labels = thousands())+
          style$theme_barchart() +
          coord_flip()+
          style$theme_legend("bottom") +
          theme(axis.title.x.top = element_blank(),
                #axis.title.x = element_text(margin = margin(1,0,0,0,"lines")),
                #axis.text.x = element_text(angle = 90,hjust = 1),
                legend.text = element_text(margin=margin(r=-1),size = rel(0.9)))
      
      },
      title = "     ",
      subtitle = wbg_name(indicator = "GDP per capita, PPP (2017 $, thousands)"),
      source = "Source: ICP, http://icp.worldbank.org"
    )
  }

  
#Just type the year you want to see the graph for 
fig_1.1_column_width()

#16575.45935


# Without Qatar and four others countries- if you want to see all just change the limit of y axis
# scale_fill_manual(name="  ",values=style$colors$regions,labels=c(" East Asia & Pacific "," Europe and Central Asia "," Latin America & Caribbean "," Middle East & North Africa "," North America "," South Aisa "," Sub-Saharan Africa "),
#To remove the fill option 


####AIC #######


fig_1.2_column_width <- function() {
  df <- read_excel("C:\\Users\\wb551496\\Desktop\\ICP 2020\\R scripts for charts\\1. Martini plot +AICpc\\Book2.xlsx")
  df <- df[,-5]
  
  df <- df %>% arrange(`2017.AICpercap`)
  
  df$Geo.Region<- factor(df$Geo.Region)
  median <- median(df$`2017.AICpercap`)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas_open(), quality = "high", aspect_ratio = 3) {
      
      df$xmin <- cumsum(df$`2017.popshare`)- df$`2017.popshare`
      df$xmax <- cumsum(df$`2017.popshare`)
      
      df <- df %>% mutate(x = (xmin  + xmax)/2, label = df$CountryName)
      label2 <- df[df$label %in% c("Ethiopia","Bangladesh","Nigeria","India","China","Egypt, Arab Rep.","Brazil","Mexico","Russian Federation","Japan","Germany","United States","Congo, Dem. Rep.","Pakistan","Vietnam","Philippines","Indonesia","Iran, Islamic Rep.","Thailand","Turkey"),]
      
      
      ggplot(df, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = `2017.AICpercap`)) +
        geom_rect(color="#00607f",fill="white",size=0.000001) + 
        geom_text(data=label2,aes(x=x, y=`2017.AICpercap`,label=label),color="black",hjust=-0.05,check_overlap = T)+
        scale_x_continuous(name= "Cumulative share of global population (%)",limits=c(0,100),breaks = c(0,10,20,30,40,50,60,70,80,90,100))+
        #scale_color_manual(name="  ",
        #                   values= c("#DF7F2E","#CE1249","#3A943C","#7F3E83","#4D4D4C","#2078B6","#FFCB06"),
         #                  labels=c(" East Asia & Pacific "," Europe and Central Asia "," Latin America & Caribbean "," Middle East & North Africa "," North America "," South Aisa "," Sub-Saharan Africa "),
          #                 guide = guide_legend(nrow = 2,label.position = "right",keywidth = 1.0)) +
        geom_hline(yintercept = 10858, linetype ="dashed",color="black")+ 
        geom_vline(xintercept = 0,color="black",alpha=0.40,size=0.00001)+ 
        annotate("text", x=9.2 , y= 10858,label="World: $10,858",hjust = -0.1,size=3) + #flipped
        style$theme() + 
        scale_y_continuous(name= "AIC per capita",limits = c(0,50000),breaks = c(0,10000,20000,30000,40000),labels = c("$0","$10,000","$20,000","$30,000","$40,000"))+
        style$theme_barchart() +
        coord_flip()+
        style$theme_legend("bottom") +
        theme(axis.title.x.top = element_blank(),
              axis.title.x = element_text(margin = margin(1,0,0,0,"lines")),
              #axis.text.x = element_text(angle = 90,hjust = 1),
              legend.text = element_text(margin=margin(r=-1),size = rel(0.9)))
      
    }
  )
}


#Just type the year you want to see the graph for 
fig_1.2_column_width()


#10841.2463
,
title = "   ",
subtitle = wbg_name(indicator = "AIC per capita, PPP (2017 $, thousands)"),
source = "Source: ICP, http://icp.worldbank.org"
