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
library(hrbrthemes)
#Need to install these packages if one has to animate
#library(animation)
#library(gifski)
#library(gganimate)

source("C:\\Users\\wb551496\\Desktop\\ICP 2020\\R scripts for charts\\styles_viz.R")
aspect_ratio = 1
style = style_atlas_open()


fig_1.1_column_width <- function() {
  df1 <- read_excel("C:\\Users\\wb551496\\Desktop\\ICP 2020\\R scripts for charts\\1. Martini plot +AICpc\\Book1.xlsx",sheet = 1)
  df2 <- read_excel("C:\\Users\\wb551496\\Desktop\\ICP 2020\\R scripts for charts\\1. Martini plot +AICpc\\Book1.xlsx",sheet = 2)
  
  df1 <- df1 %>% arrange(`2011.Index of Real GDP`)
  df2 <- df2 %>% arrange(`2017.Index of Real GDP`)
  
  df1$Geo.Region<- factor(df1$Geo.Region)
  df2$Geo.Region<- factor(df2$Geo.Region)

  
  figure(
    data = df1,
    plot = function(df1, style = style_atlas_open(), quality = "high", aspect_ratio = 3) {
      
      
      df1$xmin <- cumsum(df1$`2011.popshare`)- df1$`2011.popshare`
      df1$xmax <- cumsum(df1$`2011.popshare`)
      
      df2$xmin <- cumsum(df2$`2017.popshare`)- df2$`2017.popshare`
      df2$xmax <- cumsum(df2$`2017.popshare`)
      
      
      df1 <- df1 %>% mutate(x = (xmin  + xmax)/2, label = df1$CountryName)
      df1$`2011.Index of Real GDP` <- df1$`2011.Index of Real GDP`*-1
      df2 <- df2 %>% mutate(x = (xmin  + xmax)/2, label = df2$CountryName)
      
      #To figure out the names of the top 20 biggest economies by population
      label = ifelse(df1$`2011.Pop`>quantile(df1$`2011.Pop`,0.89),df1$label,"")
      label_0 = ifelse(df2$`2017.Pop`>quantile(df2$`2017.Pop`,0.885),df2$label,"")
      
      label1 <- df1[df1$label %in% c("Ethiopia","Bangladesh","Nigeria","India","China","Egypt, Arab Rep.","Brazil","Mexico","Russian Federation","Japan","Germany","United States","Congo, Dem. Rep.","Pakistan","Vietnam","Philippines","Indonesia","Iran, Islamic Rep.","Thailand","Turkey"),]
      label2 <- df2[df2$label %in% c("Ethiopia","Bangladesh","Nigeria","India","China","Egypt, Arab Rep.","Brazil","Mexico","Russian Federation","Japan","Germany","United States","Congo, Dem. Rep.","Pakistan","Vietnam","Philippines","Indonesia","Iran, Islamic Rep.","Thailand","Turkey"),]
      
      
      
        ggplot() +
        ylab(" ")+
        geom_rect(data=df2,mapping=aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = `2017.Index of Real GDP`,color="2017"),fill="white",size=0.00001) + 
        geom_rect(data=df1,mapping=aes(xmin = xmin, xmax = xmax, ymin = 0, ymax=`2011.Index of Real GDP`,color="2011"),fill="white",size=0.00001) + 
        geom_text(data=label2,mapping=aes(x=x,y=`2017.Index of Real GDP`,label=label),size=2.5,hjust=-0.20,check_overlap = F,fontface="bold")+
        geom_text(data=label1,mapping=aes(x=x,y=`2011.Index of Real GDP`,label=label),size=2.5,hjust= 1.15,check_overlap = T,fontface="bold")+
        scale_color_manual(name="  ",values=c("#00607f","#7f1f00"),guide = guide_legend(nrow = 1,label.position = "right",keywidth = 1.0)) +
        scale_x_continuous(name= "Cumulative share of global population(%)",breaks = c(10,20,30,40,50,60,70,80,90,100)) +
        geom_vline(xintercept = min(df1$`2011.Pop`),color="black",alpha=0.40,size=0.00001)+ 
        geom_hline(yintercept = -100, linetype ="dashed",color="black",alpha=0.40,size=0.00001)+
        geom_hline(yintercept = 100, linetype ="dashed",color="black",alpha=0.40,size=0.00001)+
        #annotate("text", x= 66.5, y= 100,label="World = 100",hjust = -0.3,size=3) + 
        #annotate("text", x= -166.5, y= -100,label="World = 100",hjust = -0.3,size=3) + #flipped
        style$theme() + 
  
        scale_y_continuous(name = "Index of GDP per capita (world=100)",limits = c(-700,700),breaks = c(-600,-500,-400,-300,-200,-100,0,100,200,300,400,500,600),labels = c("600","500","400","300","200","100","0","100","200","300","400","500","600"))+

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
fig_1.1_column_width()

#label=ifelse(`2017.Pop`>quantile(`2017.Pop`,0.92),label,"")


#,
#title = "Index of GDP per capita, PPP (World=100)",
#subtitle = wbg_name(indicator = "20 biggest countries by population in both years"),
#source = "Source: ICP, http://icp.worldbank.org"