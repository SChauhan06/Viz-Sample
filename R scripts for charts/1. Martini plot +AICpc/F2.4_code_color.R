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
library(wbgmaps)
library(wbggeo)
library(animation)

library(gifski)
library(gganimate)

source("C:\\Users\\wb551496\\Desktop\\ICP 2020\\R scripts for charts\\styles_viz.R")
year=c(2011:2017)
#year = 2011
#year=2017
style = style_atlas()
aspect_ratio = 1


fig_1.1_column_width <- function(year=c(2011:2017)) {
  indicators <- c("NY.GDP.PCAP.PP.KD", "SP.POP.TOTL")
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicators,
    years = year,
    indicator.wide = TRUE
  )
  
  df <- df %>% arrange(NY.GDP.PCAP.PP.KD)
  df<- df %>% left_join(wbgref$countries$regions)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas_open(), quality = "high", aspect_ratio = 3) {
      df <- df[complete.cases(df),]
      
      df$xmin <- cumsum(df$SP.POP.TOTL) - df$SP.POP.TOTL
      df$xmax <- cumsum(df$SP.POP.TOTL)
      
      df <- df %>% mutate(x = (xmin  + xmax)/2, label = wbgref$countries$labels[iso3c])
     
      #label2 <- df2[df2$label %in% c("Ethiopia","Bangladesh","Nigeria","India","China","Egypt, Arab Rep.","Brazil","Mexico","Russian Federation","Japan","Germany","United States","Congo, Dem. Rep.","Pakistan","Vietnam","Philippines","Indonesia","Iran, Islamic Rep.","Thailand","Turkey"),]
      
      
      p<- ggplot(df, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = NY.GDP.PCAP.PP.KD)) +
        geom_rect(aes(color = region_iso3c),fill="white",size = 0.0001,alpha=0.85) + labs(x="",y="(in thousands)") + 
        geom_text(aes(x=x,y=NY.GDP.PCAP.PP.KD,label=ifelse(SP.POP.TOTL>quantile(SP.POP.TOTL,0.885),label," ")),hjust=-0.05,check_overlap = T)+
        scale_color_manual(name="  ",
                          values=style$colors$regions,
                          labels=c(" East Asia & Pacific "," Europe and Central Asia "," Latin America & Caribbean "," Middle East & North Africa "," North America "," South Aisa "," Sub-Saharan Africa "),
                          guide = guide_legend(nrow = 2,label.position = "right",keywidth = 1.0)) +
        scale_x_continuous(name= "Countries, scaled by total population",breaks = 0,labels = " ") +
        
        #Note: you will have to manually change the world average looking at the value in the data bank
        geom_hline(yintercept = 15508, linetype ="dashed",color="black")+ 
        annotate("text", x= 665558218, y= 15508,label="World: US $15,508",hjust = -0.3,size=3) + #flipped
        style$theme() + scale_y_continuous(limits = c(0,70000),breaks = c(20000,40000,60000,80000,100000),labels = thousands())+
        style$theme_barchart() +
        coord_flip()+
        style$theme_legend("bottom") +
        theme(axis.title.x.top = element_blank(),
              axis.title.x = element_text(margin = margin(1,0,0,0,"lines")),
              #axis.text.x = element_text(angle = 90,hjust = 1),
              legend.text = element_text(margin=margin(r=-1),size = rel(0.9)))
      
    },
    title = "Nearly three-quarters of the world's population lives below the global mean GDP per capita.",
    subtitle = wbg_name(indicator = "GDP per capita, PPP (constant 2011 international $, thousands)", year = year),
    source = "Source: WDI NY.GDP.PCAP.PP.KD; SP.POP.TOTL"
  )
}


#Just type the year you want to see the graph for 
fig_1.1_column_width(2017)



# Without Qatar and four others and 
#change the axis visible in style_viz 
#World Bank values

#2017- 15,508
#2016 - 15,116


