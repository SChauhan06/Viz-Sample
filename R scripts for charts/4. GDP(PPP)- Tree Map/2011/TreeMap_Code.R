

library(tidyverse)
library(wbgcharts)
library(wbgdata)

library(wbstats)
library(ggplot2)
library(countrycode)
library(ggmosaic)
library(treemapify)


source("C:\\Users\\wb551496\\Desktop\\ICP 2020\\R scripts for charts\\styles_viz.R")

year=2017
aspect_ratio =0.75
style = style_atlas()


#Type in the year and for that year you have a graph 
fig_gdp_treemap <- function(year=2017){
  
  #to use Live API data  
  indicators <- c("NY.GDP.MKTP.PP.CD")
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicators,
    years = year,
    indicator.wide = TRUE
  )
  
  df<- df %>% left_join(wbgref$countries$regions,by="iso3c")
  labels <- df %>% transmute(iso3c = iso3c, labels = wbgref$countries$labels[iso3c])
  df <- df %>% left_join(labels,by="iso3c")
  
  
  
  df <- df[complete.cases(df),]
  
  figure(data = df,
         plot = function(df, style = style_atlas_open(), aspect_ratio = 0.75) {
           
          
           colors <- style$colors$regions
           
           ggplot(df, aes(area= NY.GDP.MKTP.PP.CD, fill = region_iso3c, subgroup = region_iso3c,label=labels)) +
             geom_treemap(stat = "identity", color = "white",size = 0.50,start = 'topleft') +
             geom_treemap_text(color="white",size=11,start = 'topleft',place = 'center',min.size = 11)+
             scale_x_continuous(expand = c(0, 0)) +
             scale_fill_manual(name="  ",values = style$colors$regions, labels = wbgref$regions$labels) +
             guides(fill = guide_legend(nrow = 2, direction = "horizontal")) +
             style$theme()+ 
             style$theme_legend("top") + 
             theme(axis.text = element_blank(), panel.grid = element_blank(),
                   strip.text = element_text(size = rel(1.2), face = "bold"),
                   panel.spacing.y = unit(0.1, "npc"))
         
         },
         aspect_ratio = 0.75,
         title = "East Asia and the Pacific account for a third of global GDP",
         subtitle = paste0("GDP, PPP (current international $), ",year),
         source = "Source: ICP 2017"
  )
}

fig_gdp_treemap()
