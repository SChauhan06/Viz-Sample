

library(tidyverse)
library(wbgcharts)
library(wbgdata)
library(formattable)
library(wbstats)
library(ggplot2)
library(countrycode)
library(ggmosaic)
library(treemapify)
library(readxl)


source("C:\\Users\\wb551496\\Desktop\\ICP 2020\\R scripts for charts\\styles_viz.R")

year=2017
aspect_ratio =0.75
style = style_atlas()


#Type in the year and for that year you have a graph 
fig_gdp_treemap <- function(){

  df <- read_excel("C:\\Users\\wb551496\\Desktop\\ICP 2020\\R scripts for charts\\4. GDP(PPP)- Tree Map\\Code_Treemap.xlsx")
  df$`WB Geo` <- factor(df$`WB Geo`)
  
  cost <- currency(df$GDP.PPP,digits = 0L)
  trial <- paste(cost,"B",sep="")
  trial2 <- paste(round(df$`Global Share`,1),"%",sep="")
  df$label1<- paste(df$CountryName,trial,
                    trial2,sep="\n")
  
  
  figure(data = df,
         plot = function(df, style = style_atlas_open(), aspect_ratio = 0.75) {
           
           
           colors <- c("#51ae96","#7e78b8","#d70b8c","#df79b0","#00607f","#659ad2","#33c0cd")
           
           ggplot(df, aes(area=GDP.PPP, fill = `WB Geo`, subgroup = `WB Geo`,label=df$label1)) +
             geom_treemap(stat = "identity", color = "white",size = 0.50,start = 'topleft') +
             geom_treemap_text(color="white",size=6,start = 'topleft',place = 'center',min.size=6)+
             scale_x_continuous(expand = c(0, 0)) +
             scale_fill_manual(name="  ", values=c("#51ae96","#7e78b8","#d70b8c","#df79b0","#00607f","#659ad2","#33c0cd"), labels = wbgref$regions$labels) +
         guides(fill = guide_legend(nrow = 2, direction = "horizontal")) +
             style$theme()+ 
             style$theme_legend("top") + 
             theme(axis.text = element_blank(), panel.grid = element_blank(),
                   strip.text = element_text(size = rel(1.2), face = "bold"),
                   panel.spacing.y = unit(0.1, "npc"))
           
         },
         aspect_ratio = 0.75
         
  )
}

fig_gdp_treemap()
#style$colors$regions
#levels(df$`WB Geo`)



#title = paste0("GDP PPP in $ Billion, and world share (%) ",year),
#subtitle = "       ",
#source = "Source: ICP 2017"