

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
library(plotly)



year=2017
aspect_ratio =0.75


#Type in the year and for that year you have a graph 
fig_gdp_treemap <- function(){
  

  
  
  figure(data = df,
         plot = function(df, style = style_atlas_open(), aspect_ratio = 0.75) {
           
           
           colors <- style$colors$regions
           
          g <-ggplot(df, aes(area=GDP.PPP, fill = `WB Geo`, subgroup = `WB Geo`,label=df$label1)) +
             geom_treemap(stat = "identity", color = "white",size = 0.50,start = 'topleft') +
             geom_treemap_text(color="white",size=6,start = 'topleft',place = 'center',min.size=6)+
             scale_x_continuous(expand = c(0, 0)) +
             scale_fill_manual(name="  ",values = c("#DF7F2E","#CE1249","#3A943C","#7F3E83","#4D4D4C","#2078B6","#FFCB06"), labels = wbgref$regions$labels) +
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

#Convert cmyk to rgb

library(treemap)
library(highcharter)

#Color 3A - EAP
#color 4B- ECS
#color 6 - LAC
#color 5 <- MENA
#Color 2- NAS
#color 7 - SAS
#color 1 - SSF

source("C:\\Users\\wb551496\\Desktop\\ICP 2020\\R scripts for charts\\styles_viz.R")
style = style_atlas()


df <- read_excel("C:\\Users\\wb551496\\Desktop\\ICP 2020\\R scripts for charts\\4. GDP(PPP)- Tree Map\\Code_Treemap.xlsx")
df$`WB Geo` <- factor(df$`WB Geo`)

cost <- currency(df$GDP.PPP,digits = 0L)
trial <- paste(cost,"B",sep="")

trial2 <- paste(round(df$`Global Share`,1),"%",sep="")
df$label1<- paste(df$CountryName,trial2,sep="\n")

colors <- c("#51ae96","#7e78b8","#d70b8c","#df79b0","#00607f","#659ad2","#33c0cd")

tm <- treemap(df, index = c("WB Geo", "CountryName"),
              title = "GDP PPP in $ Billion, and world share (%),2017",
              border.col="white",position.legend = "bottom",
              vSize = "GDP.PPP", vColor = "WB Geo",
              type = "categorical", palette = colors)

  hctreemap(tm, allowDrillToNode = F,legend=T,layoutAlgorithm = "squarified") %>% 
  hc_title(text = "GDP PPP in $ Billion, and world share (%),2017") %>% 
  hc_tooltip(pointFormat = "<b>{point.name}</b>:<br>GDP PPP: {point.value:','.0f}<br>") %>% 
  hc_legend(enabled=T) %>%
  hc_exporting(enabled = TRUE) # enable export

  