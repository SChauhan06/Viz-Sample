


windowsFonts("Nunito Sans"=windowsFont("Nunito Sans"))
windowsFonts("Avenir Heavy"=windowsFont("Avenir Heavy"))
windowsFonts("Avenir Book"=windowsFont("Avenir Book"))








library(extrafontdb)
loadfonts(device = "win")
library(dplyr)
library(ggrepel)
library(ggplot2)
library(wbgdata)
library(wbgcharts)
library(wbgmaps)
library(wbggeo)
library(countrycode)
library(gtable)
library(grid)
source("C:\\Users\\wb551496\\Desktop\\Summer Assignment\\Graphs in R\\Codes\\styles_viz.R")


year=c(2011:2017)
fig_lineplot <- function(year=c(2011:2017)) {
indicators <- c("NY.GDP.PCAP.PP.KD", "NY.GDP.MKTP.PP.KD","SP.POP.TOTL")
  
  
  df <- wbgdata(
    wbgref$incomes$iso3c,
    indicators,
    years = year,
    indicator.wide = TRUE)
  df$iso3c <- factor(df$iso3c,levels = c("HIC","UMC","LMC","LIC"))
  colnames(df)[2] <- "time1"
  
df <- df[order(df$time1,df$iso3c),]


 
df <- df %>%
  group_by(iso3c) %>%
    mutate(GDP_rebase=(NY.GDP.MKTP.PP.KD/NY.GDP.MKTP.PP.KD[time1==2011])*100) %>%
    mutate(PPP_rebase = (NY.GDP.PCAP.PP.KD/NY.GDP.PCAP.PP.KD[time1==2011]*100))%>%
    mutate(Pop_rebase= (SP.POP.TOTL/SP.POP.TOTL[time1==2011]*100))


  
  figure(
    data = df,
    plot = function(df, style = style_atlas_open(),quality="high",aspect_ratio=1.5) {
      
      facet_labeller = as_labeller(c(
        HIC = "High Income Countries",
        UMC = "Upper Middle Countries",
        LMC= "Lower Middle Countries",
        LIC = "Low Income Countries"))
      
      ggplot(
        df) +
        geom_line(mapping = aes(x= time1,y=PPP_rebase,color="GDP per capita, PPP",linetype="GDP per capita, PPP"),size=style$linesize)+
        geom_line(mapping = aes(x=time1,y=GDP_rebase,color="GDP, PPP",linetype ="GDP, PPP"),size=style$linesize) +
        geom_line(mapping = aes(x=time1,y=Pop_rebase,color="Population",linetype ="Population"),size=style$linesize)+
        scale_color_manual(name=" ",values = c("#00607f","#009acc","black"))+
        scale_linetype_manual(name=" ", values=c("dashed","dotdash","solid"))+
        xlab(" ")+ylab(" ")+
        
        scale_y_continuous(limits = c(100,140),breaks = seq(100,140,by=5),sec.axis= dup_axis()) +
        scale_x_discrete(limits=c(2011:2017)) +
        facet_wrap(~ iso3c, labeller = facet_labeller,ncol = 4,nrow = 1) +
        style$theme() +
        style$theme_legend("bottom")+
        theme(panel.grid.major.x   = element_blank(),
          strip.text.x = element_text(hjust = 0.5,size=11),
              plot.title = element_text(size = 12),
              plot.subtitle = element_text(size=9))
    
      
    },
    title = "GDP per capita in PPP terms rose by over a quarter in middle income countries, whereas high income countries grew by just 8 percent.",
    subtitle = "Growth in GDP,PPP (constant 2011 international $) GDP per capita, PPP (constant 2011 international $) and Population by Income Group (2011 = 100)",
    note="Based on the latest income classification",
    source = "Source: WDI: NY.GDP.PCAP.PP.KD; NY.GDP.MKTP.PP.KD; SP.POP.TOTL"
  )
}




fig_lineplot()


#Dilemma over the title, 20 or a quarter 




