library(tidyverse)
library("ggplot2")
require(showtext)
library(ggtext)     
library(ggforce)     
library(ggdist)      
library(magick)     
library(patchwork)  
library(geojsonio)
library(sf)
library(rmapshaper)
library(ggmap)
library(gridExtra)
library(rgdal)
library(raster)
library(cowplot)

# LOADING THE DATASET ----------

tuesdata <- tidytuesdayR::tt_load(2021, week = 36)
bird_baths <- tuesdata$bird_baths

#The taxonomy file can be downloaded here https://data.vertlife.org/birdtree/BLIOCPhyloMasterTax.csv
# Loading the taxonomy file, it will be usful to group birds into families and orders
taxonomyfile<-read.csv("~/YOURFILEPATH/BLIOCPhyloMasterTax.csv")

# Some names in the bird bath dataset do not fit with the ones provided in the Taxonomy dataset,
# with this I'm just manually associate name by name

Missing<-tibble(Species=unique(bird_baths$bird_type)[-which(unique(bird_baths$bird_type) %in%taxonomyfile$English)],
                NewName=c("Chestnut-breasted Munia","Wild Duck","Willie-wagtail","Grey Teal","Red-backed Fairywren",
                          "Variegated Fairywren","Red-tailed Black-cockatoo","Asian Dollarbird",
                          "Black-faced Cuckooshrike","Australian Sacred Ibis","Dusky Myzomela","Australian Wood Duck",
                          "Eurasian Blackbird","Chestnut Teal","Splendid Fairywren","Superb Fairywren","Little Wattlebird",
                          "Ringneck Parrot","Rock Pigeon","Jacky-winter","Crimson Rosella","Scarlet Myzomela"))

bird_baths$newname=bird_baths$bird_type

for (i in 1:22) {
  IDs=which(bird_baths$bird_type==Missing$Species[i])
  bird_baths$newname[IDs]=Missing$NewName[i]
}

bird_baths$Family=taxonomyfile$BLFamilyLatin[match(bird_baths$newname,taxonomyfile$English)]
#I'm not sure what species is intended with wild duck but for sure is anseriform(hopefully also an anatidae)

#Adding some entries in the bird bath dataset
bird_baths$Family[which(bird_baths$bird_type=="Wild Duck")]="Anatidae"
bird_baths$Family=as.factor(bird_baths$Family)
bird_baths$binomial=taxonomyfile$Scientific[match(bird_baths$newname,taxonomyfile$English)]
bird_baths$Orders=taxonomyfile$IOCOrder[match(bird_baths$newname,taxonomyfile$English)]
bird_baths$Orders[which(bird_baths$bird_type=="Wild Duck")]="ANSERIFORMES"


bird_baths$bioregions=as.factor(bird_baths$bioregions)
bird_baths$urban_rural =as.factor(bird_baths$urban_rural)

#Removing NA for years
bird_bathssummed<-bird_baths[which(is.na(bird_baths$survey_year)),]

bird_baths<-bird_baths[-which(is.na(bird_baths$survey_year)),]

# PREPARING THE SHAPE FILES ----
#Shape file can be downloaded here
#https://www.environment.gov.au/fed/catalog/search/resource/details.page?uuid=%7B4A2321F0-DD57-454E-BE34-6FD4BDE64703%7D
#Reading the Shape files from whole australian region 
Bioregions_whole <- st_read("YOURFILEPATH/ibra7_regions.shp")

#Simplifying the map, to plot as a background
Bioregions_whole_simple <- ms_simplify(Bioregions_whole, keep = 0.001)

#Filtering just the regions of our interest
Bioregions_sp <- Bioregions_whole %>% filter(REG_CODE_7 %in% c("BBS","NNC","NSS","SYB","SEH","SCP","SVP","VIM","FLB","SEQ"#,"EYB","JAF","SEC"
                                                               ))
#And then again simplyifing
Bioregions<- ms_simplify(Bioregions_sp, keep = 0.1)

#Ensure that labels of bioregions match to the one preent in the bird bath dataset
Matching_Tib<-tibble(bioregions=unique(bird_baths$bioregions)[match(Bioregions$REG_NAME_6,unique(bird_baths$bioregions))],
       RegionCode=Bioregions$REG_CODE_7)

bird_baths<-full_join(bird_baths, Matching_Tib, by="bioregions")

#Setting the colours

BKg="#F6E8EA"
Rural="#EF626C"
BlueWater="#84DCCF"
Urban="#312F2F"
Urban_Dark="#22181C"

# BASE PLOT----

BASE_Plot<-ggplot()+
  geom_sf(data = Bioregions_whole_simple, color = NA, fill= BKg) +
  theme(
    plot.title = element_blank(),
    plot.caption = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.background = element_rect(fill = BlueWater,colour = NA),
    strip.background = element_rect(fill="Transparent",color="Transparent"),
    plot.background = element_rect(fill=BKg, color=BKg),
    #panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = BCKg), 
    #panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = BCKg),
    plot.margin = margin(5, 2, 5, 2),#t r b l
    panel.grid= element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = .1),
    axis.ticks.length = unit(0, "lines"),
    panel.border = element_rect(colour = BKg, fill=NA, size=0.1),legend.position = "none")  

## DUCKS PLOT -----------

#Summarization
Bioregions$Anseriformes_rural=(bird_baths %>% 
                                 group_by(Orders,RegionCode,urban_rural)%>% 
                                 summarise(x=sum(bird_count)) %>% 
                                 filter(Orders=="ANSERIFORMES" & urban_rural=="Rural"))$x
Bioregions$Anseriformes_urban=(bird_baths %>% 
                                 group_by(Orders,RegionCode,urban_rural)%>% 
                                 summarise(x=sum(bird_count)) %>% 
                                 filter(Orders=="ANSERIFORMES" & urban_rural=="Urban"))$x

#Download from Phylopic the silhouette and then using magick to change its color
png <- magick::image_read("http://phylopic.org/assets/images/submissions/d20848dc-7587-4a4e-8abd-3cc69c7a0f23.128.png")
duck_rur <- grid::rasterGrob(image_colorize(png, opacity = 100, Rural), interpolate = TRUE)

(DUCKS_Rur<- BASE_Plot+
  geom_sf(data = Bioregions, aes(fill=Anseriformes_rural), color = NA ) +
  annotation_custom(duck_rur, ymin = -22 , ymax = -27, xmin = 134, xmax = 139) +
  scale_fill_gradient(low = BKg, high = Rural)+
  coord_sf( ylim =c(-23 ,-39) ,xlim = c(135, 155)))

duck_ur <- grid::rasterGrob(image_colorize(png, opacity = 100, Urban), interpolate = TRUE)

(DUCKS_Ur<- BASE_Plot+
    geom_sf(data = Bioregions, aes(fill=Anseriformes_urban), color = NA ) +
    annotation_custom(duck_ur, ymin = -22 , ymax = -27, xmin = 134, xmax = 139) +
    scale_fill_gradient(low = BKg, high = Urban)+
    coord_sf( ylim =c(-23 ,-39) ,xlim = c(135, 155))   )

## PARROTS PLOT -----------

#Summarization
Bioregions$Psittaciformes_rural=(bird_baths %>% 
                                 group_by(Orders,RegionCode,urban_rural)%>% 
                                 summarise(x=sum(bird_count)) %>% 
                                 filter(Orders=="PSITTACIFORMES" & urban_rural=="Rural"))$x
Bioregions$Psittaciformes_urban=(bird_baths %>% 
                                 group_by(Orders,RegionCode,urban_rural)%>% 
                                 summarise(x=sum(bird_count)) %>% 
                                 filter(Orders=="PSITTACIFORMES" & urban_rural=="Urban"))$x

#Download from Phylopic
png <- magick::image_read("http://phylopic.org/assets/images/submissions/2236b809-b54a-45a2-932f-47b2ca4b8b7e.128.png")
parrot_rur <- grid::rasterGrob(image_colorize(png, opacity = 100, Rural), interpolate = TRUE)

(PARROT_Rur<- BASE_Plot+
    geom_sf(data = Bioregions, aes(fill=Psittaciformes_rural), color = NA ) +
    annotation_custom(parrot_rur, ymin = -22.5 , ymax = -27.5, xmin = 134, xmax = 139) +
    scale_fill_gradient(low = BKg, high = Rural)+
    coord_sf( ylim =c(-23 ,-39) ,xlim = c(135, 155)) )

parrot_ur <- grid::rasterGrob(image_colorize(png, opacity = 100, Urban), interpolate = TRUE)

(PARROT_Ur<-BASE_Plot+
    geom_sf(data = Bioregions, aes(fill=Psittaciformes_urban), color = NA ) +
    annotation_custom(parrot_ur, ymin = -22.5 , ymax = -27.5, xmin = 134, xmax = 139) +
    scale_fill_gradient(low = BKg, high = Urban)+
    coord_sf( ylim =c(-23 ,-39) ,xlim = c(135, 155)) )


## COLUMBID PLOT -----------

#Summarization

Bioregions$Columbiformes_rural=(bird_baths %>% 
                                   group_by(Orders,RegionCode,urban_rural)%>% 
                                   summarise(x=sum(bird_count)) %>% 
                                   filter(Orders=="COLUMBIFORMES" & urban_rural=="Rural"))$x
Bioregions$Columbiformes_urban=(bird_baths %>% 
                                   group_by(Orders,RegionCode,urban_rural)%>% 
                                   summarise(x=sum(bird_count)) %>% 
                                   filter(Orders=="COLUMBIFORMES" & urban_rural=="Urban"))$x

#Download from Phylopic
png <- magick::image_read("http://phylopic.org/assets/images/submissions/3644ff17-3cd1-4c98-afdb-91113d4e2cca.128.png")
pidgeon_rur <- grid::rasterGrob(image_colorize(png, opacity = 100, Rural), interpolate = TRUE)

(PIDGEONS_Rur<-BASE_Plot +
    geom_sf(data = Bioregions, aes(fill=Columbiformes_rural), color = NA ) +
    annotation_custom(pidgeon_rur, ymin = -22 , ymax = -27, xmin = 134, xmax = 139) +
    scale_fill_gradient(low = BKg, high = Rural)+
    coord_sf( ylim =c(-23 ,-39) ,xlim = c(135, 155)) )

pidgeon_ur <- grid::rasterGrob(image_colorize(png, opacity = 100, Urban), interpolate = TRUE)

(PIDGEONS_Ur<- BASE_Plot +
    geom_sf(data = Bioregions, aes(fill=Columbiformes_urban), color = NA ) +
    annotation_custom(pidgeon_ur, ymin = -22 , ymax = -27, xmin = 134, xmax = 139) +
    scale_fill_gradient(low = BKg, high = Urban)+
    coord_sf( ylim =c(-23 ,-39) ,xlim = c(135, 155)) )


## KINGFISHERS PLOT -----------

#Summarization

Bioregions$Coraciiformes_rural=(bird_baths %>% 
                                  group_by(Orders,RegionCode,urban_rural)%>% 
                                  summarise(x=sum(bird_count)) %>% 
                                  filter(Orders=="CORACIIFORMES" & urban_rural=="Rural"))$x
Bioregions$Coraciiformes_urban=(bird_baths %>% 
                                  group_by(Orders,RegionCode,urban_rural)%>% 
                                  summarise(x=sum(bird_count)) %>% 
                                  filter(Orders=="CORACIIFORMES" & urban_rural=="Urban"))$x

#Download from Phylopic
png <- magick::image_read("http://phylopic.org/assets/images/submissions/6209c9be-060e-4d7f-bc74-a75f3ccf4629.128.png")
kingfisher_rur <- grid::rasterGrob(image_colorize(png, opacity = 100, Rural), interpolate = TRUE)

(KINGFISHERS_Rur<-BASE_Plot +
    geom_sf(data = Bioregions, aes(fill=Coraciiformes_rural), color = NA ) +
    annotation_custom(kingfisher_rur, ymin = -22 , ymax = -27, xmin = 134, xmax = 139) +
    scale_fill_gradient(low = BKg, high = Rural)+
    coord_sf( ylim =c(-23 ,-39) ,xlim = c(135, 155)) )

kingfisher_ur <- grid::rasterGrob(image_colorize(png, opacity = 100, Urban), interpolate = TRUE)

(KINGFISHERS_Ur<- BASE_Plot +
    geom_sf(data = Bioregions, aes(fill=Coraciiformes_urban), color = NA ) +
    annotation_custom(kingfisher_ur, ymin = -22 , ymax = -27, xmin = 134, xmax = 139) +
    scale_fill_gradient(low = BKg, high = Urban)+
    coord_sf( ylim =c(-23 ,-39) ,xlim = c(135, 155)) )


GeographicMaps<-((DUCKS_Rur|DUCKS_Ur)/(PARROT_Rur|PARROT_Ur)/(PIDGEONS_Rur|PIDGEONS_Ur)/(KINGFISHERS_Rur|KINGFISHERS_Ur))




# PLOTTING HISTOGRAMS ----
#Let's make a summarized version, easier to treat with geom_bars

Summarized_Baths<-bird_baths %>% filter(Orders=="PASSERIFORMES") %>% group_by(Family,survey_year,urban_rural) %>% 
  summarise(x=sum(bird_count)) %>%  
  spread(key =survey_year,value= x) %>% group_by(Family) %>% 
  mutate(mean=sum(`2014`+`2015`),n=1:n()) %>% ungroup() %>%  arrange(-mean,Family,n) %>% 
  mutate(Familyreord = fct_reorder(Family, mean)) %>% slice(1:30)

#Utility function to compute the summed bird of each family each year
Summarized_Baths$Neg_lab<-rep((Summarized_Baths %>% filter(urban_rural=="Rural"))$`2014`+
  (Summarized_Baths %>% filter(urban_rural=="Urban"))$`2014`,each=2)
Summarized_Baths$Pos_lab<-rep((Summarized_Baths %>% filter(urban_rural=="Rural"))$`2015`+
                                (Summarized_Baths %>% filter(urban_rural=="Urban"))$`2015`,each=2)

Summarized_Baths<-left_join(x = Sumarized_Baths,y=LabelCoords, by="Family" )  

(BarPlot<-Summarized_Baths%>% 
  ggplot() + 
  geom_bar(aes(x=as.numeric(Familyreord) ,y=-`2014`,fill=urban_rural),stat = "identity") + 
  geom_bar(aes(x=as.numeric(Familyreord) ,y=`2015`,fill=urban_rural),stat = "identity") + 
    geom_hline(yintercept = 0,color=BKg)+
    geom_richtext(data= tibble(label = c(800,400,200,50,50,200,400,800),
                             y =  c(-800,-400,-200,-50,50,200,400,800),
                             x=16,
                             vjust=c(0,0,0,0,1,1,1,1)),
                  aes(x=x,y=y,label=label,vjust=vjust),  angle =90,hjust=0
                ,color=Rural,fill = NA, label.color = NA,family="Unica One") +
  geom_text(data= tibble(label = c("Number of total passeriform birds observed\nby year divided by taxonomic family"),
                               y =  0,
                               x=32.25,
                               hjust=0.5),
                  aes(x=x,y=y,label=label,hjust=hjust),
                  color=Urban,fill = NA, label.color = NA,family="Unica One",size=4) +
  geom_richtext(data= tibble(label = c(2014,2015),
                             y =  c(-800,800),
                             x=32.25,
                             hjust=c(0,1)),
                aes(x=x,y=y,label=label,hjust=hjust),
                color=Urban,fill = NA, label.color = NA,family="Unica One",size=9) +
    
  scale_x_continuous(limits = c(16,33),expand = c(0,0) )+
  scale_y_continuous(breaks=c(-800,-400,-200,-50,0,50,200,400,800),limits = c(-825,825))+

  coord_flip()+
  scale_fill_manual(values = c (Rural,Urban))+
  theme(
    plot.title = element_blank(),
    plot.caption = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.background = element_rect(fill = BKg,colour = NA),
    strip.background = element_rect(fill="Transparent",color="Transparent"),
    plot.background = element_rect(fill=BKg, color=BKg),
    #panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = BCKg), 
    #panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = BCKg),
    plot.margin = margin(5, 10, 5, 10),#t r b l
    panel.grid= element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(size = .1,color=Rural),
    axis.ticks.length = unit(0, "lines"),
    panel.border = element_rect(colour = Urban_Dark, fill=NA, size=0.4),legend.position = "none"
    )  
)
#BirdSilouehettes 

png <- magick::image_read("http://phylopic.org/assets/images/submissions/312dcb35-7b82-4496-9db5-2bf7de55e810.256.png")
Meliphagidae <- grid::rasterGrob(image_colorize(png, opacity = 100, Urban), interpolate = TRUE)
png <- magick::image_read("http://phylopic.org/assets/images/submissions/ac6920cc-61a3-4d33-86b6-e8da07011f87.128.png")
Cracticidae<- grid::rasterGrob(image_colorize(png, opacity = 100, Urban), interpolate = TRUE)
png <- magick::image_read("http://phylopic.org/assets/images/submissions/d708f0e9-1e19-4507-8576-a1e0c870a42c.128.png")
Acanthizidae <-grid::rasterGrob(image_colorize(png, opacity = 100, Urban), interpolate = TRUE)
png <- magick::image_read("http://phylopic.org/assets/images/submissions/35afd1db-6ff2-42fa-a192-edd039c0cd0c.128.png")
Rhipiduridae <-grid::rasterGrob(image_colorize(png, opacity = 100, Urban), interpolate = TRUE)
png <- magick::image_read("http://phylopic.org/assets/images/submissions/52adef9e-6d59-46e8-9c3e-7fc593c55a42.256.png")
Maluridae <-grid::rasterGrob(image_colorize(png, opacity = 100, Urban), interpolate = TRUE)
png <- magick::image_read("http://phylopic.org/assets/images/submissions/f71a3424-1afd-493a-bf2a-495004030dcc.128.png")
Estrildidae <-grid::rasterGrob(image_colorize(png, opacity = 100, Urban), interpolate = TRUE)
Maluridae <-grid::rasterGrob(image_colorize(png, opacity = 100, Urban), interpolate = TRUE)
png <- magick::image_read("http://phylopic.org/assets/images/submissions/e66d17b1-2c95-42a6-b6dc-9df4513c6601.128.png")
Monarchidae<-grid::rasterGrob(image_colorize(png, opacity = 100, Urban), interpolate = TRUE)
png <- magick::image_read("http://phylopic.org/assets/images/submissions/c3c19b65-cb8d-40bd-b1a6-82a3277bcd4f.128.png")
Ptilonorhynchidae<-grid::rasterGrob(image_colorize(png, opacity = 100, Urban), interpolate = TRUE)
png <- magick::image_read("http://phylopic.org/assets/images/submissions/4f25df70-f5e0-410b-822c-a229e24d4546.128.png")#Maxime Dahirel
Sturnidae<-grid::rasterGrob(image_colorize(png, opacity = 100, Urban), interpolate = TRUE)
png <- magick::image_read("http://phylopic.org/assets/images/submissions/c961d79e-eeba-40c1-a4da-f93f22b264c7.128.png")
Petroicidae<-grid::rasterGrob(image_colorize(png, opacity = 100, Urban), interpolate = TRUE)
png <- magick::image_read("http://phylopic.org/assets/images/submissions/3b74c3e5-1ffa-4089-95b1-371f1b71fce0.128.png")#Andrew Butko
Passeridae<-grid::rasterGrob(image_colorize(png, opacity = 100, Urban), interpolate = TRUE)
png <- magick::image_read("http://phylopic.org/assets/images/submissions/14962f0a-c7db-4df2-89e3-6cfc593faed1.128.png")
Turdidae<-grid::rasterGrob(image_colorize(png, opacity = 100, Urban), interpolate = TRUE)
png <- magick::image_read("http://phylopic.org/assets/images/submissions/9d382526-f8be-457a-b97d-f323a146b42a.128.png")
Zosteropidae<-grid::rasterGrob(image_colorize(png, opacity = 100, Urban), interpolate = TRUE)
png <- magick::image_read("http://phylopic.org/assets/images/submissions/0f5aae41-01a7-4219-be63-24f91ac6f6b8.128.png")#Jean-Raphaël Guillaumin and T. Michael Keesey 
Corvidae<-grid::rasterGrob(image_colorize(png, opacity = 100, Urban), interpolate = TRUE)
png <- magick::image_read("http://phylopic.org/assets/images/submissions/09cc9689-1e5e-4cbb-8d85-c65fa15a0521.128.png")#Michelle Site
Colluricinclidae<-grid::rasterGrob(image_colorize(png, opacity = 100, Urban), interpolate = TRUE)


#To ease label positioning
LabCoord_Raw<-tibble::tribble(
    ~ymin,   ~ymax,   ~xmin,   ~xmax,
  -850L, -725L,  29.5,  31.5,
   750L,  850L,  28.5,  30.5,
  -650L, -550L, 27.75,    29.5,
   525L,  625L,    27,  29.5,
  -800L, -700L,  26, 28.25,
   675L,  775L,  24.75,  27.75,
  -675L, -575L, 23.5,  26.25,
   650L,  750L,  22,  25,
  -820L, -700L,    22.5,  23.7,
   700L,  775L, 19.5, 21.5, 
  -775L, -650L,  20.5,  21.5,
   475L,  600L,    17.75,    20.75,  
  -575L, -450L,    19,    20,
   600L,  725L,  17,  18.5,
  -725L, -600L,    16.75,    18.75
  )
LabCoord_Raw$y=(LabCoord_Raw$ymin+LabCoord_Raw$ymax)/2
LabCoord_Raw$x=(LabCoord_Raw$xmin+LabCoord_Raw$xmax)/2
LabCoord_Raw$label=unique(Summarized_Baths$Family)
LabCoord_Raw$yend=c(rbind(-(Summarized_Baths %>% filter(n==1))$Neg_lab[c(1,3,5,7,9,11,13,15)],(Summarized_Baths %>% filter(n==1))$Pos_lab[c(2,4,6,8,10,12,14,14)]))[-16]
LabCoord_Raw$xend=31:17

#Unica e Crimson
BarPlot_Annotated<-(BarPlot+
  geom_segment(data= LabCoord_Raw, aes(xend=xend,yend=yend,x=x,y=y),lty="dotdash",color=Rural,alpha=.5 )+
  geom_richtext(data= LabCoord_Raw,
                aes(x=x,y=y,label=label),vjust=1,nudge_x = -0.75, family="Crimson Text",
                color=Urban,fill = NA, label.color = NA)+
  annotation_custom(Meliphagidae, ymin = -850 , ymax = -725, xmin = 29.5, xmax = 31.5)+
  annotation_custom(Cracticidae,  ymin = 750, ymax = 850 , xmin = 28.5, xmax = 30.5)+
  annotation_custom(Acanthizidae, ymin = -650 , ymax = -550, xmin = 27.75, xmax = 29.5)+
  annotation_custom(Rhipiduridae, ymin = 525 , ymax = 625, xmin = 27, xmax = 29.5)+
  annotation_custom(Maluridae, ymin = -800 , ymax = -700, xmin = 26, xmax = 28.25)+
  annotation_custom(Estrildidae, ymin = 675 , ymax = 775, xmin = 24.75, xmax = 27.75)+
  annotation_custom(Monarchidae, ymin = -675 , ymax = -575, xmin = 23.5, xmax = 26.25)+
  annotation_custom(Ptilonorhynchidae, ymin = 650 , ymax = 750, xmin = 22, xmax = 25)+
  annotation_custom(Sturnidae, ymin = -820 , ymax = -700, xmin = 22.5, xmax = 23.7)+
  annotation_custom(Petroicidae, ymin = 700 , ymax = 775, xmin = 19.5, xmax = 21.5)+
  annotation_custom(Passeridae, ymin = -775 , ymax = -650, xmin = 20.5, xmax = 21.5)+
  annotation_custom(Turdidae, ymin = 475 , ymax = 600, xmin = 17.75, xmax = 20.75)+
  annotation_custom(Zosteropidae, ymin = -575 , ymax = -450, xmin = 19, xmax = 20)+
  annotation_custom(Corvidae, ymin = 600 , ymax =725, xmin = 17, xmax = 18.5)+
  annotation_custom(Colluricinclidae, ymin = -725 , ymax = -600, xmin = 16.75, xmax = 18.75)
)
# FINAL PLOT--------

layout <- "
AAAAAAAAABBBB
AAAAAAAAABBBB
AAAAAAAAABBBB
"
#Patchwork entry
BarPlot_Annotated+GeographicMaps+ 
  plot_layout(design = layout)+ plot_annotation(
    theme = theme(plot.background = element_rect(fill=BKg, color=BKg),
                  plot.title = element_text(size = 20,lineheight = 0.9,family ="Unica One",color=Rural),
                  plot.subtitle =element_markdown(family ="Crimson Text" ,size = 14,colour ="#775562" ),
                  plot.caption =element_text(lineheight = 0.9,family = "Unica One", color="#775562"),),
    title = 'Australian birds like having a bath',
    subtitle = 'Bird sights count at bath sites from<b style="font-size:14pt;color:#EF626C;"> rural </b>and <b style="font-size:14pt;color:#312F2F">urban </b>areas from different australian regions. Left section shows counts from passeriform birds from the whole country whereas right section  \nshows the most common bird families divided by bioregion. Color intensity represent frequency of sights.',
    caption = 'Source:Cleary et al.Avian Assemblages at Bird Baths: A Comparison of Urban and Rural Bird Baths in Australia. (2016). Image credit: Phylopic and to Maxime Dahirel, Andrew Butko, Jean-Raphaël Guillaumin, T. Michael Keesey and Michelle Site.'
  )

ggsave("BirdBaths.pdf", width = 40.09091, height = 21, units = "cm", device = cairo_pdf)
ggsave("BirdBaths.png", width = 40.09091, height = 21, units = "cm")
