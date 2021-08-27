library(tidyverse)
library("ggplot2")
library("ape")
library("phytools")
library("ggtree")
require(showtext)
library(systemfonts) 
library(scico)      
library(ggtext)     
library(ggforce)    
library(ggdist)

tuesdata <- tidytuesdayR::tt_load('2021-08-24')
tuesdata <- tidytuesdayR::tt_load(2021, week = 35)

#Load the dataset
lemurs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')


#Load the taxonomy, note that I changed some labels (subspecies were moved to species status)
Taxonomy<-tibble::tribble(
  ~taxon,                    ~Latin.name,                   ~Common.name,
  "CMEAD",          "Cheirogaleus medius",       "Fat-tailed dwarf lemur",
  "DMAD", "Daubentonia madagascariensis",                      "Aye-aye",
  "EALB",            "Eulemur albifrons",    "White-fronted brown lemur",
  "ECOL",             "Eulemur collaris",         "Collared brown lemur",
  "ECOR",            "Eulemur coronatus",                "Crowned lemur",
  "EFLA",           "Eulemur flavifrons",        "Blue-eyed black lemur",
  "EFUL",               "Eulemur fulvus",           "Common brown lemur",
  "EMAC",               "Eulemur macaco",                  "Black lemur",
  "EMON",               "Eulemur mongoz",               "Mongoose lemur",
  "ERUB",          "Eulemur rubriventer",            "Red-bellied lemur",
  "ERUF",                "Eulemur rufus",      "Red-fronted brown lemur",
  "ESAN",             "Eulemur sanfordi",        "Sanford’s brown lemur",
  "EUL",              "Eulemur Eulemur",                       "hybrid",
  "GMOH",                "Galago moholi",               "Mohol bushbaby",
  "HGG",            "Hapalemur griseus",  "Eastern lesser bamboo lemur",# griseus griseus
  "LCAT",                  "Lemur catta",            "Ring-tailed lemur",
  "LTAR",            "Loris tardigradus",                "Slender loris",
  "MMUR",           "Microcebus murinus",             "Gray mouse lemur",
  "MZAZ",              "Mirza coquereli",   "Northern giant mouse lemur",
  "NCOU",           "Nycticebus coucang",                   "Slow loris",
  "NPYG",          "Nycticebus pygmaeus",             "Pygmy slow loris",
  "OGG",           "Otolemur garnettii",      "Northern greater galago",# garnetti garnetti
  "PCOQ",        "Propithecus coquereli",            "Coquerel’s sifaka",
  "PPOT",           "Perodicticus potto",                        "Potto",
  "VAR",              "Varecia Varecia",                       "hybrid",
  "VRUB",                "Varecia rubra",             "Red ruffed lemur",
  "VVV",            "Varecia variegata", "Black-and-white ruffed lemur" #sarebbe variegata variegata
)

#PHYLOGENY BUILDING 
#The phylogeny is from James P. Herrera, Liliana M. Dávalos, Phylogeny and Divergence Times of Lemurs Inferred with Recent and Ancient Fossils in the Tree. (2016)
#Specify your folder names to load the file
MCCLem<-read.tree("LemursPhylogeny.tre")

#Let's add a column with the species name formatted in the same way as they appear in the phylogentic tree
Taxonomy$tiplab<-gsub(pattern = " ",replacement = "_",x = Taxonomy$Latin.name)

#This command print out all the species in the Taxonomy that are present in the Phylogenetic tree
Taxonomy$tiplab[which(Taxonomy$tiplab %in% MCCLem$tip.label)]

#This removes from the taxonomy object all secies that are not present in the phylogeny
Taxonomy<-Taxonomy[which(Taxonomy$tiplab %in% MCCLem$tip.label),]

#Now we need to prunine the tree from taxa that are absent in the Lemurs dataset
LemurTree<-ape::drop.tip(phy = MCCLem, tip = c("Cheirogaleus_medius", MCCLem$tip.label[-which(MCCLem$tip.label %in% Taxonomy$tiplab)]))

#Merge datasets
lemurs<-left_join(lemurs,Taxonomy,.by = `taxon` )

#Let's then filter by species in the phylogeny and only adults
lemursPhylo<-lemurs %>% 
  filter(tiplab %in% LemurTree$tip.label) %>%  
  filter(age_category=="adult" ) 

#Then a little bit of data manipulation
summary<-lemursPhylo %>% group_by(tiplab,sex) %>%
  summarise(med=median(weight_g),
            sd=sd(weight_g),
            Low=quantile(weight_g,probs =0.055) , Up= quantile(weight_g,probs=0.945),
            mean=mean(weight_g),
            gestasion=mean(expected_gestation_d,na.rm = T),
            n=n()) %>% 
  group_by(tiplab) %>% 
  mutate(groupmean=mean(med)) %>% 
  ungroup() %>% 
  arrange(groupmean,sex)%>% mutate(N=1:n())  

#Computing size differences between male and females (log scale)
weight_diff<-tibble(
  Value=log(summary$med[which(summary$sex=="F")])-log(summary$med[which(summary$sex=="M")]),
  label=summary$tiplab[which(summary$sex=="M")])

#Then converting these estimates to a vector, for the ancestral state estimation
WeightdiffVect=weight_diff$Value
names(WeightdiffVect)=weight_diff$label 
Ancstates=fastAnc(LemurTree,x = WeightdiffVect )

#Ancestral estimation of  absolute Size
SizeVec=LabTibble$med
names(SizeVec)=summary$tiplab[which(summary$sex=="M")] 
AncstatesSize=fastAnc(LemurTree,x = SizeVec )

#Now the Silhouettes with the Phylopic repository, it may take a little bit 
Silhouettes<-ggimage::phylopic_uid(LemurTree$tip.label)
#Manually filling missing entries
Silhouettes["Hapalemur_griseus",2]=Silhouettes["Lemur_catta",2]
Silhouettes["Varecia_rubra",2]=Silhouettes["Lemur_catta",2]
Silhouettes[c("Eulemur_sanfordi","Eulemur_albifrons","Eulemur_rufus","Eulemur_flavifrons"),2]=Silhouettes["Eulemur_rubriventer",2]

#Now we gonna make an another ancestral state reconstruction but on the silhouettes!
#What is the look of the common ancestor?
SilVec=as.factor(Silhouettes$uid)
names(SilVec)=rownames(Silhouettes)

fitER <- ape::ace(SilVec,LemurTree,model="ER",type="discrete")
ancsilhouettes <- as.data.frame(fitER$lik.anc)
MaxlikAnc<-tibble(Lik=apply(ancsilhouettes,1,max),
                  node = 1:LemurTree$Nnode+Ntip(LemurTree))

#Now we gonna assume that at each node  the silhoutte will be the one with the highest likelihood 
#I guess it's ok for aestetichal purposes 
SilhouetteID=rep(NA,times=LemurTree$Nnode)
for (i in 1:LemurTree$Nnode) {
  SilhouetteID[i]=which(ancsilhouettes[i,]==MaxlikAnc$Lik[i])
}
MaxlikAnc$uid=colnames(ancsilhouettes)[SilhouetteID]

#Now we can finally generate the tree with all the attached estimated vaulues...
merged_tree=full_join(LemurTree, weight_diff, by="label")
#and add other ancestral infos
merged_tree@data$Value[25:47]=Ancstates
merged_tree@data$Species=c(gsub(merged_tree@phylo$tip.label,pattern = "_",replacement = " "),rep(NA,times=47-24))

merged_tree@data$uid=c(Silhouettes$uid[match(LemurTree$tip.label,Silhouettes$name)],
                       MaxlikAnc$uid)
merged_tree@data$Size=c(SizeVec[match(LemurTree$tip.label,names(SizeVec))],
                        AncstatesSize)/1000
BCKg="#FAF9C9"

#Here I had to manually displace each silhouette to make them readable trough the phylogeny
Xoff<-c(-.02,-.02,-.02,-.02,-.02,-.02, -.02, -.02, -.02, -.02, -.02, -.02, -.02, -.015,  -.015, -.015, -.02, -.02, -.02,-.02,  -.02, -.02, -.015)
Yoff<-c(0.6, 0.4, 0.4, 0.4, 0.4,   .2,  0.5,  0.6,  0.6,  0.6,  0.6,  0.6,  0.6,  0.6,  0.7,   0.6,  0.7,  0.6,  0.6,  0.6,  0.6,  0.6, 0.7)

ggtree(merged_tree,aes(colour=Value),
       continuous=T,size=1.1)  + 
  geom_tippoint(aes(colour=Value,size=Size)) +
  geom_nodepoint(aes(size=Size)) +
  geom_nodelab(aes(image=uid), geom="phylopic", size=0.02,nudge_x = Xoff, nudge_y = Yoff)+
  scale_color_gradientn(colours=scico(n = 9,palette = "hawaii",begin =1 ,end = 0))+
  geom_tiplab(aes(label=Species),color=scico(n = 9,palette = "hawaii")[1],family='Montserrat',fontface = "italic",offset = 0.01)  + xlim(0,0.8)+
  labs(color = "Difference between female \nand male size (log scale)",
       size = "Average weight (Kgs)" )  +
  labs(title = "Lemurs sexual dimorphism evolutionary history",
       subtitle = "Lemurs come in different sizes, and in each species being male or female matters.\nBut what matters even more is the impact of the tree of life, closer species tends to resemble more both in terms of absolute size and differences between male and female (sexual dimorphism)\nthan any other species randomically picked!",
       caption = "Data source: Zehr, SM, Roach RG, Haring D, Taylor J, Cameron FH, Yoder AD.Life history profiles for 27 strepsirrhine primate taxa generated using captive data from the Duke Lemur Center.(2014)
       \nJames P. Herrera, Liliana M. Dávalos, Phylogeny and Divergence Times of Lemurs Inferred with Recent and Ancient Fossils in the Tree. (2016)")+
  theme(plot.title = element_text(size = 20,lineheight = 0.9,family = "Work Sans",face = "bold",color=scico(n = 9,palette = "hawaii")[4]),
        plot.subtitle =element_text(lineheight = 0.9,family = "Montserrat",color=scico(n = 9,palette = "hawaii")[1]),
        plot.caption =element_text(size=6,lineheight = 0.9,family = "Montserrat",color=scico(n = 9,palette = "hawaii")[1]),
        legend.title  = element_text(lineheight = 0.9,family = "Work Sans",color=scico(n = 9,palette = "hawaii")[4]),
        plot.background = element_rect(fill = BCKg,
                                       colour = BCKg,
                                       size = 0.5, linetype = "solid"),
        legend.key = element_rect(fill = BCKg,
                                  colour = BCKg,
                                  size = 0.5, linetype = "solid"),
        legend.background = element_rect(fill = BCKg,
                                         colour = BCKg,
                                         size = 0.5, linetype = "solid"),
        legend.text= element_text(lineheight = 0.9,family = "Montserrat",color=scico(n = 9,palette = "hawaii")[1]),
        panel.background = element_rect(fill = BCKg,
                                        colour = BCKg,
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = BCKg), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = BCKg),
        plot.margin = margin(15, 25, 15, 25),
        legend.position = c(0.1,0.75))
ggsave("LemurSexualDimorphismPhylo.png", width = 40.09091, height = 21, units = "cm", device = png )
