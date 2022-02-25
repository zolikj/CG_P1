library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package


c677t=read.csv("A_c677T.csv")
names(c677t)

zc677t=c677t
zc677t=zc677t[-c(76:83), ] #to get rid of AD stuff I deleted the last few rows
zc677t[zc677t==0] <- NA

nineBC=subset(zc677t, BC==9000)
EightBC=subset(zc677t, BC==8000)
sevenBC=subset(zc677t, BC==7000)
sixBC=subset(zc677t, BC==6000)
fiveBC=subset(zc677t, BC==5000)
fourBC=subset(zc677t, BC==4000)
threeBC=subset(zc677t, BC==3000)
twoBC=subset(zc677t, BC==2000)
oneBC=subset(zc677t, BC==1000)
#first_BC=rbind(nineBC,EightBC,sevenBC)





#tm_shape(world) +
 # tm_polygons() +
  #tm_shape(first_BC) +
  #tm_symbols(col = "black", border.col = "white", size = "q_freq") +
  #tm_facets(by = "BC", nrow = 2, free.coords = FALSE)





#
library(rgeos)
library(ggspatial)

ggplot(data = world) +
  labs( x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(0, 80), ylim = c(-10, 150), expand = FALSE) +
  
  ##
  library(rgeos)
library(ggspatial)


#annotation_scale(location = "bl", width_hint = 0.5) +
#annotation_north_arrow(location = "bl", which_north = "true", 
#pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
#style = north_arrow_fancy_orienteering) +

    
## Best
library("ggplot2")
library("dplyr")
library("plotly")
library("viridis")
library("rgeos")
library("ggspatial")
library(sf)
library(raster)
library(dplyr)
#library(spData)
#library(spDataLarge)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package

  ##
    #scale_fill_gradient(low="blue", high="red", name="Frequency of T Allele") +
#scale_color_gradientn(colours = rainbow(5),breaks=c(0,0.5,1),labels=c(0,0.5,1),limits=c(0,1), name=" ") +
#na.value = "grey50"
c677t=read.csv("A_c677T.csv")
names(c677t)

zc677t=c677t
zc677t=zc677t[-c(76:83), ] #to get rid of AD stuff I deleted the last few rows
zc677t[zc677t==0] <- NA

nineBC=subset(zc677t, BC==9000)
EightBC=subset(zc677t, BC==8000)
sevenBC=subset(zc677t, BC==7000)
sixBC=subset(zc677t, BC==6000)
fiveBC=subset(zc677t, BC==5000)
fourBC=subset(zc677t, BC==4000)
threeBC=subset(zc677t, BC==3000)
twoBC=subset(zc677t, BC==2000)
oneBC=subset(zc677t, BC==1000)
#install.packages("wesanderson")
library("wesanderson")


#to change the size of the map
lon=c(-10,60)

ggplot(data = world) +
  geom_sf(fill= "gray90",alpha=0.3) +
  labs( x = "Longitude", y = "Latitude") +
  coord_sf(xlim = lon, ylim = c(30, 70), expand = TRUE) +
  geom_point(data=zc677t, 
             aes(x=Lat, y=Long, color=q,size=Individual.No.), alpha=.85) +
  scale_size_continuous(breaks = c(1,5,10,20,30), limit = c(1,35), name="No.Individuals") + 
  scale_color_gradientn(colours = wes_palette("Zissou1"), na.value = "black", 
                        breaks=c(0.001,0.5,1),labels=c(0.001,0.5,1), 
                        limits=c(0,1), name="Frequency") +
  ggtitle("T-Allele Frequency 9000BC-0AD")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size = unit(0.7, "lines"),
        legend.title.align =.5, 
        legend.title = element_text(size=6),
        legend.text = element_text(size=6), 
        legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))
#9000-8000BC (good!!)
ggplot(data = world) +
  geom_sf(fill= "gray90",alpha=0.3) +
  labs( x = "Longitude", y = "Latitude") +
  coord_sf(xlim = lon, ylim = c(30, 70), expand = TRUE) +
  geom_point(data=nineBC, 
             aes(x=Lat, y=Long, color=q,size=Individual.No.), alpha=.85) +
  scale_size_continuous(breaks = c(1,5,10,20,30), limit = c(1,35), name="No.Individuals") + #changes size of dot to be consistent with all data (min 1 indv. Maxis 35)
  scale_color_gradientn(colours = wes_palette("Zissou1"), na.value = "black", #Changes the gradeinet colors and makes those without T-allel freq to be black
                        breaks=c(0.001,0.5,1),labels=c(0.001,0.5,1), #Makes it so there are three distinct breaks in the data
                        limits=c(0,1), name="Frequency") + #the imit makes all of the data consistent and mave a min slightly above zero and a max of 1
  ggtitle("T-Allele Frequency 9000-8000BC")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size = unit(0.7, "lines"),
        legend.title.align =.5, 
        legend.title = element_text(size=6),
        legend.text = element_text(size=6), 
        legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))
##8000-7000
ggplot(data = world) +
  geom_sf(fill= "gray90",alpha=0.3) +
  labs( x = "Longitude", y = "Latitude") +
  coord_sf(xlim = lon, ylim = c(30, 70), expand = TRUE) +
  geom_point(data=EightBC, 
             aes(x=Lat, y=Long, color=q,size=Individual.No.), alpha=.85) +
  scale_size_continuous(breaks = c(1,5,10,20,30), limit = c(1,35), name="No.Individuals") + 
  scale_color_gradientn(colours = wes_palette("Zissou1"), na.value = "black", 
                        breaks=c(0.001,0.5,1),labels=c(0.001,0.5,1), 
                        limits=c(0,1), name="Frequency") +
  ggtitle("T-Allele Frequency 8000-7000BC")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size = unit(0.7, "lines"),
        legend.title.align =.5, 
        legend.title = element_text(size=6),
        legend.text = element_text(size=6), 
        legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))
##7000-6000BC
ggplot(data = world) +
  geom_sf(fill= "gray90",alpha=0.3) +
  labs( x = "Longitude", y = "Latitude") +
  coord_sf(xlim = lon, ylim = c(30, 70), expand = TRUE) +
  geom_point(data=sevenBC, 
             aes(x=Lat, y=Long, color=q,size=Individual.No.), alpha=.85) +
  scale_size_continuous(breaks = c(1,5,10,20,30), limit = c(1,35), name="No.Individuals") + 
  scale_color_gradientn(colours = wes_palette("Zissou1"), na.value = "black", 
                        breaks=c(0.001,0.5,1),labels=c(0.001,0.5,1), 
                        limits=c(0,1), name="Frequency") +
  ggtitle("T-Allele Frequency 7000-6000BC")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size = unit(0.7, "lines"),
        legend.title.align =.5, 
        legend.title = element_text(size=6),
        legend.text = element_text(size=6), 
        legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))

##6000-5000BC
ggplot(data = world) +
  geom_sf(fill= "gray90",alpha=0.3) +
  labs( x = "Longitude", y = "Latitude") +
  coord_sf(xlim = lon, ylim = c(30, 70), expand = TRUE) +
  geom_point(data=sixBC, 
             aes(x=Lat, y=Long, color=q,size=Individual.No.), alpha=.85) +
  scale_size_continuous(breaks = c(1,5,10,20,30), limit = c(1,35), name="No.Individuals") + 
  scale_color_gradientn(colours = wes_palette("Zissou1"), na.value = "black", 
                        breaks=c(0.001,0.5,1),labels=c(0.001,0.5,1), 
                        limits=c(0,1), name="Frequency") +
  ggtitle("T-Allele Frequency 6000-5000BC")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size = unit(0.7, "lines"),
        legend.title.align =.5, 
        legend.title = element_text(size=6),
        legend.text = element_text(size=6), 
        legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))
#5000-4000BC
ggplot(data = world) +
geom_sf(fill= "gray90",alpha=0.3) +
  labs( x = "Longitude", y = "Latitude") +
  coord_sf(xlim = lon, ylim = c(30, 70), expand = TRUE) +
  geom_point(data=fiveBC, 
             aes(x=Lat, y=Long, color=q,size=Individual.No.), alpha=.85) +
  scale_size_continuous(breaks = c(1,5,10,20,30), limit = c(1,35), name="No.Individuals") + 
  scale_color_gradientn(colours = wes_palette("Zissou1"), na.value = "black", 
                        breaks=c(0.001,0.5,1),labels=c(0.001,0.5,1), 
                        limits=c(0,1), name="Frequency") +
  ggtitle("T-Allele Frequency 5000-4000BC")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size = unit(0.7, "lines"),
        legend.title.align =.5, 
        legend.title = element_text(size=6),
        legend.text = element_text(size=6), 
        legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))
#4000-3000
ggplot(data = world) +
  geom_sf(fill= "gray90",alpha=0.3) +
  labs( x = "Longitude", y = "Latitude") +
  coord_sf(xlim = lon, ylim = c(30, 70), expand = TRUE) +
  geom_point(data=fourBC, 
             aes(x=Lat, y=Long, color=q,size=Individual.No.), alpha=.85) +
  scale_size_continuous(breaks = c(1,5,10,20,30), limit = c(1,35), name="No.Individuals") + 
  scale_color_gradientn(colours = wes_palette("Zissou1"), na.value = "black", 
                        breaks=c(0.001,0.5,1),labels=c(0.001,0.5,1), 
                        limits=c(0,1), name="Frequency") +
  ggtitle("T-Allele Frequency 4000-3000BC")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size = unit(0.7, "lines"),
        legend.title.align =.5, 
        legend.title = element_text(size=6),
        legend.text = element_text(size=6), 
        legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))

##3000-2000BC
ggplot(data = world) +
  geom_sf(fill= "gray90",alpha=0.3) +
  labs( x = "Longitude", y = "Latitude") +
  coord_sf(xlim = lon, ylim = c(30, 70), expand = TRUE) +
  geom_point(data=threeBC, 
             aes(x=Lat, y=Long, color=q,size=Individual.No.), alpha=.85) +
  scale_size_continuous(breaks = c(1,5,10,20,30), limit = c(1,35), name="No.Individuals") + 
  scale_color_gradientn(colours = wes_palette("Zissou1"), na.value = "black", 
                        breaks=c(0.001,0.5,1),labels=c(0.001,0.5,1), 
                        limits=c(0,1), name="Frequency") +
  ggtitle("T-Allele Frequency 3000-2000BC")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size = unit(0.7, "lines"),
        legend.title.align =.5, 
        legend.title = element_text(size=6),
        legend.text = element_text(size=6), 
        legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))

##2000-1000
ggplot(data = world) +
  geom_sf(fill= "gray90",alpha=0.3) +
  labs( x = "Longitude", y = "Latitude") +
  coord_sf(xlim = lon, ylim = c(30, 70), expand = TRUE) +
  geom_point(data=twoBC, 
             aes(x=Lat, y=Long, color=q,size=Individual.No.), alpha=.85) +
  scale_size_continuous(breaks = c(1,5,10,20,30), limit = c(1,35), name="No.Individuals") + 
  scale_color_gradientn(colours = wes_palette("Zissou1"), na.value = "black", 
                        breaks=c(0.001,0.5,1),labels=c(0.001,0.5,1), 
                        limits=c(0,1), name="Frequency") +
  ggtitle("T-Allele Frequency 2000-1000BC")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size = unit(0.7, "lines"),
        legend.title.align =.5, 
        legend.title = element_text(size=6),
        legend.text = element_text(size=6), 
        legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))

##1000-0
ggplot(data = world) +
  geom_sf(fill= "gray90",alpha=0.3) +
  labs( x = "Longitude", y = "Latitude") +
  coord_sf(xlim = lon, ylim = c(30, 70), expand = TRUE) +
  geom_point(data=oneBC, 
             aes(x=Lat, y=Long, color=q,size=Individual.No.), alpha=.85) +
  scale_size_continuous(breaks = c(1,5,10,20,30), limit = c(1,35), name="No.Individuals") + 
  scale_color_gradientn(colours = wes_palette("Zissou1"), na.value = "black", 
                        breaks=c(0.001,0.5,1),labels=c(0.001,0.5,1), 
                        limits=c(0,1), name="Frequency") +
  ggtitle("T-Allele Frequency 1000BC-0AD")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size = unit(0.7, "lines"),
        legend.title.align =.5, 
        legend.title = element_text(size=6),
        legend.text = element_text(size=6), 
        legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))

##plot alle vs time 
tc677t=read.csv("time_allele.csv")
ggplot(tc677t, aes(x=Year.BC, y=q)) + 
  geom_bar(stat="identity",position = 'dodge')+
  ggtitle("T-Allele Frequency Over Time")+
  labs( x = "Time in BC", y = "T-Allele Frequency")+
            scale_x_reverse()
 #time to individuals
ggplot(tc677t, aes(x=Year.BC, y=Individual.No.)) + 
  geom_bar(stat="identity", position = 'dodge')+
  ggtitle("Number of Individuals Over Time")+
  labs( x = "Time in BC", y = "Number fo Individuals")+
  scale_x_reverse()

# hetero vs homo
ftc677t=read.csv("freq_time.csv")

library(ggplot2)
library(tidyr)

ggplot(ftc677t, aes(x=Year.BC,y=Count,fill=allele_freq))+
  geom_bar(stat = "identity", position = 'dodge')+
  ggtitle("Genotype Frequency Over Time")+
  labs( x = "Time in BC", y = "Genotype Frequency")+
  scale_x_reverse()

##Freq vs location over time

  #Western Europe (-4 to 13.9 longitude)

WEc677t=read.csv("freq_time_WE.csv")
ggplot(WEc677t, aes(x=Year.BC,y=Count,fill=allele_freq))+
  geom_bar(stat = "identity", position = 'dodge')+
  ggtitle("Genotype Frequency Over Time in Western Europe")+
  labs( x = "Time in BC", y = "Genotype Frequency")+
  scale_x_reverse()

  #Centrl Europe (14- 30 longitude)
CEc677t=read.csv("freq_time_CE.csv")
ggplot(CEc677t, aes(x=Year.BC,y=Count,fill=allele_freq))+
  geom_bar(stat = "identity", position = 'dodge')+
  ggtitle("Genotype Frequency Over Time in Central Europe")+
  labs( x = "Time in BC", y = "Genotype Frequency")+
  scale_x_reverse()

#Eestern Europe (30-60 longitude)
EEc677t=read.csv("freq_time_EE.csv")
ggplot(EEc677t, aes(x=Year.BC,y=Count,fill=allele_freq))+
  geom_bar(stat = "identity", position = 'dodge')+
  ggtitle("Genotype Frequency Over Time in Eastern Europe")+
  labs( x = "Time in BC", y = "Genotype Frequency")+
  scale_x_reverse()
