#figures for the paper martin et al 

#Contact cristina.madrid@uab.cat

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# First you have to do install.packages (¨name of package)####
library(dplyr)
library (ggplot2)
library (reshape2)
library(pals)
library(ggsankey)

#Figure 4####
units<-read.csv("data/Units.csv", sep = ";")

data4<-read.csv("data/Figure4.csv", sep = ";")%>%
  mutate(Year = as.factor(Year))%>%
  mutate(Level = as.factor(Level))%>%
  mutate(Processor = as.factor(Processor))%>%
  mutate(GHG = GHG/1000)%>%
  melt(id.vars = c("Level", "Processor", "Year"))%>%
  merge(units, by= "variable" )
data4<-data4[,c(2,3,4,1,6,5)]
data4$Processor <- factor(data4$Processor, 
                          levels = c("Fuel",
                                     "Heat",
                                     "Electricity",
                                     
                                     "Renewable", 
                                     "Non-renewable", 
                              
                                     "Coal",
                                     "Natural Gas", 
                                     "Hydro", 
                                     "Diesel",
                                     "Kerosene",
                                     "Waste",
                                     "Nuclear", 
                                     "Bioenergy",
                                     "Wind", 
                                     "Solar"))


#barchart n-1####

pal <- c("#E31A1C","#FF7F00",  "dodgerblue1","green3",  "#6A3D9A",
         "black", "darkorange4", "skyblue2", "orchid1", "#FDBF6F", 
         "gray70", "maroon", "palegreen2", "darkturquoise", "gold1")

# New facet label names for variable variable
variable.labs <- c("Energy \n(EJ)", 
                   "GHG emissions \n(Pg CO2 eq)", 
                   "Land Occupation \n(thousand Km2)",
                   "Water Depletion \n(Km3)",
                   "Supply Risk \n(years)",
                   "Human Activity \n(Gh)")
names(variable.labs) <- c("Energy", "GHG", "LO", "WD", "SR", "HA")

Level.labs <- c("n-1 (Functional)","n-2 (Functional)", "n-3 (Structural)")
names(Level.labs) <- c("n-1", "n-2", "n-3")

p<-data4%>%
  ggplot (aes(x=Year, y=value, fill = Processor))+
#here you decide the chart type
  geom_bar (stat = "sum", show.legend=c(size=FALSE))+
#  scale_fill_brewer()+
  scale_fill_manual(values = pal)+
#                    labels = c("Illinois", "Indiana",
#                              "Michigan","Ohio", "Wisconsin"))+ 
#these are the format parts
  theme_light()+
  theme(legend.position = "bottom", 
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        legend.title=element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = 'black'))+
#this is the mosaic part
  facet_grid(vars(variable), vars(Level), scales = "free",
             labeller = labeller(variable = variable.labs,
                                 Level = Level.labs))
p

ggsave(filename="Fig4.png", plot=p, device="png",
       path="output/", height=10, width=8, units="in", dpi=500)


#Figure 5####
data5<-read.csv("data/Figure4.csv", sep = ";")%>%
  mutate(Year = as.factor(Year))%>%
  mutate(Level = as.factor(Level))%>%
  mutate(Processor = as.factor(Processor))%>%
  mutate(EMR = Energy*1000000000000/(HA*1000000000))%>%
  mutate(GHGMR = GHG*1000000000/(HA*1000000000))%>%
  mutate(WMR = WD*1000000000/(HA*1000000000))%>%
  mutate(GHGDens = GHG*1000000000/LO*1000000000) #Kg CO2equ/m2

data7<-data5%>%
  melt(id.vars = c("Level", "Processor", "Year", "Energy", "EMR"))
data7$Processor <- factor(data7$Processor, 
                          levels = c("Fuel",
                                     "Heat",
                                     "Electricity",
                                     
                                     "Renewable", 
                                     "Non-renewable", 
                                     
                                     "Coal",
                                     "Natural Gas", 
                                     "Hydro", 
                                     "Diesel",
                                     "Kerosene",
                                     "Waste",
                                     "Nuclear", 
                                     "Bioenergy",
                                     "Wind", 
                                     "Solar"))
data7$variable <- factor(data6$variable, 
                         levels = c("Energy",
                                    "EMR",
                                    "WD",
                                    
                                    "WMR", 
                                    "GHG", 
                                    
                                    "GHGMR",
                                    "GHGDens",
                                    "LO", 
                                    "SR", 
                                    "EM",
                                    "HA"))


p2<-data7[data7$Level == "n-3" & !data7$variable %in% c("EM", "HA"),] %>%
#  subset(variable %in% c("WMR", "GHGMR"))%>%
  mutate(EMR = as.numeric(EMR))%>%
  ggplot (aes(x=EMR, y=value, 
              color = Processor, size = 5))+
  geom_point(aes(shape=Level), show.legend=c(size=FALSE))+
  scale_x_continuous(trans = 'log10') +
  scale_y_continuous(trans = 'log10') +
  scale_color_manual(values = pal) +
#  scale_shape_manual(values = c(15, 17, 19)) +
  theme_light() +
  theme(legend.position = "bottom", 
       axis.title.y=element_blank(),
#       axis.title.x=element_blank(),
        legend.title=element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = 'black'))+
  facet_grid(vars(variable),vars(Year),  scales = "free")
  
p2  
ggsave(filename="Fig5.png", plot=p2, device="png",
       path="output/", height=10, width=8, units="in", dpi=500)


#graphical abstract####
library(GGally)
library(ggalluvial)

data6<-data5%>%
  melt(id.vars = c("Level", "Processor", "Year"))
data6$Processor <- factor(data6$Processor, 
                          levels = c("Fuel",
                                     "Heat",
                                     "Electricity",
                                     
                                     "Renewable", 
                                     "Non-renewable", 
                                     
                                     "Coal",
                                     "Natural Gas", 
                                     "Hydro", 
                                     "Diesel",
                                     "Kerosene",
                                     "Waste",
                                     "Nuclear", 
                                     "Bioenergy",
                                     "Wind", 
                                     "Solar"))
data6$variable <- factor(data6$variable, 
                          levels = c("Energy",
                                     "EMR",
                                     "WD",
                                     
                                     "WMR", 
                                     "GHG", 
                                     
                                     "GHGMR",
                                     "LO", 
                                     "SR", 
                                     "EM",
                                     "HA"))


variable.labs <- c("Origin of energy production", 
                   "Energy metabolic rate per hour of activity",
                   "Water Depletion",
                   "Water Metabolic Rate",
                   "GHG emissions",
                   "GHG Metabolic Rate",
                   "Land Occupation",
                   "Supply Risk",
                   "Employment",
                   "Human Activity")
names(variable.labs) <- c("Energy",
                           "EMR",
                           "WD",
                           
                           "WMR", 
                           "GHG", 
                           
                           "GHGMR",
                           "LO", 
                           "SR", 
                           "EM",
                           "HA")

data6[is.na(data6)]<-0
p3<-ggplot(data = data6[data6$Level == "n-2" & data6$variable %in% c("Energy", "EMR", "LO", "SR"),],
       aes(axis1 = Processor, axis2 = Year, 
           y = value, fill = value, label = value)) +
  scale_fill_manual(values = pal) +
  scale_x_discrete(limits = c("Function", "Year"), expand = c(.2, .05)) +
  scale_y_continuous(label = scales::percent_format(scale = 100 / value)) +
  geom_alluvium(aes(fill = Processor)) +
  geom_stratum(aes(fill = Processor)) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank())+
  labs(title="\n    How sustainable is our renewable energy transition?\n")+
  facet_wrap(vars(variable), nrow = 2, scales = "free",
             labeller = labeller(variable = variable.labs))

p3

ggsave(filename="GraphAbstract.png", plot=p3, device="png",
       path="output/", height=7, width=7, units="in", dpi=500)




p4<-ggplot(data = data6[data6$Level == "n-1" & !data6$variable %in% c("EM") ,],
           aes(axis1 = Processor, axis2 = Year, y = value)) +
  scale_x_discrete(limits = c("Function", "Year"), expand = c(.2, .05)) +
  geom_alluvium(aes(fill = Processor)) +
  geom_stratum(aes(fill = Processor)) +
  scale_fill_manual(values = pal) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank())+
  facet_wrap(vars(variable), nrow = 5, scales = "free",
             labeller = labeller(variable = variable.labs))



ggsave(filename="Characterization.png", plot=p4, device="png",
       path="output/", height=10, width=8, units="in", dpi=500)



