rm(list=ls())

source("GroupedPath.R")

library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(paletteer)
library(curl)
library(readxl)

#Read in DRD data
#data comes from:
#England/Wales https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsrelatedtodrugpoisoningenglandandwalesreferencetable
#Scotland https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/deaths/drug-related-deaths-in-scotland/2018
#Northern Ireland https://www.nisra.gov.uk/publications/drug-related-and-drug-misuse-deaths-2007-2017

data <- fread("Data/DRDUK.csv")
data_long <- gather(data, age, deaths, c(3:15))

#Read in population data
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2001tomid2018detailedtimeseries/ukpopulationestimates18382018.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
EngPop <- read_excel(temp, sheet="Table 11", range="A7:AA98")
WalPop <- read_excel(temp, sheet="Table 13", range="A7:AA98")
ScoPop <- read_excel(temp, sheet="Table 16", range="A7:X98")
NIPop <- read_excel(temp, sheet="Table 19", range="A7:M98")

#Tidy up
colnames(EngPop) <- c("age", 2018:1993)
colnames(WalPop) <- c("age", 2018:1993)
colnames(ScoPop) <- c("age", 2018:1996)
colnames(NIPop) <- c("age", 2018:2007)

EngPop <- gather(EngPop, Year, Pop, c(2:27))
WalPop <- gather(WalPop, Year, Pop, c(2:27))
ScoPop <- gather(ScoPop, Year, Pop, c(2:20))
NIPop <- gather(NIPop, Year, Pop, c(2:13))

EngPop$Country <- "England"
WalPop$Country <- "Wales"
ScoPop$Country <- "Scotland"
NIPop$Country <- "Northern Ireland"

Popdata <- rbind(EngPop, WalPop, ScoPop, NIPop)

#Compress population age groups into groups to match deaths data (this throws some errors but works)
#For England & Wales
Popdata$Ageband1 <- case_when(
  as.numeric(Popdata$age)<20 ~ "U20",
  as.numeric(Popdata$age)<30 ~ "20-29",
  as.numeric(Popdata$age)<40 ~ "30-39",
  as.numeric(Popdata$age)<50 ~ "40-49",
  as.numeric(Popdata$age)<70 ~ "50-69",
  TRUE ~ "70+"
  )

#For Scotland & NI
Popdata$Ageband2 <- case_when(
  as.numeric(Popdata$age)<15 ~ "U14",
  as.numeric(Popdata$age)<25 ~ "15-24",
  as.numeric(Popdata$age)<35 ~ "25-34",
  as.numeric(Popdata$age)<45 ~ "35-44",
  as.numeric(Popdata$age)<55 ~ "45-54",
  as.numeric(Popdata$age)<65 ~ "55-64",
  TRUE ~ "65+"
)



Popdata1 <- Popdata %>%
  filter(Country %in% c("England", "Wales")) %>%
  group_by(Ageband1, Year, Country) %>%
  summarise(Pop1=sum(Pop))

Popdata2 <- Popdata %>%
  filter(Country %in% c("Scotland", "Northern Ireland")) %>%
  group_by(Ageband2, Year, Country) %>%
  summarise(Pop2=sum(Pop))


#merge into deaths data
data_long <- merge(data_long, Popdata1, by.x=c("age", "Year", "Country"), 
                   by.y=c("Ageband1", "Year", "Country"), all.x=TRUE)

data_long <- merge(data_long, Popdata2, by.x=c("age", "Year", "Country"), 
                   by.y=c("Ageband2", "Year", "Country"), all.x=TRUE)
data_long$Pop <- ifelse(is.na(data_long$Pop1), data_long$Pop2, data_long$Pop1)
data_long <- data_long[,-c(5,6)]

#remove misaligned agebands
data_long <- data_long[complete.cases(data_long),]

#Set the order of the age groups
data_long$age <- factor(data_long$age, levels=c("U14", "15-24", "25-34", "35-44", 
                                                      "45-54", "55-64", "65+",
                                                "U20", "20-29", "30-39", 
                                                "40-49", "50-69", "70+"))

#calculate rates
data_long$mortrate <- data_long$deaths*100000/data_long$Pop

#order data
data_long <- data_long[order(data_long$Country, data_long$Year, data_long$age),]

#Set country-specific colours
Engcol <- "#FB475E"
Walcol <- "#019992"
Scocol <- "#44EE77"
NIcol <- "#FFB001"

#draw graphs by country
Engplot <- groupedpath(data=subset(data_long, Country=="England"), group="age", time="Year", 
                       outcome="mortrate", fill=Engcol, xlabel="Age", 
                       ylabel="Annual deaths from drug misuse per 100,000",
                       title="Deaths from drug misuse in England 1993-2018",
                       caption="Data from ONS | Plot by @VictimOfMaths")
Walplot <- groupedpath(data=subset(data_long, Country=="Wales"), group="age", time="Year", 
                       outcome="mortrate", fill=Walcol, xlabel="Age", 
                       ylabel="Annual deaths from drug misuse per 100,000",
                       title="Deaths from drug misuse in Wales 1993-2018",
                       caption="Data from ONS | Plot by @VictimOfMaths")
Scoplot <- groupedpath(data=subset(data_long, Country=="Scotland"), group="age", time="Year", 
                       outcome="mortrate", fill=Scocol, xlabel="Age", 
                       ylabel="Annual deaths from drug misuse per 100,000",
                       title="Deaths from drug misuse in Scotland 1996-2018",
                       caption="Data from NRS | Plot by @VictimOfMaths")
NIplot <- groupedpath(data=subset(data_long, Country=="Northern Ireland"), group="age",  
                      time="Year", outcome="mortrate", fill=NIcol, xlabel="Age", 
                      ylabel="Annual deaths from drug misuse per 100,000",
                      title="Deaths from drug misuse in Northern Ireland 2007-2017",
                      caption="Data from NISRA | Plot by @VictimOfMaths")

#save them
tiff("Outputs/DRDEng.tiff", units="in", width=9, height=6.6, res=300)
ggdraw(Engplot)
dev.off()

tiff("Outputs/DRDWal.tiff", units="in", width=9, height=6.6, res=300)
ggdraw(Walplot)
dev.off()

tiff("Outputs/DRDSco.tiff", units="in", width=9, height=6.6, res=300)
ggdraw(Scoplot)
dev.off()

tiff("Outputs/DRDNI.tiff", units="in", width=9, height=6.6, res=300)
ggdraw(NIplot)
dev.off()

#draw graphs comparing alcohol and drug deaths
#bring in ASD
ASDdata <- fread("Data/ScoNIASDdata.csv")

DRDdata <- subset(data_long, Country %in% c("Scotland", "Northern Ireland") &  Year>2007)

#harmonise age groups
DRDdata$Age <- ifelse(DRDdata$age=="U14", "u14", as.character(DRDdata$age))

#combined datasets
ASDDRD <- merge(ASDdata, DRDdata, by=c("Country", "Year", "Age"), all.x=TRUE)
ASDDRD <- ASDDRD[,c(1:3, 5, 10)]
colnames(ASDDRD) <- c("Country", "Year", "Age", "alcdeaths", "drugdeaths")

#generate index
ASDDRD$Age <- factor(ASDDRD$Age, levels=c("u14", "15-24", "25-34", "35-44", "45-54", 
                                          "55-64", "65+"))
ASDDRD <- ASDDRD[order(ASDDRD$Country, ASDDRD$Age, ASDDRD$Year)]
ASDDRD$index <- c(1:77, 1:77)

plot <- ggplot()+
  geom_path(data=subset(ASDDRD, Country=="Scotland"),aes(x=index, y=alcdeaths, group=Age), 
            arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")), colour="SkyBlue")+
  geom_path(data=subset(ASDDRD, Country=="Scotland"),aes(x=index, y=drugdeaths, group=Age), 
            arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")), colour="Red")+
  theme_classic()+
  #Relabel the x=axis
  scale_x_continuous(breaks=c(6,17,28,39,50,61,72), labels=c("0-14", "15-24",
                                                             "25-34", "35-44", "45-54",
                                                             "55-64", "65+"),name="Age")+
  scale_y_continuous(name="Annual deaths per 100,000", limits=c(0,70))+
  labs(title="Trends in alcohol and drug deaths by age in Scotland 2008-2018",
       caption="Data from Office for National Statistics & National Records of Scotland | Plot by @VictimOfMaths")

inset <- ggplot()+
  geom_path(aes(x=c(0,1), y=c(1,1)), colour="SkyBlue", arrow=arrow(angle=25, type="closed", 
                                                                length=unit(0.2, "cm")))+
  geom_path(aes(x=c(0,1), y=c(1.5,1.5)), colour="Red", arrow=arrow(angle=25, type="closed", 
                                                                    length=unit(0.2, "cm")))+
  
  theme_void()+
  theme(legend.position="none")+
  annotate("text", x=0.5, y=1.2, label="Alcohol", size=3)+
  annotate("text", x=0.5, y=1.7, label="Drugs", size=3)

ASDDRDScotland <- ggdraw()+
  draw_plot(plot)+
  draw_plot(inset, x=0.15, y=0.7, width=0.1, height=0.15)+
  geom_rect(aes(xmin=0.14, xmax=0.26, ymin=0.68, ymax=0.865), colour="Black", alpha=0.01)

tiff("Outputs/ASDDRDScot.tiff", units="in", width=9, height=6.6, res=300)
ggdraw(ASDDRDScotland)
dev.off()

#Generate version with polygons
Agebands <- unique(as.character(ASDDRD$Age))
Years <- unique(ASDDRD$Year)

for (i in 1:length(Agebands)) {
  name <- paste0("Alc",i)
  assign(name, 0)
  
  for (j in 1:length(Years)) {
    assign(name, rbind(eval(as.symbol(name)), ASDDRD$alcdeaths[ASDDRD$Age==Agebands[i] & 
                                                                 ASDDRD$Year==Years[j] &
                                                                 ASDDRD$Country=="Scotland"]))
    
  }
  assign(name, rbind(eval(as.symbol(name)), 0))
}

for (i in 1:length(Agebands)) {
  name <- paste0("Drg",i)
  assign(name, 0)
  
  for (j in 1:length(Years)) {
    assign(name, rbind(eval(as.symbol(name)), ASDDRD$drugdeaths[ASDDRD$Age==Agebands[i] & 
                                                                 ASDDRD$Year==Years[j] &
                                                                 ASDDRD$Country=="Scotland"]))
    
  }
  assign(name, rbind(eval(as.symbol(name)), 0))
}

polyplot <- ggplot()+
  geom_polygon(aes(x=c(1,1:11,11), y=Alc1), fill="SkyBlue", alpha=0.7)+
  geom_polygon(aes(x=c(12,12:22,22), y=Alc2), fill="SkyBlue", alpha=0.7)+
  geom_polygon(aes(x=c(23,23:33,33), y=Alc3), fill="SkyBlue", alpha=0.7)+
  geom_polygon(aes(x=c(34,34:44,44), y=Alc4), fill="SkyBlue", alpha=0.7)+
  geom_polygon(aes(x=c(45,45:55,55), y=Alc5), fill="SkyBlue", alpha=0.7)+
  geom_polygon(aes(x=c(56,56:66,66), y=Alc6), fill="SkyBlue", alpha=0.7)+
  geom_polygon(aes(x=c(67,67:77,77), y=Alc7), fill="SkyBlue", alpha=0.7)+
  geom_polygon(aes(x=c(1,1:11,11), y=Drg1), fill="Red", alpha=0.4)+
  geom_polygon(aes(x=c(12,12:22,22), y=Drg2), fill="Red", alpha=0.4)+
  geom_polygon(aes(x=c(23,23:33,33), y=Drg3), fill="Red", alpha=0.4)+
  geom_polygon(aes(x=c(34,34:44,44), y=Drg4), fill="Red", alpha=0.4)+
  geom_polygon(aes(x=c(45,45:55,55), y=Drg5), fill="Red", alpha=0.4)+
  geom_polygon(aes(x=c(56,56:66,66), y=Drg6), fill="Red", alpha=0.4)+
  geom_polygon(aes(x=c(67,67:77,77), y=Drg7), fill="Red", alpha=0.4)+
  geom_path(data=subset(ASDDRD, Country=="Scotland"),aes(x=index, y=alcdeaths, group=Age), 
            arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")), colour="Black")+
  geom_path(data=subset(ASDDRD, Country=="Scotland"),aes(x=index, y=drugdeaths, group=Age), 
            arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")), colour="Black")+
  theme_classic()+
  #Relabel the x=axis
  scale_x_continuous(breaks=c(6,17,28,39,50,61,72), labels=c("0-14", "15-24",
                                                             "25-34", "35-44", "45-54",
                                                             "55-64", "65+"),name="Age")+
  scale_y_continuous(name="Annual deaths per 100,000", limits=c(0,70))+
  labs(title="Trends in alcohol and drug deaths by age in Scotland 2008-2018",
       caption="Data from Office for National Statistics & National Records of Scotland | Plot by @VictimOfMaths")

insetvalues <- runif(length(Years), 0.5,1)
insetvalues2 <- c(0,insetvalues,0)

inset1 <- ggplot()+
  geom_polygon(aes(x=c(1,1:11,11), y=insetvalues2), fill="SkyBlue", alpha=0.7)+
  geom_line(aes(x=c(1:length(Years)), y=insetvalues), arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  theme_classic()+
  #Remove all clutter
  theme(axis.line=element_blank(), axis.text=element_blank(),axis.ticks=element_blank(),
        axis.title=element_blank())
  
insetvalues3 <- runif(length(Years), 0.5,1)
insetvalues4 <- c(0,insetvalues3,0)

inset2 <- ggplot()+
  geom_polygon(aes(x=c(1,1:11,11), y=insetvalues4), fill="Red", alpha=0.4)+
  geom_line(aes(x=c(1:length(Years)), y=insetvalues3), arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")))+
  theme_classic()+
  #Remove all clutter
  theme(axis.line=element_blank(), axis.text=element_blank(),axis.ticks=element_blank(),
        axis.title=element_blank())

ASDDRDpolyScot <- ggdraw()+
  draw_plot(polyplot)+
  draw_plot(inset1, x=0.07, y=0.75, width=0.1, height=0.15)+
  draw_plot(inset2, x=0.07, y=0.62, width=0.1, height=0.15)+
  draw_label(min(Years), x=0.085, y=0.62, size=10)+
  draw_label(max(Years), x=0.155, y=0.62, size=10)+
  draw_label("Cause", x=0.09, y=0.91, size=10)+
  draw_label("Alcohol", x=0.12, y=0.8, size=10)+
  draw_label("Drugs", x=0.12, y=0.67, size=10)
  
tiff("Outputs/ASDDRDpolyScot.tiff", units="in", width=9, height=6.6, res=300)
ggdraw(ASDDRDpolyScot)
dev.off()
