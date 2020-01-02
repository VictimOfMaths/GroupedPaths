rm(list=ls())

library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(curl)
library(readxl)
library(cowplot)
library(paletteer)

#Read in Alcohol-Specific Deaths data
Engdata <- fread("Data/ASDEng.csv")
Waldata <- fread("Data/ASDWal.csv")
Scodata <- fread("Data/ASDSco.csv")
NIdata <- fread("Data/ASDNI.csv")

data <- rbind(Engdata, Waldata, Scodata, NIdata)

#Read in population data
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2001tomid2018detailedtimeseries/ukpopulationestimates18382018.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
EngPop <- read_excel(temp, sheet="Table 11", range="A7:S98")
WalPop <- read_excel(temp, sheet="Table 13", range="A7:S98")
ScoPop <- read_excel(temp, sheet="Table 16", range="A7:S98")
NIPop <- read_excel(temp, sheet="Table 19", range="A7:S98")

#Tidy up
colnames(EngPop) <- c("age", 2018:2001)
colnames(WalPop) <- c("age", 2018:2001)
colnames(ScoPop) <- c("age", 2018:2001)
colnames(NIPop) <- c("age", 2018:2001)

EngPop <- gather(EngPop, Year, Pop, c(2:19))
WalPop <- gather(WalPop, Year, Pop, c(2:19))
ScoPop <- gather(ScoPop, Year, Pop, c(2:19))
NIPop <- gather(NIPop, Year, Pop, c(2:19))

EngPop$Country <- "England"
WalPop$Country <- "Wales"
ScoPop$Country <- "Scotland"
NIPop$Country <- "Northern Ireland"

Popdata <- rbind(EngPop, WalPop, ScoPop, NIPop)

#Convert from wide to long
data_long <- gather(data, age, deaths, c(4:23))

#Filter out older data
data_long <- subset(data_long, Year>=2008)

#compress age groups
data_long$Age <- case_when(
  data_long$age %in% c("<1", "01-04", "05-09", "10-14") ~ "u14",
  data_long$age %in% c("15-19", "20-24") ~ "15-24",
  data_long$age %in% c("25-29", "30-34") ~ "25-34",
  data_long$age %in% c("35-39", "40-44") ~ "35-44",
  data_long$age %in% c("45-49", "50-54") ~ "45-54",
  data_long$age %in% c("55-59", "60-64") ~ "55-64",
  data_long$age %in% c("65-69", "70-74", "75-79", "80-84", "85-89", "90+") ~ "65+",
  TRUE ~ "ERROR"
)

data_long <- data_long %>%
  group_by(Country, Year, Age, Cause) %>%
  summarise(Deaths=sum(as.numeric(deaths)))

#Read in 2018 NI data (not separated by age & cause) 
#from https://www.nisra.gov.uk/publications/alcohol-specific-deaths-2008-2018
NI2018data <- fread("Data/ASDNI2018.csv")
data_long <- rbind(data.frame(data_long), data.frame(NI2018data))

#Compress population count age groups (this is definitely a stupid way to do this)
Popdata$Age <- case_when(
  Popdata$age %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14") ~ "u14",
  Popdata$age %in% c("15", "16", "17", "18", "19", "20", "21", "22", "23", "24") ~ "15-24",
  Popdata$age %in% c("25", "26", "27", "28", "29", "30", "31", "32", "33", "34") ~ "25-34",
  Popdata$age %in% c("35", "36", "37", "38", "39", "40", "41", "42", "43", "44") ~ "35-44",
  Popdata$age %in% c("45", "46", "47", "48", "49", "50", "51", "52", "53", "54") ~ "45-54",
  Popdata$age %in% c("55", "56", "57", "58", "59", "60", "61", "62", "63", "64") ~ "55-64",
  TRUE ~ "65+"
)

Popdata <-  Popdata %>%
  group_by(Country, Year, Age) %>%
  summarise(Pop=sum(Pop))

#merge in Population data and calculate rates
data_long <- merge(data_long, Popdata, by=c("Country", "Year", "Age"))
data_long$mortrate <- data_long$Deaths*100000/data_long$Pop

#Set the order of the age groups
data_long$Age <- factor(data_long$Age, levels=c("u14", "15-24", "25-34", "35-44", "45-54",
                                              "55-64", "65+"))

#compress cause groups
data_long_all <- data_long %>%
  group_by(Country, Year, Age) %>%
  summarise(mortrate=sum(mortrate))

#Set country-specific colours
Engcol <- "#FB475E"
Walcol <- "#019992"
Scocol <- "#44EE77"
NIcol <- "#FFB001"

#draw plots using the function
Engplot <- groupedpath(data=subset(data_long_all, Country=="England"), group="Age", time="Year", outcome="mortrate", fill=Engcol, xlabel="Age", ylabel="Annual alcohol-specific deaths per 100,000")
Walplot <- groupedpath(data=subset(data_long_all, Country=="Wales"), group="Age", time="Year", outcome="mortrate", fill=Walcol, xlabel="Age", ylabel="Annual alcohol-specific deaths per 100,000")
Scoplot <- groupedpath(data=subset(data_long_all, Country=="Scotland"), group="Age", time="Year", outcome="mortrate", fill=Scocol, xlabel="Age", ylabel="Annual alcohol-specific deaths per 100,000")
NIplot <- groupedpath(data=subset(data_long_all, Country=="Northern Ireland"), group="Age", time="Year", outcome="mortrate", fill=NIcol, xlabel="Age", ylabel="Annual alcohol-specific deaths per 100,000")

tiff("Outputs/ASDEng.tiff", units="in", width=9, height=6.6, res=300)
ggdraw(Engplot)
dev.off()

tiff("Outputs/ASDWal.tiff", units="in", width=9, height=6.6, res=300)
ggdraw(Walplot)
dev.off()

tiff("Outputs/ASDSco.tiff", units="in", width=9, height=6.6, res=300)
ggdraw(Scoplot)
dev.off()

tiff("Outputs/ASDNI.tiff", units="in", width=9, height=6.6, res=300)
ggdraw(NIplot)
dev.off()

#######################################
#Put all 4 countries on one graph

data_long_all <- data_long_all[order(data_long_all$Country, data_long_all$Age, data_long_all$Year),]
data_long_all$index <- c(1:77, 1:77, 1:77, 1:77)

MergedCountries <- ggplot()+
  geom_path(data=subset(data_long_all, Country=="England"),aes(x=index, y=mortrate, group=Age), 
            arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")), colour=Engcol)+
  geom_path(data=subset(data_long_all, Country=="Wales"),aes(x=index, y=mortrate, group=Age), 
            arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")), colour=Walcol)+
  geom_path(data=subset(data_long_all, Country=="Scotland"),aes(x=index, y=mortrate, group=Age), 
            arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")), colour=Scocol)+
  geom_path(data=subset(data_long_all, Country=="Northern Ireland"),aes(x=index, y=mortrate, group=Age), 
            arrow=arrow(angle=25, type="closed", length=unit(0.2, "cm")), colour=NIcol)+
  theme_classic()+
  #Relabel the x=axis
  scale_x_continuous(breaks=c(6,17,28,39,50,61,72), labels=c("0-14", "15-24",
                                                             "25-34", "35-44", "45-54",
                                                             "55-64", "65+"),name="Age")+
  scale_y_continuous(name="Annual alcohol-specific deaths per 100,000", limits=c(0,70))+
  labs(title="Alcohol-specific deaths by UK country 2008-2018",
       caption="Data from Office for National Statistics & NISRA | Plot by @VictimOfMaths")

inset <- ggplot()+
  geom_path(aes(x=c(0,1), y=c(1,1)), colour=Engcol, arrow=arrow(angle=25, type="closed", 
                                                                   length=unit(0.2, "cm")))+
  geom_path(aes(x=c(0,1), y=c(1.5,1.5)), colour=Walcol, arrow=arrow(angle=25, type="closed", 
                                                                   length=unit(0.2, "cm")))+
  geom_path(aes(x=c(0,1), y=c(2,2)), colour=NIcol, arrow=arrow(angle=25, type="closed", 
                                                                   length=unit(0.2, "cm")))+
  geom_path(aes(x=c(0,1), y=c(2.5,2.5)), colour=Scocol, arrow=arrow(angle=25, type="closed", 
                                                                   length=unit(0.2, "cm")))+
  theme_void()+
  theme(legend.position="none")+
  annotate("text", x=0.5, y=1.2, label="England", size=3)+
  annotate("text", x=0.5, y=1.7, label="Wales", size=3)+
  annotate("text", x=0.5, y=2.2, label="Northern Ireland", size=3)+
  annotate("text", x=0.5, y=2.7, label="Scotland", size=3)

ASD4Countries <- ggdraw()+
  draw_plot(MergedCountries)+
  draw_plot(inset, x=0.15, y=0.5, width=0.1, height=0.15)+
  geom_rect(aes(xmin=0.14, xmax=0.26, ymin=0.48, ymax=0.67), colour="Black", alpha=0.01)

  
tiff("Outputs/ASD4countries.tiff", units="in", width=9, height=6.6, res=300)
ggdraw(ASD4Countries)
dev.off()