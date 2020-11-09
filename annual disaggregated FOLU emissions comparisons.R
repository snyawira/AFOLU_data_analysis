#########SELECT the tropical regions for Houghton and then for the other countries all then we use Houghton to build countries#######
######library(ggplot2)
library(data.table)
library(reshape2)
library(dplyr)
library(tidyr)
library(stringdist)
library(fuzzyjoin)
library(rworldmap)
library(classInt)
library(RColorBrewer)
library(tidyverse)
library(janitor)
library(ggplot2)
library(countrycode)
library(ncdf4)

############SOURCE FUNCTIONS IN A DIFFERENT FILE################
source("disaggregated_FOLU_timesclices.R")


df1   <- read.csv("Houghton_annual net flux_by land use_countries.csv", header = FALSE) ##Houghtons bookkeeping model
df2   <- read.csv("FOLU_FAOSTAT_emissions_1990-2017.csv", header = TRUE) ##FAOSTAT FOLU emissions and removal
df3   <- read.csv("Forestland_FAOSTAT_emissions_1990-2015.csv", header = TRUE) ##Forest emissions and removal

####################Deforestation emissions from ESMs######################################
mdl <- c('CESM2', 'CNRM-ESM2-1')
var1 <-  c('fDeforestToAtmos')
years <- c(1990:2014)
tslice <- c("199101-200012","200101-201012","201101-201412")
ts <- c('t1','t2','t3')


###########Read in the global deforestation emissions over the diffent period for the two models
for (ei in 1:length(mdl)){
  fname  = nc_open(paste0('fDeforestToAtmos_Emon_',mdl[ei],'_land-hist_199001-201412_global.nc'))
  fLuc = ncvar_get(fname, var1)   ##################Get the variable############################
  fLucn = na.omit(as.vector(fLuc))
  assign(paste("fLuc_", mdl[ei], sep = ""), fLucn)
}

####Time slices deforestation emissions for the CESM and CNRM models
for(ai in 1:length(tslice)){
  for (ei in 1:length(mdl)){
    fname  = nc_open(paste0('fDeforestToAtmos_Emon_',mdl[ei],'_land-hist_',tslice[ai],'_global.nc'))
    fLuc = ncvar_get(fname, var1)   ##################Get the variable############################
    fLucn = na.omit(as.vector(fLuc))
    assign(paste("fLuc_", mdl[ei],".",ts[ai], sep = ""), fLucn)
  }
}

#########Combine the data frames for the two models#####
df.esms.t1 <- data.frame(fLuc_CESM2.t1,`fLuc_CNRM-ESM2-1.t1`)
df.esms.t2 <- data.frame(fLuc_CESM2.t2,`fLuc_CNRM-ESM2-1.t2`)
df.esms.t3 <- data.frame(fLuc_CESM2.t3,`fLuc_CNRM-ESM2-1.t3`)
colnames(df.esms.t1) <- c("CESM","CNRM")
colnames(df.esms.t2) <- c("CESM","CNRM")
colnames(df.esms.t3) <- c("CESM","CNRM")

#########Calculate the mean and standard deviations for the two models
esms.mn.t1 <- apply(df.esms.t1[,1:2], 1, mean)
esms.mn.t2 <- apply(df.esms.t2[,1:2], 1, mean)
esms.mn.t3 <- apply(df.esms.t3[,1:2], 1, mean)
esms.sd.t1 <- apply(df.esms.t1[,1:2], 1, sd)
esms.sd.t2 <- apply(df.esms.t2[,1:2], 1, sd)
esms.sd.t3 <- apply(df.esms.t3[,1:2], 1, sd)

##########Transpose Houghton data#############################
varnames <- df1[,1] ####Get variable names
df1.tp <-  t(df1)
df1.tp <- as.data.frame(df1.tp)
colnames(df1.tp) <- varnames # we set variable names
df1.tp <- df1.tp[-1,]
df1.tp <- pivot_longer(data = df1.tp, cols = c(4:169),names_to = "Year",values_to = "Emissions")
country1.names<-unique(df1.tp$Country)

######Houghton data all FOLU emissions per process######################
df1.tp.n <- df1.tp %>%
  transform(Year=as.integer(Year),Emissions = as.numeric(Emissions)) %>%
  mutate(CO2 = Emissions * 44/12 * 0.001) %>% ###Convert carbon to CO2 and Teragrams into Gigatonnes###
  filter(Year >= 1990 & Year <= 2015) %>% 
  select(REGION, Country, Process, Year, CO2) 
######FAOSTAT data all FOLU emissions per LandUse##################
df2.tp.n <- df2 %>%
  rename(Country.Code = Area.Code, Country = Area) %>%
  mutate(Country = countrycode(Country, origin  = 'country.name', destination = 'iso.name.en'))%>%
  mutate(CO2 =  Value/1000000) %>%  ####Convert gigagrams into gigatonnes
  rename(Process=Item)%>%
  select(Country, Process, Year, CO2) 

########FAOSTAT forestland (Net forest conversion and other forestry)######
df3.tp.n <- df3 %>%
  rename(Country.Code = Area.Code, Country = Area) %>%
  mutate(Country = countrycode(Country, origin  = 'country.name', destination = 'iso.name.en'))%>%
  group_by(Year, Country) %>%
  mutate(CO2 =  Value/1000000) %>%  ####Convert gigagrams into gigatonnes
  rename(Process=Item)%>%
  select(Country, Process, Year, CO2) 

##########HOUGHTON BOOK KEEPING MODEL DATA#########################
#######Deforestation emissions Houghton############################################
df1.tp.def <- df1.tp.n %>%
  group_by(Year)%>%
  filter((Process == "FP" | Process == "FC"| Process == "FCO" ))  %>%
  summarise(CO2ann=sum(CO2, na.rm=TRUE))
#########Emissions/sinks from forestry####################
df1.tp.fry <- df1.tp.n %>%
  group_by(Year)%>%
  filter((Process == "FUEL"| Process == "IND")) %>%
  summarise(CO2ann=sum(CO2, na.rm=TRUE))
########Other land use emissions data######################
df1.tp.oLU <- df1.tp.n %>%
  group_by(Year)%>%
  filter((Process == "CP"| Process == "OC" | Process == "OP" | Process == "FIRE" |Process == "PLANT")) %>%
  summarise(CO2ann=sum(CO2, na.rm=TRUE))


####################FAOSTAT############################################
########Deforestation emissions FAOSTAT###################################
df3.tp.dep <- df3.tp.n %>%
  group_by(Year)%>%
  filter((Process == "Net Forest conversion")) %>%
  summarise(CO2ann=sum(CO2, na.rm=TRUE))

########Other forestry emsission/sinks##########################
df3.tp.fry <- df3.tp.n %>%
  group_by(Year)%>%
  filter((Process == "Forest land")) %>%
  summarise(CO2ann=sum(CO2, na.rm=TRUE))
##########Other land use emissions#############################
df2.tp.oLU <- df2.tp.n %>%
  group_by(Year)%>%
  filter((Process == "Burning Biomass" | Process == "Cropland" | Process == "Grassland")) %>%
  summarise(CO2ann=sum(CO2, na.rm=TRUE))


df3.tp.n %>%
  group_by(Year)%>%
  filter((Process == "Net Forest conversion" & CO2<0)) %>%
  summarise(CO2ann=sum(CO2, na.rm=TRUE))


###############CALLL THE FUNCTIONS TO CALCULATE DIFFERENT COMPONENTS OF FOLU emissions on different time slices##################
###########Houghton: deforestration, other land use and harvest emissions#######################################
df1.tp.def.t1 <- global_deforestation_timeSlice_HN(df1.tp.n, 1991, 2000)
df1.tp.def.t2 <- global_deforestation_timeSlice_HN(df1.tp.n, 2001, 2010)
df1.tp.def.t3 <- global_deforestation_timeSlice_HN(df1.tp.n, 2011, 2015)

df1.tp.oLu.t1 <- global_oLandUse_timeSlice_HN(df1.tp.n, 1991, 2000)
df1.tp.oLu.t2 <- global_oLandUse_timeSlice_HN(df1.tp.n, 2001, 2010)
df1.tp.oLu.t3 <- global_oLandUse_timeSlice_HN(df1.tp.n, 2011, 2015)

df1.tp.fry.t1 <- global_forestry_timeSlice_HN(df1.tp.n, 1991, 2000)
df1.tp.fry.t2 <- global_forestry_timeSlice_HN(df1.tp.n, 2001, 2010)
df1.tp.fry.t3 <- global_forestry_timeSlice_HN(df1.tp.n, 2011, 2015)

###########FAOSTAT: deforestation, forestland and other land use emissions#######################################
df3.tp.def.t1 <- global_deforestation_timeSlice_FAO(df3.tp.n, 1991, 2000)
df3.tp.def.t2 <- global_deforestation_timeSlice_FAO(df3.tp.n, 2001, 2010)
df3.tp.def.t3 <- global_deforestation_timeSlice_FAO(df3.tp.n, 2011, 2015)

df3.tp.fry.t1 <- global_forest_timeSlice_FAO(df3.tp.n, 1991, 2000)
df3.tp.fry.t2 <- global_forest_timeSlice_FAO(df3.tp.n, 2001, 2010)
df3.tp.fry.t3 <- global_forest_timeSlice_FAO(df3.tp.n, 2011, 2015)

df2.tp.fry.t1 <- global_forest_timeSlice_FAO(df2.tp.n, 1991, 2000)
df2.tp.fry.t2 <- global_forest_timeSlice_FAO(df2.tp.n, 2001, 2010)
df2.tp.fry.t3 <- global_forest_timeSlice_FAO(df2.tp.n, 2011, 2015)

df2.tp.oLu.t1 <- global_oLandUse_timeSlice_FAO(df2.tp.n, 1991, 2000)
df2.tp.oLu.t2 <- global_oLandUse_timeSlice_FAO(df2.tp.n, 2001, 2010)
df2.tp.oLu.t3 <- global_oLandUse_timeSlice_FAO(df2.tp.n, 2011, 2015)

#########Create dataframes for deforestation and other land use emissions################
dfc.totdef <- data.frame(Period = c("1991-2000", "1991-2000","1991-2000",
                                    "2001-2010", "2001-2010","2001-2010",
                                    "2011-2015", "2011-2015", "2011-2015"),
                         Source = c("Houghton","FAOSTAT","ESMs",
                                    "Houghton","FAOSTAT","ESMs",
                                    "Houghton","FAOSTAT","ESMs"),
                         Emissions = c(df1.tp.def.t1$CO2tot,df3.tp.def.t1$CO2tot,esms.mn.t1,
                                       df1.tp.def.t2$CO2tot,df3.tp.def.t2$CO2tot,esms.mn.t2,
                                       df1.tp.def.t3$CO2tot,df3.tp.def.t3$CO2tot,esms.mn.t3),
                         sd=c(NA, NA, esms.sd.t1,
                              NA, NA, esms.sd.t1,
                              NA, NA, esms.sd.t1))
dfc.totoLu <- data.frame(Period = c("1991-2000", "1991-2000","2001-2010", "2001-2010", 
                                    "2011-2015", "2011-2015"),
                         Source = c("Houghton","FAOSTAT","Houghton","FAOSTAT","Houghton","FAOSTAT"),
                         Emissions = c(df1.tp.oLu.t1$CO2tot,df2.tp.oLu.t1$CO2tot,
                                       df1.tp.oLu.t2$CO2tot,df2.tp.oLu.t2$CO2tot,
                                       df1.tp.oLu.t3$CO2tot,df2.tp.oLu.t3$CO2tot))
dfc.totoFr <- data.frame(Period = c("1991-2000", "1991-2000","2001-2010", "2001-2010", 
                                    "2011-2015", "2011-2015"),
                         Source = c("Houghton","FAOSTAT","Houghton","FAOSTAT","Houghton","FAOSTAT"),
                         Emissions = c(df1.tp.fry.t1$CO2tot,df3.tp.fry.t1$CO2tot,
                                       df1.tp.fry.t2$CO2tot,df3.tp.fry.t2$CO2tot,
                                       df1.tp.fry.t3$CO2tot,df3.tp.fry.t3$CO2tot))


#dfc.totdef.m <- melt(dfc.totdef,id.vars=c("Period","Source"))
dfc.totdef$Period <- factor(dfc.totdef$Period, levels=dfc.totdef$Period)
dfc.totdef$Source <- factor(dfc.totdef$Source, levels=c("Houghton","FAOSTAT","ESMs"),
                            labels=c('H&N','FAOSTAT',"ESMs"))

dfc.totoLu.m <- melt(dfc.totoLu,id.vars=c("Period","Source"))
dfc.totoLu.m$Period <- factor(dfc.totoLu.m$Period, levels=dfc.totoLu.m$Period)
dfc.totoLu.m$Source <- factor(dfc.totoLu.m$Source, levels=c("Houghton","FAOSTAT"),
                              labels=c('H&N','FAOSTAT'))

dfc.totoFr.m <- melt(dfc.totoFr,id.vars=c("Period","Source"))
dfc.totoFr.m$Period <- factor(dfc.totoFr.m$Period, levels=dfc.totoFr.m$Period)
dfc.totoFr.m$Source <- factor(dfc.totoFr.m$Source, levels=c("Houghton","FAOSTAT"),
                              labels=c('H&N','FAOSTAT'))



#################Plotting the bar plots####################################
values=brewer.pal(n = 5, name = "RdBu")
ggplot(dfc.totdef, aes(fill=Source, y=Emissions, x=Period)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(ymin=Emissions-sd, ymax=Emissions+sd), width=.2,position=position_dodge(.9)) +
  ylab(expression(Emissions~(GtCO[2]~yr^-1))) + xlab("")+
  scale_fill_manual(values = c("#F46D43","#74ADD1","#FEE090"))+
  theme(legend.position="bottom",legend.title=element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        panel.background = element_rect(fill = 'white', colour = 'black', linetype = "solid"),
        legend.text=element_text(colour="black",size=10),strip.background=element_rect(fill="white"),
        strip.text=element_text(colour="black",size=10), panel.grid.minor=element_blank(),
        axis.text.x=element_text(colour="black",size=12), axis.text.y=element_text(colour="black",size=12),
        axis.ticks.x=element_line(colour="black"),axis.ticks.y=element_line(colour="black"))


values=brewer.pal(n = 5, name = "RdBu")
ggplot(dfc.totoLu.m, aes(fill=Source, y=value, x=Period)) + 
  geom_bar(position="dodge", stat="identity")+
  ylab(expression(Other~Land~Use~emissions~(GtCO[2]~yr^-1))) + xlab("")+
  scale_y_continuous(limits = c(0, 2.0))+
  scale_fill_manual(values = c("#F46D43","#74ADD1"))+
  theme(legend.position="bottom",legend.title=element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        panel.background = element_rect(fill = 'white', colour = 'black', linetype = "solid"),
        legend.text=element_text(colour="black",size=10),strip.background=element_rect(fill="white"),
        strip.text=element_text(colour="black",size=10), panel.grid.minor=element_blank(),
        axis.text.x=element_text(colour="black",size=12), axis.text.y=element_text(colour="black",size=12),
        axis.ticks.x=element_line(colour="black"),axis.ticks.y=element_line(colour="black"))

values=brewer.pal(n = 5, name = "RdBu")
ggplot(dfc.totoFr.m, aes(fill=Source, y=value, x=Period)) + 
  geom_bar(position="dodge", stat="identity")+
  ylab(expression(Other~Forest~emissions/sources~(GtCO[2]~yr^-1))) + xlab("")+
  scale_y_continuous(limits = c(-3.5, 1.0))+
  scale_fill_manual(values = c("#F46D43","#74ADD1"))+
  theme(legend.position="bottom",legend.title=element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        panel.background = element_rect(fill = 'white', colour = 'black', linetype = "solid"),
        legend.text=element_text(colour="black",size=10),strip.background=element_rect(fill="white"),
        strip.text=element_text(colour="black",size=10), panel.grid.minor=element_blank(),
        axis.text.x=element_text(colour="black",size=12), axis.text.y=element_text(colour="black",size=12),
        axis.ticks.x=element_line(colour="black"),axis.ticks.y=element_line(colour="black"))











