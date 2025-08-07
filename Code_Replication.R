library(geofacet)
library(ggplot2)
library(readxl)
library(datasets)

setwd("") #Set WD to read file from there

DataFile <- read_excel("State_MRR_RuralPenalty.xlsx")

DataFile$STABB<-state.abb[match(DataFile$State, state.name)]

DataFile$STABB<-ifelse(is.na(DataFile$STABB)==TRUE,"DC",DataFile$STABB)

ggplot(DataFile, aes(Year, MRR)) +
  geom_line(size=1.5) +
  facet_geo(~ STABB, grid = "us_state_grid2") +
  scale_y_continuous(breaks = c(0,200)) + theme_bw()+
  scale_x_continuous(breaks=c(2000,2015)) +
                       theme(axis.text.x = element_text(size=8,angle = 90, vjust = 0.5, hjust=1))+
  ylab("Mortality Rate Ratio")+xlab("Year")+
  theme(legend.position = "bottom",legend.title =element_blank())


#NGR
DataFile$STABB<-state.abb[match(DataFile$State, state.name)]
DataFile$STABB<-ifelse(is.na(DataFile$STABB)==TRUE,"DC",DataFile$STABB)

#Open a Jpg file
png("rplot.png", width = 1500, height =1000)

#create plot
ggplot(DataFile, aes(Year, MRR)) +
  geom_line(color = "blue") +
  facet_geo(~ State, grid = "us_state_grid2") +
  scale_y_continuous(breaks = c(85,100,115,130)) + theme_bw()+
  scale_x_continuous(breaks=c(1999,2008,2016)) +
  theme(
    axis.text.y = element_text(size=7.5),
    axis.text.x = element_text(size=8,angle = 90, vjust = 0.5, hjust=1))+
  ylab("Mortality Rate Ratios (Rural/Urban)*100")+
  xlab("Year")+
  theme(legend.position = "bottom",legend.title =element_blank())+
  geom_hline(yintercept=100, linetype="dashed", 
             color = "red", size=0.10)+
  labs(title = "Non-Metropolitan Mortality Penalty by State, 1999-2016",
       subtitle = "Red line represents equal rates",
       caption="Values for New Jersey, Rhode Island and Washington DC are missing")

# Close the file
dev.off()
