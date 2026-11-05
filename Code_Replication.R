library(geofacet)
library(ggplot2)
library(readxl)
library(datasets)

setwd("") #Set WD to read file from there

DataFile <- read_excel("State_Year_1999_2019.xlsx") #Read the file

DataFile[DataFile$State == "Massachusetts", c("MRR_Adj", "MRR_Lo_Adj", "MRR_Hi_Adj")] <- NA 
#Make MA missing in the nonmetropolitan areas

DataFile$STABB<-state.abb[match(DataFile$State, state.name)] #State names match abbreviations
DataFile$STABB<-ifelse(is.na(DataFile$STABB)==TRUE,"DC",DataFile$STABB) #Confirm DC

#create plot
ggplot(DataFile, aes(Year, MRR)) +
  geom_ribbon(aes(ymin = MRR_Lo, ymax = MRR_Hi), fill = "blue", alpha = 0.15) +
  geom_ribbon(aes(ymin = MRR_Lo_Adj, ymax = MRR_Hi_Adj), fill = "forestgreen", alpha = 0.15) +
  geom_line(color = "blue") +
  geom_line(aes(y = MRR_Adj), color = "forestgreen") +
  facet_geo(~ State, grid = "us_state_grid2") +
  scale_y_continuous(breaks = c(85,100,115,130)) + theme_bw() +
  scale_x_continuous(breaks=c(1999,2009,2019)) +
  theme(
    axis.text.y = element_text(size=7.5),
    axis.text.x = element_text(size=8,angle = 90, vjust = 0.5, hjust=1))+
  ylab("Mortality Rate Ratios (Rural/Urban)*100")+
  xlab("Year")+
  geom_hline(yintercept=100, linetype="dashed",
             color = "red", linewidth=0.5)+
  labs(title = "Non-Metropolitan Mortality Penalty by State, 1999-2019",
       subtitle = "Blue line: observed. Green line: ethnoracial composition held equal. Shaded bands: 95% confidence intervals",
       caption="Red line represents equal rates.")

#Export 
ggsave("rplot.png",width=15,height=10, units="in", dpi=300)

