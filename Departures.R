#This Script will load all of the employment data from cabinent level agencies.
setwd('/Users/matthew/Documents/GitHub/Trump_Administration_departures/Data/')
library(dplyr)
library(ggplot2)
library(readr)
library(reshape2)
library(DataCombine)
#################################### Load Data ####################################

# Load data from the office of personnel management

EmploymentData <- read.csv('OPM_Data.csv')

#calculate fraction of year 

yearFrac = EmploymentData$Year + (EmploymentData$Month/12)

EmploymentData <- cbind(EmploymentData, yearFrac)

#################################### How many people work for government ###################################
all <- aggregate(EmploymentData$United.States, by=list(Category=EmploymentData$fracYear), FUN=sum)


a <- ggplot(all, aes(x = all$Category, y = all$x))+
  
  geom_line()+
  xlim(2004, 2018)

a


##################################### create a plot ####################################
#of state department employees vs year

stateDept = filter(EmploymentData, Employment == "DJ-DEPARTMENT OF JUSTICE")

p <- ggplot(stateDept, aes(x = stateDept$fracYear, y = stateDept$Location...All
                           ) )+
  geom_line()+
  # geom_line(data = stateDept, aes(x = stateDept$fracYear, y = stateDept$Foreign.Countries))+
  geom_vline(xintercept = 2017)+
  labs(x = 'Year',y = 'Number of Employees')+
  labs(title = "Number of State Department Employees")+
  theme_classic()+
  
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  scale_x_continuous( 
                   breaks =seq(2006, 2018, 2),
                   limits = c(2006, 2018))+
    ylim(10000, 14000)

p
##################################### create a plot ####################################
#of state department employees vs year

edDept = filter(EmploymentData, Employment == "ED-DEPARTMENT OF EDUCATION")

e <- ggplot(stateDept, aes(x = edDept$fracYear, y = edDept$Location...All
) )+
  geom_line()+
  # geom_line(data = stateDept, aes(x = stateDept$fracYear, y = stateDept$Foreign.Countries))+
  geom_vline(xintercept = 2017)+
  labs(x = 'Year',y = 'Number of Employees')+
  labs(title = "Number of Department Department of Education Employees")+
  theme_classic()+
  
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  scale_x_continuous( 
    breaks =seq(2006, 2018, 2),
    limits = c(2006, 2018))
  # ylim(10000, 15000)

e

##################################### create a plot ####################################
#of state department employees vs year

eDept = filter(ED, Employment == "IN-DEPARTMENT OF THE INTERIOR")

e <- ggplot(stateDept, aes(x = eDept$Year, y = eDept$PerChange) )+
  geom_line()+
  # geom_line(data = stateDept, aes(x = stateDept$fracYear, y = stateDept$Foreign.Countries))+
  geom_vline(xintercept = 2017)+
  labs(x = 'Year',y = 'Number of Employees')+
  labs(title = "Number of Department Department of Energy Employees")+
  theme_classic()+
  
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  scale_x_continuous( 
    breaks =seq(2006, 2018, 2),
    limits = c(2006, 2018))
# ylim(10000, 15000)

e


#################################### heat map ####################################
EmploymentData <- EmploymentData[!(EmploymentData$Employment == "Cabinet Level Agencies"),]
EData <- filter(EmploymentData,Month == 9)
EData <- filter(ED, Year >= 2006)

# #Test of how well the data is concurrent
# EData <- EData[order(EData$Employment,EData$yearFrac),]
# EData$Employment <- factor(EData$Employment)
barplot(prop.table(table(EData$Employment)))

# Now try to calculate the precent change between year between carbinent
ED <- PercChange(EData,Var = "United.States",
           type = "percent",
           GroupVar = "Employment",
           slideBy = -1,
           TimeVar = "yearFrac",
           NewVar = "PerChange")
# EmploymentData <- EmploymentData[order(Employment)]

stateDept = filter(ED, Employment == "ST-DEPARTMENT OF STATE")

ggplot(ED, aes(x = ED$Year,
                           y = ED$Employment,
                           # color = ED$PerChange,
                           fill = ED$PerChange)) +
  
  
  geom_tile(colour="white", linewidth=2, 
            width=.9, height=.9) +
  geom_vline(xintercept = 2017)+
  coord_fixed()+
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen")+
  labs(x = "Year", y = "Cabinent Department", fill = "Percent Change")+
  theme_classic()+
  xlim(2007, 2017)+
    scale_x_continuous( 
      breaks =seq(2006, 2018, 1),
      limits = c(2006, 2018))+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))
    
   #################################### Heatmap final ####################################
EmploymentData <- EmploymentData[!(EmploymentData$Employment == "Cabinet Level Agencies"),]
EData <- filter(EmploymentData,Month == 9)
EData <- filter(EData, Year >= 2006)
# EData <- filter(EData, EmploymentData$Employment != "AG-DEPARTMENT OF AGRICULTURE" &
#                   EmploymentData$Employment!= "IN-DEPARTMENT OF THE INTERIOR" )

# #Test of how well the data is concurrent
# EData <- EData[order(EData$yearFrac),]
EData$Employment <- factor(EData$Employment)
# barplot(prop.table(table(EData$Employment)))

# Now try to calculate the precent change between year between carbinent
ED <- PercChange(EData,Var = "United.States",
                 type = "percent",
                 GroupVar = "Employment",
                 slideBy = -1,
                 TimeVar = "yearFrac",
                 NewVar = "PerChange")
# EmploymentData <- EmploymentData[order(Employment)]

stateDept = filter(ED, Employment == "ST-DEPARTMENT OF STATE")

ggplot(ED, aes(x = ED$Year,
               y = ED$Employment,
               # color = ED$PerChange,
               fill = ED$PerChange)) +
  
  
  geom_tile(colour="white",  
            width=.9, height=.9) +
  geom_vline(xintercept = 2016.5)+
  coord_fixed()+
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen")+
  labs(x = "Year", y = "Cabinent Department", fill = "Percent Change")+
  ggtitle("% Change in Employees Cabinent Level Agencies")+
  theme_classic()+
  xlim(2007, 2017)+
  scale_x_continuous( 
    breaks =seq(2006, 2018, 1),
    limits = c(2006, 2018))+
  scale_y_discrete(limits = rev(levels(ED$Employment)))+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))
  # coord_flip()


ggsave("Percent_change_per_Agency.eps", device=cairo_ps)