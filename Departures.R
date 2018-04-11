# <Load Office of Public Managment Data and visualize the result.>
#     Copyright (C) <2018>  <Matthew C Morriss>
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

#This Script will load all of the employment data from cabinent level agencies.
setwd('/Users/matthew/Documents/GitHub/Trump_Administration_departures/TAD')
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


##################################### State Dept. ####################################
#of state department employees vs year

stateDept = filter(EmploymentData, Employment == "ST-DEPARTMENT OF STATE")

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


   #################################### Heatmap final ####################################
EmploymentData <- EmploymentData[!(EmploymentData$Employment == "Cabinet Level Agencies"),]
EData <- filter(EmploymentData,Month == 9)
EData <- filter(EData, Year >= 2006)
EData$Employment <- factor(EData$Employment)


# Now  calculate the precent change between year between carbinent
ED <- PercChange(EData,Var = "United.States",
                 type = "percent",
                 GroupVar = "Employment",
                 slideBy = -1,
                 TimeVar = "yearFrac",
                 NewVar = "PerChange")



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
  


# ggsave("Percent_change_per_Agency.eps", device=cairo_ps)
