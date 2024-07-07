library(tidyverse)
library(sf)
library(tidygeocoder)
library(readxl)
library(gridExtra)

##Neighborhood Boundaries
neighborhoods <- st_read("/Users/josetorres/Downloads/Boundaries - Community Areas (current)")
neighborhoods <- neighborhoods%>%
  mutate(area_numbe = as.numeric(area_numbe))%>%
  rename("Community Area" = "area_numbe")

##CRIME
crime <- read_csv("/Users/josetorres/Downloads/Crimes_-_2001_to_Present.csv")

crime <- crime %>%
  filter(!is.na(Latitude) & !is.na(Longitude),
         Year == 2010|Year == 2011|Year == 2012|Year == 2013|Year == 2014|Year == 2016|Year == 2017|Year == 2018|Year == 2019|Year == 2020|Year == 2021,
         `Primary Type`=="NARCOTICS",
         Arrest == T,
         Latitude>37) %>%
  st_as_sf(coords=c("Longitude","Latitude"), crs = 4326)

##SCHOOLS
cps20202021 <- read_csv("/Users/josetorres/Downloads/Chicago_Public_Schools_-_School_Profile_Information_SY2021.csv")
cps20182019 <- read_csv("/Users/josetorres/Downloads/Chicago_Public_Schools_-_School_Profile_Information_SY1819.csv")
cps20172018 <- read_csv("/Users/josetorres/Downloads/Chicago_Public_Schools_-_School_Profile_Information_SY1718.csv")
cps20162017 <- read_csv("/Users/josetorres/Downloads/Chicago_Public_Schools_-_School_Profile_Information_SY1617.csv")

cps20202021 <- cps20202021%>%
  mutate(school_rating = case_when(Overall_Rating=="Level 1+" ~ 5,
                                   Overall_Rating=="Level 1" ~ 4,
                                   Overall_Rating=="Level 2+" ~ 3,
                                   Overall_Rating=="Level 2" ~ 2,
                                   Overall_Rating=="Level 3" ~ 1,
                                   Overall_Rating=="Inability to Rate" ~ 0),
         Year = ifelse(Student_Count_Total>0,2021,0))%>%
  select(Year, School_ID, Short_Name, Long_Name, Student_Count_Total, Student_Count_Low_Income, Dress_Code, Overall_Rating, Rating_Status, School_Latitude, School_Longitude, school_rating)%>%
  filter(school_rating!= 0)

cps20182019 <- cps20182019%>%
  mutate(school_rating = case_when(Overall_Rating=="Level 1+" ~ 5,
                                   Overall_Rating=="Level 1" ~ 4,
                                   Overall_Rating=="Level 2+" ~ 3,
                                   Overall_Rating=="Level 2" ~ 2,
                                   Overall_Rating=="Level 3" ~ 1,
                                   Overall_Rating=="Inability to Rate" ~ 0),
         Year = ifelse(Student_Count_Total>0,2021,0))%>%
  select(Year, School_ID, Short_Name, Long_Name, Student_Count_Total, Student_Count_Low_Income, Dress_Code, Overall_Rating, Rating_Status, School_Latitude, School_Longitude, school_rating)%>%
  filter(school_rating!= 0)

cps20172018 <- cps20172018%>%
  mutate(school_rating = case_when(Overall_Rating=="Level 1+" ~ 5,
                                   Overall_Rating=="Level 1" ~ 4,
                                   Overall_Rating=="Level 2+" ~ 3,
                                   Overall_Rating=="Level 2" ~ 2,
                                   Overall_Rating=="Level 3" ~ 1,
                                   Overall_Rating=="Inability to Rate" ~ 0),
         Year = ifelse(Student_Count_Total>0,2021,0))%>%
  select(Year, School_ID, Short_Name, Long_Name, Student_Count_Total, Student_Count_Low_Income, Dress_Code, Overall_Rating, Rating_Status, School_Latitude, School_Longitude, school_rating)%>%
  filter(school_rating!= 0)

cps20162017 <- cps20162017%>%
  mutate(school_rating = case_when(Overall_Rating=="Level 1+" ~ 5,
                                   Overall_Rating=="Level 1" ~ 4,
                                   Overall_Rating=="Level 2+" ~ 3,
                                   Overall_Rating=="Level 2" ~ 2,
                                   Overall_Rating=="Level 3" ~ 1,
                                   Overall_Rating=="Inability to Rate" ~ 0),
         Year = ifelse(Student_Count_Total>0,2021,0))%>%
  select(Year, School_ID, Short_Name, Long_Name, Student_Count_Total, Student_Count_Low_Income, Dress_Code, Overall_Rating, Rating_Status, School_Latitude, School_Longitude, school_rating)%>%
  filter(school_rating!= 0)


cpsproject <- rbind(cps20202021,cps20182019,cps20172018,cps20162017)

cpsplotratings <- cpsproject%>%
  group_by(School_Latitude)%>%
  summarise(avg_rating = mean(school_rating),
            school_latitude = max(School_Latitude),
            school_longitude = max(School_Longitude))%>%
  st_as_sf(coords=c("school_longitude","school_latitude"), crs = 4326)%>%
  filter(avg_rating==5|avg_rating<3)%>%
  mutate(high = ifelse(avg_rating ==5,1,0))



#Crime by Community Area
crime_summary<-crime%>%
  group_by(`Community Area`)%>%
  summarise(total_drug_arrests = n())%>%
  st_drop_geometry()
                                      
                                      
##Neighborhood Statistics
neighborhoodpop<- read_xlsx("/Users/josetorres/Downloads/CCASF12010CMAP.xlsx")
neighborhoodpop <- neighborhoodpop%>%
  filter(P0050001!="Total Population")%>%
  select(GEOGNAME, GEOGKEYX, P0050001, P0050004)%>%
  rename("Name"="GEOGNAME",
         "Community Area"="GEOGKEYX",
         "total_pop"="P0050001",
         "total_black" = "P0050004")%>%
  mutate(`Community Area` = as.numeric(`Community Area`),
         total_black = as.numeric(total_black),
         total_pop = as.numeric(total_pop),
         pct_black = 100*(total_black/total_pop))
  



##Schools by Community Areas
cpsproject2 <- cpsproject %>%
  reverse_geocode(lat = School_Latitude, long = School_Longitude, method = 'osm',
                  address = address_found, full_results = TRUE)

cpsproject3 <- cpsproject2%>%
  select(School_ID, Long_Name, school_rating, suburb, neighbourhood)%>%
  mutate(`Community Area` = case_when(suburb == "Englewood" ~ 68,
                                      suburb == "Uptown" ~ 3,
                                      suburb == "Ashburn" ~ 70,
                                      suburb == "Washington Heights" ~ 73,
                                      suburb == "Avondale" ~ 21,
                                      suburb == "Avalon Park" ~ 45,
                                      suburb == "Gage Park" ~ 63,
                                      suburb == "Lower West Side" ~ 31,
                                      suburb == "Chicago Lawn" ~ 66,
                                      suburb == "West Englewood" ~ 67,
                                      suburb == "West Lawn" ~ 65,
                                      suburb == "North Lawndale" ~ 29,
                                      suburb == "Albany Park" ~ 14,
                                      suburb == "North Center" ~ 5,
                                      suburb == "Lakeview" ~ 6,
                                      suburb == "East Garfield Park" ~ 27,
                                      suburb == "Kenwood" ~ 39,
                                      suburb == "Near West Side" ~ 28,
                                      suburb == "Lakeview" ~ 6,
                                      suburb == "Lake View" ~ 6,
                                      suburb == "Lower West Side" ~ 31,
                                      suburb == "Grand Boulevard" ~ 38,
                                      suburb == "Jefferson Park" ~ 11,
                                      suburb == "Austin" ~ 25,
                                      suburb == "South Lawndale" ~ 30,
                                      suburb == "Norwood Park" ~ 10,
                                      suburb == "Auburn Gresham" ~ 71,
                                      suburb == "Morgan Park" ~ 75,
                                      suburb == "West Ridge" ~ 2,
                                      suburb == "New City" ~ 61,
                                      suburb == "Brighton Park" ~ 58,
                                      suburb == "Lincoln Park" ~ 7,
                                      suburb == "Lincoln Square" ~ 4,
                                      suburb == "North Park" ~ 13,
                                      suburb == "Edison Park" ~ 9,
                                      suburb == "Hermosa" ~ 20,
                                      suburb == "Irving Park" ~ 16,
                                      suburb == "McKinley Park" ~ 59,
                                      suburb == "Edgewater" ~ 77,
                                      suburb == "Forest Glen" ~ 12,
                                      suburb == "Near North Side" ~ 8,
                                      suburb == "West Pullman" ~ 53,
                                      suburb == "Rogers Park" ~ 1,
                                      suburb == "Oakland" ~ 36,
                                      suburb == "Pullman" ~ 50,
                                      suburb == "West Garfield Park" ~ 26,
                                      suburb == "South Shore" ~ 43,
                                      suburb == "South Chicago" ~ 46,
                                      suburb == "West Town" ~ 24,
                                      suburb == "Clearing" ~ 64,
                                      suburb == "Garfield Ridge" ~ 56,
                                      suburb == "Woodlawn" ~ 42,
                                      suburb == "Humboldt Park" ~ 23,
                                      suburb == "South Deering" ~ 51,
                                      suburb == "Douglas" ~ 35,
                                      suburb == "Logan Square" ~ 22,
                                      suburb == "Archer Heights" ~ 57,
                                      suburb == "Near South Side" ~ 33,
                                      suburb == "Washington Park" ~ 40,
                                      suburb == "Hyde Park" ~ 41,
                                      suburb == "Portage Park" ~ 15,
                                      suburb == "Bridgeport" ~ 60,
                                      suburb == "Mount Greenwood" ~ 74,
                                      suburb == "Beverly" ~ 72,
                                      suburb == "Riverdale" ~ 54,
                                      suburb == "Loop" ~ 32,
                                      suburb == "Hegewisch" ~ 55,
                                      suburb == "Fuller Park" ~ 37,
                                      suburb == "Calumet Heights" ~ 48,
                                      suburb == "Armour Square" ~ 34,
                                      neighbourhood == "Roseland" ~ 49,
                                      neighbourhood == "Belmont Cragin" ~ 19,
                                      neighbourhood == "Chatham" ~ 19,
                                      neighbourhood == "Dunning" ~ 17,
                                      neighbourhood == "East Side" ~ 52,
                                      neighbourhood == "West Elsdon" ~ 62,
                                      neighbourhood == "Greater Grand Crossing" ~ 69,
                                      Long_Name == "Henry O Tanner Elementary School" ~ 69,
                                      Long_Name == "Paul Revere Elementary School" ~ 69,
                                      Long_Name == "Charles S Deneen Elementary School" ~ 69))%>%
  drop_na(`Community Area`)
  
summarycps <- cpsproject3%>%
  group_by(`Community Area`)%>%
  summarise(avg_community_rating = mean(school_rating))
  

##Connecting it all
fullsummary <- list(summarycps,neighborhoodpop,crime_summary)%>%
        reduce(full_join, by='Community Area')%>%
  mutate(total_pop = as.numeric(total_pop),
         drug_arrests_rate_per_resident = total_drug_arrests/total_pop)

## Graphic 2
plot1 <- fullsummary%>%
  select(`Community Area`,avg_community_rating,pct_black)%>%
  mutate(type = case_when(avg_community_rating>0 ~ "Average School Rating"))%>%
  rename("avg" = "avg_community_rating")

plot2 <- fullsummary%>%
  select(`Community Area`,drug_arrests_rate_per_resident, pct_black)%>%
  mutate(type = case_when(drug_arrests_rate_per_resident>0 ~ "Drug Arrests per Resident"))%>%
  rename("avg" = "drug_arrests_rate_per_resident")



plot3 <- rbind(plot1,plot2)

plot3%>%
  drop_na(avg)%>%
  ggplot(aes(x=pct_black,y=avg))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_grid(rows = vars(type), scales = "free", switch="y")+
  scale_y_continuous(trans = "log10")+
  theme_bw()+
  theme(axis.title=element_text(size=20,face = "bold"),
        plot.title = element_text(size=20,face = "bold",hjust = 0.5),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.text.y = element_text(size = 20))+
  xlab("Percent Black")+
  ylab(" ")+
  ggtitle("Average School Ratings and Drug Arrests Rates by Community")




##regression of drug arrests and percent black
summary(lm(drug_arrests_rate_per_resident ~ pct_black,
           data = fullsummary))

##regression of school rating and percent black
summary(lm(avg_community_rating ~ pct_black,
           data = fullsummary))

##race and crime+school map
Schoolplotwithcrime <- cpsplotratings%>%
  ggplot()+
  geom_sf(data = neighborhoods)+
  geom_sf(data=crime, alpha = 0.1, size = 0.5)+
  geom_sf(aes(color=as.factor(high)), size=4)+
  scale_color_manual(values = c("red","blue"),
                     labels = c("Low Achieving School","High Achieving School"))+
  labs(title="Drug Arrests and Average School Rating",
       subtitle = "2010-2021",
       caption = "Data from Chicago Data Portal",
       color = "School Rating")+
  guides(colour=guide_legend(override.aes=list(alpha=1, size=10))) +
  theme_void() +
  theme(plot.title=element_text(size=25, face = "bold", hjust = 0.5),
        plot.subtitle=element_text(size=15, face = "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_text(size=15),
        legend.text = element_text(size=15))


all <- list(neighborhoods,neighborhoodpop)%>%
  reduce(full_join, by='Community Area')

neighborhooddemographics <- all%>%
  ggplot()+
  geom_sf(aes(fill = pct_black))+
  scale_fill_gradient(low="white", high="black")+
  labs(title="Black Resident Distribution",
       caption = "Data from 2010 Census",
       fill = "Percent Black")+
  guides(colour=guide_legend(override.aes=list(alpha=1, size=10))) +
  theme_void() +
  theme(plot.title=element_text(size=25, face = "bold", hjust = 0.5),
        plot.subtitle=element_text(size=15, face = "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_text(size=15),
        legend.text = element_text(size=15))

grid.arrange(neighborhooddemographics, Schoolplotwithcrime, ncol = 2)
