library(stringr)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(scales)
library(RColorBrewer)
library(ggrepel)
library(ggplot2)


# The script was adapted from original code by Amin Adibi
# available at https://www.shefa.ca/portfolio/covid19-trajectory-in-canada/


#Data import from https://open-fdoh.hub.arcgis.com/search
caseType <- "Confirmed"
caseType <- "confirmed_global"
fcaseline <- read_csv("https://opendata.arcgis.com/datasets/f5d69a918fb747019734d9a90cd602f4_0.csv")

fcases <- read_csv("https://opendata.arcgis.com/datasets/a7887f1940b34bf5a02c6f7f27a5cb2c_0.csv")


# Case count for Florida
Florida <- fcaseline %>%
  select(Case_) %>%
  rename (date = "Case_") %>%
  group_by(date) %>%
  mutate(cases = n()) %>% 
  mutate(name = "Florida") %>%
  filter(row_number() == 1) %>% #removes duplicate rows by filtering only 1st one
  ungroup() %>% 
  arrange(date) %>%
  mutate(cases = cumsum(cases))

  
# Case count for Jacksonville Metro Area
Jacksonville  <- fcaseline %>%
  select(Case_,County) %>%
  rename (date = "Case_") %>%
  rename (county = "County") %>%
  group_by(date) %>%
  filter(county == "Duval" | county == "St. Johns" | county == "Clay") %>%
  mutate(cases = n()) %>% 
  mutate(county = "Jacksonville") %>%
  filter(row_number() == 1) %>% #removes duplicate rows by filtering only 1st one
  rename (name = "county") %>%
  ungroup() %>% 
  arrange(date) %>%
  mutate(cases = cumsum(cases))


# Case count for Miami Metro Area  
Miami   <- fcaseline %>%
  select(Case_,County) %>%
  rename (date = "Case_") %>%
  rename (county = "County") %>%
  group_by(date) %>%
  filter(county == "Dade" | county == "Broward" | county == "Palm Beach") %>%
  mutate(cases = n()) %>% 
  mutate(county = "Miami") %>%
  filter(row_number() == 1) %>% #removes duplicate rows by filtering only 1st one
  rename (name = "county") %>%
  ungroup() %>% 
  arrange(date) %>%
  mutate(cases = cumsum(cases))

# Case count for Orlando Metro Area  
Orlando <- fcaseline %>%
  select(Case_,County) %>%
  rename (date = "Case_") %>%
  rename (county = "County") %>%
  group_by(date) %>%
  filter(county == "Orange" | county == "Osceola" | county == "Lake" | county == "Seminole") %>%
  mutate(cases = n()) %>% 
  mutate(county = "Orlando") %>%
  filter(row_number() == 1) %>% #removes duplicate rows by filtering only 1st one
  rename (name = "county") %>%
  ungroup() %>% 
  arrange(date) %>%
  mutate(cases = cumsum(cases))

# Case count for Tampa Metro Area  
Tampa <- fcaseline %>%
  select(Case_,County) %>%
  rename (date = "Case_") %>%
  rename (county = "County") %>%
  group_by(date) %>%
  filter(county == "Hillsborough" | county == "Pinellas" | county == "Pasco" | county == "Hernando") %>%
  mutate(cases = n()) %>% 
  mutate(county = "Tampa") %>%
  filter(row_number() == 1) %>% #removes duplicate rows by filtering only 1st one
  rename (name = "county") %>%
  ungroup() %>% 
  arrange(date) %>%
  mutate(cases = cumsum(cases))

# bind into one dataset
covidCases <- rbind(Florida,Miami,Orlando,Tampa,Jacksonville)
covidCases$date <- as.Date(covidCases$date)

colourBlindPal <- c("#E69F00", "#D55E00", "#009E73", "#56B4E9", "#CC79A7", "#0072B2")        

lineDataCases <- covidCases %>% 
  select (cases,name,date) %>% 
  filter (cases>=50) %>% arrange (name, date) %>% 
  group_by(name) %>% mutate(date = date - date[1L]) %>%
  mutate(days = as.numeric(date)) #%>% filter(days <30)

lastDay <- max(lineDataCases$days)+10  

#plot 50k y-axis
plot50k <- ggplot(data = lineDataCases, aes(x=days, y=cases, colour = name)) +
  geom_line(size=0.9) + geom_point(size=1) + xlab ("\n Number of days since 50th cases") + 
  ylab ("Cases \n") +
  geom_text_repel(data = lineDataCases %>% 
                    filter(days == last(days)), aes(label = name, 
                                                    x = days + 0.2, 
                                                    y = cases, 
                                                    color = name,
                                                    fontface=2), size = 5) + 

  scale_y_continuous(trans = log10_trans(),
                     breaks = c(50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000),labels = comma) +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35)) +
  
  annotate(geom = "text", x = 25, y = 70, 
           label = "Miami Counties: Dade + Broward + Palm Beach\n Tampa Counties: Hillsborough + Pinellas + Pasco + Hernando 
  Orlando Counties: Orange + Seminol + Osceola + Lake\n Jacksonville Counties: Duval + Clay + St.John\n", color = "#333333", size=3) +
  
  annotate("segment", linetype = "longdash", 
           x = 0, xend = lastDay, y = 50, yend = 10*(2^(1/3))^lastDay,
           colour = "#333333") +
  
  annotate(geom = "text", x = 30, y = 22000, 
           label = "... every 3 days", color = "#333333", fontface=2) +
  
  annotate("segment", linetype = "longdash", 
           x = 0, xend = lastDay, y = 50, yend = 10*(2^(1/7))^lastDay,
           colour = "#333333") +
  annotate(geom = "text", x = 30, y = 300, 
           label = "... every week", color = "#333333", fontface=2) +
  
  annotate("segment", linetype = "longdash", 
           x = 0, xend = 24.5, y = 50, yend = 10*(2^(1/2))^24.5,
           colour = "#333333") +
  annotate(geom = "text", x = 24, y = 57000, 
           label = "... every 2 days", color = "#333333", fontface=2) +
  
  annotate("segment", linetype = "longdash", 
           x = 0, xend = 12.5, y = 50, yend = 10*(2)^12.5,
           colour = "#333333") +
  annotate(geom = "text", x = 12, y = 57000, 
           label = "doubles every day", color = "#333333", fontface=2) +
  
  scale_colour_manual(values=colourBlindPal) +
  theme_economist() + 
  ggtitle("Trajectory of case counts in Florida\n", subtitle = "Cumulative number of cases by days since 50th case (Data from FL DOH)") +
  theme(text = element_text(size=13)) +
  theme(legend.position = "none") +
  theme(legend.title=element_blank()) +
  labs(caption = paste0("Last updated: ", covidCases[[nrow(covidCases),1]])) 

plot50k



#plot regions only
region <- subset(lineDataCases,name=="Miami" | name=="Tampa" | name=="Orlando" | name=="Jacksonville")

plot_region <- ggplot(data = region, aes(x=days, y=cases, colour = name)) +
  geom_line(size=0.9) + geom_point(size=1) + xlab ("\n Number of days since 50th cases") + 
  ylab ("Cases \n") +
  geom_text_repel(data = region %>% 
                    filter(days == last(days)), aes(label = name, 
                                                    x = days + 0.2, 
                                                    y = cases, 
                                                    color = name,
                                                    fontface=2), size = 5) + 
  
  scale_y_continuous(trans = log10_trans(),
                     breaks = c(50, 100, 200, 500, 1000, 2000, 5000, 10000),labels = comma) +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30.35)) +
  
  annotate(geom = "text", x = 25, y = 28, 
           label = "Miami Counties: Dade + Broward + Palm Beach\n Tampa Counties: Hillsborough + Pinellas + Pasco + Hernando 
  Orlando Counties: Orange + Seminol + Osceola + Lake\n Jacksonville Counties: Duval + Clay + St.John\n", color = "#333333", size=3) +
  
  annotate("segment", linetype = "longdash", 
           x = 0, xend = 30, y = 50, yend = 10*(2^(1/3))^30,
           colour = "#333333") +
  
  annotate(geom = "text", x = 30, y = 12000, 
           label = "... every 3 days", color = "#333333", fontface=2) +
  
  annotate("segment", linetype = "longdash", 
           x = 0, xend = lastDay, y = 50, yend = 10*(2^(1/7))^lastDay,
           colour = "#333333") +
  annotate(geom = "text", x = 30, y = 300, 
           label = "... every week", color = "#333333", fontface=2) +
  
  annotate("segment", linetype = "longdash", 
           x = 0, xend = 20, y = 50, yend = 10*(2^(1/2))^20,
           colour = "#333333") +
  annotate(geom = "text", x = 20, y = 12000, 
           label = "... every 2 days", color = "#333333", fontface=2) +
  
  annotate("segment", linetype = "longdash", 
           x = 0, xend = 10, y = 50, yend = 10*(2)^10,
           colour = "#333333") +
  annotate(geom = "text", x = 10, y = 12000, 
           label = "doubles every day", color = "#333333", fontface=2) +
  
  scale_colour_manual(values=colourBlindPal) +
  theme_economist() + 
  ggtitle("Trajectory of case counts in Florida\n", subtitle = "Cumulative number of cases by days since 50th case\n") +
  theme(text = element_text(size=13)) +
  theme(legend.position = "none") +
  theme(legend.title=element_blank()) +
  labs(caption = paste0("Last updated: ", covidCases[[nrow(covidCases),1]])) 



# case rate
caserate <- fcases %>%
  select(COUNTYNAME,C_AllResTypes,PUIsTotal) %>%
  group_by(COUNTYNAME) %>%
  mutate(cr = C_AllResTypes/PUIsTotal) %>%
  arrange(desc(cr))

currentDate <- covidCases[[nrow(covidCases),1]]
csvFileName <- paste("CaseRate ",currentDate,".csv",sep="")
write.csv(caserate,file=csvFileName)



#plots
# plot50k
# plot_region


pngFileName <- paste("Log Chart FL DOH ",currentDate,".png",sep="")

png(filename=pngFileName, width = 1280, height = 720)
plot(plot50k)
dev.off()


