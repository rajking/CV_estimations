library(stringr)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(scales)
library(RColorBrewer)
library(ggrepel)
library(ggplot2)


#Data import
caseType <- "Confirmed"
caseType <- "confirmed_global"
time_series_19_covid_Confirmed <- read_csv("time_series_covid19_confirmed_florida.csv")

covidCases <- time_series_19_covid_Confirmed %>% rename (country = "Country/Region") %>% rename (name = "Province/State")
colourBlindPal <- c("#E69F00", "#D55E00", "#009E73", "#56B4E9", "#CC79A7", "#0072B2")        

lineDataCases <- covidCases %>% 
  select (-c(country)) %>% 
  pivot_longer(cols = -1, names_to = "date", values_to = "Cases") %>%  mutate(date=mdy(date)) %>%
  filter (Cases>=10) %>% arrange (name, date) %>% 
  group_by(name) %>% mutate(date = date - date[1L]) %>%
  mutate(days = as.numeric(date)) #%>% filter(days <30)

lastDay <- max(lineDataCases$days)+10  

#plot 100k y-axis
plot100k <- ggplot(data = lineDataCases, aes(x=days, y=Cases, colour = name)) +
  geom_line(size=0.9) + geom_point(size=1) + xlab ("\n Number of days since 10th cases") + 
  ylab ("Cases \n") +
  geom_text_repel(data = lineDataCases %>% 
                    filter(days == last(days)), aes(label = name, 
                                                    x = days + 0.2, 
                                                    y = Cases, 
                                                    color = name,
                                                    fontface=2), size = 5) + 

  scale_y_continuous(trans = log10_trans(),
                     breaks = c(10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000, 100000),labels = comma) +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30)) +
  
  annotate("segment", linetype = "longdash", 
           x = 0, xend = lastDay, y = 10, yend = 10*(2^(1/3))^lastDay,
           colour = "#333333") +
  
  annotate(geom = "text", x = 26, y = 6000, 
           label = "... every 3 days", color = "#333333", fontface=2,
           angle = 33) +
  
  
  annotate("segment", linetype = "longdash", 
           x = 0, xend = lastDay, y = 10, yend = 10*(2^(1/7))^lastDay,
           colour = "#333333") +
  annotate(geom = "text", x = 26, y = 188, 
           label = "... every week", color = "#333333", fontface=2,
           angle = 20) +
  
  annotate("segment", linetype = "longdash", 
           x = 0, xend = 26.5, y = 10, yend = 10*(2^(1/2))^26.5,
           colour = "#333333") +
  annotate(geom = "text", x = 23.2, y = 45000, 
           label = "... every 2 days", color = "#333333", fontface=2,
           angle = 43) +
  
  annotate("segment", linetype = "longdash", 
           x = 0, xend = 13.2, y = 10, yend = 10*(2)^13.2,
           colour = "#333333") +
  annotate(geom = "text", x = 11.2, y = 45000, 
           label = "doubles every day", color = "#333333", fontface=2,
           angle = 60) +
  
  scale_colour_manual(values=colourBlindPal) +
  theme_economist() + 
  ggtitle("Trajectory of case counts in Florida\n", subtitle = "Cumulative number of cases by days since 10th case") +
  theme(text = element_text(size=13)) +
  theme(legend.position = "none") +
  theme(legend.title=element_blank()) +
  labs(caption = paste0("Last updated: ", ymd(mdy(colnames(covidCases[length(covidCases)]))))) 


# plot 10k y axis
plot10k <- ggplot(data = lineDataCases, aes(x=days, y=Cases, colour = name)) +
  geom_line(size=0.9) + geom_point(size=1) + xlab ("\n Number of days since 10th cases") + 
  ylab ("Cases \n") +
  geom_text_repel(data = lineDataCases %>% 
                    filter(days == last(days)), aes(label = name, 
                                                    x = days + 0.2, 
                                                    y = Cases, 
                                                    color = name,
                                                    fontface=2), size = 5) + 
  
  scale_y_continuous(trans = log10_trans(),
                     breaks = c(10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000),labels = comma) +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30)) +
  
  annotate("segment", linetype = "longdash", 
           x = 0, xend = lastDay, y = 10, yend = 10*(2^(1/3))^lastDay,
           colour = "#333333") +
  
  annotate(geom = "text", x = 26, y = 6000, 
           label = "... every 3 days", color = "#333333", fontface=2) +
  
  annotate("segment", linetype = "longdash", 
           x = 0, xend = lastDay, y = 10, yend = 10*(2^(1/7))^lastDay,
           colour = "#333333") +
  annotate(geom = "text", x = 26, y = 188, 
           label = "... every week", color = "#333333", fontface=2) +
  
  annotate("segment", linetype = "longdash", 
           x = 0, xend = 20, y = 10, yend = 10*(2^(1/2))^20,
           colour = "#333333") +
  annotate(geom = "text", x = 20, y = 12000, 
           label = "... every 2 days", color = "#333333", fontface=2) +
  
  annotate("segment", linetype = "longdash", 
           x = 0, xend = 10, y = 10, yend = 10*(2)^10,
           colour = "#333333") +
  annotate(geom = "text", x = 10, y = 12000, 
           label = "doubles every day", color = "#333333", fontface=2) +
  
  scale_colour_manual(values=colourBlindPal) +
  theme_economist() + 
  ggtitle("Trajectory of case counts in Florida\n", subtitle = "Cumulative number of cases by days since 10th case") +
  theme(text = element_text(size=13)) +
  theme(legend.position = "none") +
  theme(legend.title=element_blank()) +
  labs(caption = paste0("Last updated: ", ymd(mdy(colnames(covidCases[length(covidCases)]))))) 


#plot regions only
region <- subset(lineDataCases,name=="Miami Area" | name=="Tampa Area" | name=="Orlando Area")

plot_region <- ggplot(data = region, aes(x=days, y=Cases, colour = name)) +
  geom_line(size=0.9) + geom_point(size=1) + xlab ("\n Number of days since 10th cases") + 
  ylab ("Cases \n") +
  geom_text_repel(data = region %>% 
                    filter(days == last(days)), aes(label = name, 
                                                    x = days + 0.2, 
                                                    y = Cases, 
                                                    color = name,
                                                    fontface=2), size = 5) + 
  
  scale_y_continuous(trans = log10_trans(),
                     breaks = c(10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000),labels = comma) +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30)) +
  
  annotate("segment", linetype = "longdash", 
           x = 0, xend = lastDay, y = 10, yend = 10*(2^(1/3))^lastDay,
           colour = "#333333") +
  
  annotate(geom = "text", x = 26, y = 6000, 
           label = "... every 3 days", color = "#333333", fontface=2) +
  
  annotate("segment", linetype = "longdash", 
           x = 0, xend = lastDay, y = 10, yend = 10*(2^(1/7))^lastDay,
           colour = "#333333") +
  annotate(geom = "text", x = 26, y = 188, 
           label = "... every week", color = "#333333", fontface=2) +
  
  annotate("segment", linetype = "longdash", 
           x = 0, xend = 20, y = 10, yend = 10*(2^(1/2))^20,
           colour = "#333333") +
  annotate(geom = "text", x = 20, y = 12000, 
           label = "... every 2 days", color = "#333333", fontface=2) +
  
  annotate("segment", linetype = "longdash", 
           x = 0, xend = 10, y = 10, yend = 10*(2)^10,
           colour = "#333333") +
  annotate(geom = "text", x = 10, y = 12000, 
           label = "doubles every day", color = "#333333", fontface=2) +
  
  scale_colour_manual(values=colourBlindPal) +
  theme_economist() + 
  ggtitle("Trajectory of case counts in Florida\n", subtitle = "Cumulative number of cases by days since 10th case") +
  theme(text = element_text(size=13)) +
  theme(legend.position = "none") +
  theme(legend.title=element_blank()) +
  labs(caption = paste0("Last updated: ", ymd(mdy(colnames(covidCases[length(covidCases)]))))) 


#plots
# plot100k
# plot10k

plot_region


