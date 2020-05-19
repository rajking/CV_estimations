library(tidyverse)
library(lubridate)
library(padr)
library(devtools)
library(NCoVUtils) # install_github("https://github.com/epiforecasts/NCoVUtils")
library(readxl)
library(ggplot2)
library(scales)
library(cowplot)
library(patchwork)
library(stringr)
library(ggthemes)
library(RColorBrewer)
library(ggrepel)
library(stringr)


#-------get data--------------
#Data import from NYtime github repository
USA_counties <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

# filter to florida
Florida_counties <- USA_counties %>%
  filter(state=="Florida")

#-------Case counts---------------
# Case count for Florida
Florida <- Florida_counties %>%
  select(date,state,cases,deaths) %>%
  group_by(date) %>%
  mutate(state = state) %>%
  mutate(cases = sum(cases)) %>%
  mutate(deaths = sum(deaths)) %>%
  filter(row_number() == 1) %>% #removes duplicate rows by filtering only 1st one
  mutate(name = "Florida")

# Case count for Jacksonville Metro Area  
Jacksonville <- Florida_counties %>%
  select(date, county, state, cases, deaths) %>%
  group_by(date) %>%
  filter(county == "Duval" | county == "St. Johns" | county == "Clay") %>%
  mutate(cases = sum(cases)) %>%
  mutate(deaths = sum(deaths)) %>%
  mutate(county = "Jacksonville") %>%
  filter(row_number() == 1) %>%
  rename (name = "county")

# Case count for Miami Metro Area  
Miami <- Florida_counties %>%
  select(date, county, state, cases, deaths) %>%
  group_by(date) %>%
  filter(county == "Miami-Dade" | county == "	Broward" | county == "Palm Beach") %>%
  mutate(cases = sum(cases)) %>%
  mutate(deaths = sum(deaths)) %>%
  mutate(county = "Miami") %>%
  filter(row_number() == 1) %>%
  rename (name = "county")

# Case count for Orlando Metro Area  
Orlando <- Florida_counties %>%
  select(date, county, state, cases, deaths) %>%
  group_by(date) %>%
  filter(county == "Orange" | county == "Osceola" | county == "Lake" | county == "Seminole") %>%
  mutate(cases = sum(cases)) %>%
  mutate(deaths = sum(deaths)) %>%
  mutate(county = "Orlando") %>%
  filter(row_number() == 1) %>%
  rename (name = "county")


# Case count for Tampa Metro Area  
Tampa <- Florida_counties %>%
  select(date, county, state, cases, deaths) %>%
  group_by(date) %>%
  filter(county == "Hillsborough" | county == "Pinellas" | county == "Pasco" | county == "Hernando") %>%
  mutate(cases = sum(cases)) %>%
  mutate(deaths = sum(deaths)) %>%
  mutate(county = "Tampa") %>%
  filter(row_number() == 1) %>%
  rename (name = "county")

#------Bind data together-----------
allDat <- rbind(Florida,Miami,Orlando,Tampa,Jacksonville)

allDatDesc <- allDat %>% 
  dplyr::ungroup() %>%
  dplyr::arrange(name, date) %>% 
  dplyr::mutate(date = lubridate::ymd(date)) %>% 
  dplyr::rename(new_cases = cases, new_deaths = deaths) %>%
  dplyr::select(date, name, new_cases, new_deaths)

#-------Under-reporting-----------------


zmeanHDT <- 13
zsdHDT <- 12.7
zmedianHDT <- 9.1
muHDT <- log(zmedianHDT)
sigmaHDT <- sqrt(2*(log(zmeanHDT) - muHDT))
cCFRBaseline <- (1.38/2)
cCFREstimateRange <- c(1.23/2, 1.53/2)
cCFRIQRRange <- c(1.3/2, 1.4/2)



# Hospitalisation to death distribution
hospitalisation_to_death_truncated <- function(x)
{
  dlnorm(x, muHDT, sigmaHDT)
}

# Function to work out correction CFR
scale_cfr <- function(data_1_in, delay_fun){
  case_incidence <- data_1_in$new_cases
  death_incidence <- data_1_in$new_deaths
  cumulative_known_t <- 0 # cumulative cases with known outcome at time tt
  # Sum over cases up to time tt
  for(ii in 1:nrow(data_1_in)){
    known_i <- 0 # number of cases with known outcome at time ii
    for(jj in 0:(ii - 1)){
      known_jj <- (case_incidence[ii - jj]*delay_fun(jj))
      known_i <- known_i + known_jj
    }
    cumulative_known_t <- cumulative_known_t + known_i # Tally cumulative known
  }
  # naive CFR value
  b_tt <- sum(death_incidence)/sum(case_incidence) 
  # corrected CFR estimator
  p_tt <- sum(death_incidence)/cumulative_known_t
  data.frame(nCFR = b_tt, cCFR = p_tt, total_deaths = sum(death_incidence), 
             cum_known_t = round(cumulative_known_t), total_cases = sum(case_incidence))
}



# Do analysis
allTogetherClean2 <- allDatDesc %>%
  dplyr::group_by(name) %>%
  padr::pad() %>%
  dplyr::mutate(new_cases = tidyr::replace_na(new_cases, 0),
                new_deaths = tidyr::replace_na(new_deaths, 0)) %>%
  dplyr::group_by(name) %>%
  dplyr::mutate(cum_deaths = sum(new_deaths)) %>%
  dplyr::filter(cum_deaths > 0) %>%
  dplyr::select(-cum_deaths) %>%
  dplyr::do(scale_cfr(., delay_fun = hospitalisation_to_death_truncated)) %>%
  dplyr::filter(cum_known_t > 0) %>%
  dplyr::mutate(nCFR_UQ = binom.test(total_deaths, total_cases)$conf.int[2],
                nCFR_LQ = binom.test(total_deaths, total_cases)$conf.int[1],
                cCFR_UQ = binom.test(total_deaths, cum_known_t)$conf.int[2],
                cCFR_LQ = binom.test(total_deaths, cum_known_t)$conf.int[1],
                underreporting_estimate = cCFRBaseline / (100*cCFR),
                lower = cCFREstimateRange[1] / (100 * cCFR_UQ),
                upper = cCFREstimateRange[2] / (100 * cCFR_LQ),
                quantile25 = binom.test(total_deaths, cum_known_t, conf.level = 0.5)$conf.int[1],
                quantile75 = binom.test(total_deaths, cum_known_t, conf.level = 0.5)$conf.int[2],
                bottom = cCFRIQRRange[1] / (100 * quantile75),
                top = cCFRIQRRange[2] / (100 * quantile25)) %>%
  dplyr::filter(total_deaths > 10)

#confidence = dplyr::case_when(total_deaths >= 100 ~ "Countries which have reported 100 or more deaths",
#                              total_deaths < 100 && total_deaths > 10  ~ "Countries that have reported fewer than 100 deaths, but more than 10",
#                              total_deaths >= 5 && total_deaths <= 10 ~ "Countries that have reported greater than or equal to 5 deaths") %>%
#                               
# factor(levels = c("Countries which have reported 100 or more deaths",
#                   "Countries that have reported fewer than 100 deaths, but more than 10", 
#                   "Countries that have reported greater than or equal to 5 deaths"))

reportDataFinal <- allTogetherClean2 %>%
  dplyr::select(name, total_cases, total_deaths, underreporting_estimate, lower,
                upper, bottom, top) %>%
  #dplyr::mutate(is.numeric, signif, digits=2)  %>%
  dplyr::mutate(underreporting_estimate = ifelse(underreporting_estimate <= 1, underreporting_estimate, 1)) %>%
  dplyr::mutate(upper = ifelse(upper <= 1, upper, 1)) %>%
  dplyr::mutate(top = ifelse(top <= 1, top, 1)) %>%
  dplyr::mutate(underreporting_estimate = signif(underreporting_estimate, 2)) %>%
  dplyr::mutate(lower = signif(lower, 2)) %>%
  dplyr::mutate(upper = signif(upper, 2)) %>%
  dplyr::mutate(bottom = signif(bottom, 2)) %>%
  dplyr::mutate(top = signif(top, 2)) %>%
  dplyr::ungroup(name) %>%
  dplyr::mutate(name = name %>% stringr::str_replace_all("_", " ")) %>% 
  dplyr::mutate(underreporting_estimate_clean = paste0(underreporting_estimate*100,
                                                       "% (",lower*100,"% - ",upper*100,"%)"))




#-------plot UR results----------------
dataTable <- reportDataFinal %>% dplyr::select(name, underreporting_estimate_clean, total_cases, total_deaths)

dataPlot <- reportDataFinal %>% 
  dplyr::mutate(
    name = name  %>% 
      factor(levels = reportDataFinal %>% 
               dplyr::arrange(desc(underreporting_estimate)) %>% 
               dplyr::pull(name) %>% 
               unique()))

#subPlotData1 <- dataPlot %>% filter(confidence == "Countries which have reported 100 or more deaths")
#subPlotData2 <- dataPlot %>% filter(confidence == "Countries that have reported fewer than 100 deaths, but more than 10")
#subPlotData3 <- dataPlot %>% filter(confidence == "Countries that have reported fewer than or equal to 10 deaths")

UR_plot <- dataPlot %>% 
  ggplot2::ggplot(ggplot2::aes(x = name)) +
  #ggplot2::geom_linerange(ggplot2::aes(ymin = bottom, ymax = top), col = "#344b85", size = 4,  alpha = 0.7) +
  ggplot2::geom_linerange(ggplot2::aes(ymin = lower, ymax = upper), col = "#344b85",  size = 4,  alpha = 0.7) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1L), limits=c(0,1),  breaks = seq(0, 1, 0.1)) +
  ggplot2::geom_hline(yintercept = 0, linetype = 2) +
  ggplot2::geom_hline(yintercept = 1, linetype = 2) +
  cowplot::theme_cowplot() +
  cowplot::panel_border() +
  ggplot2::theme(axis.text.x = element_text(angle = 0, size = 15), axis.text.y = element_text(size = 15)) +
  ggplot2::theme(legend.position = "none") +
  ggplot2::guides(col = ggplot2::guide_legend(nrow = 2)) +
  ggplot2::labs(x = "Region", y = "Percentage of cases reported") + 
  ggplot2::coord_flip() + 
  ggplot2::ggtitle("Estimates for percentage of symptomatic cases reported in Florida", subtitle = "For Metro areas with greater than 10 deaths") +
  ggplot2::theme(text = element_text(size=13)) +
  ggplot2::theme(legend.position = "none") +
  ggplot2::theme(legend.title=element_blank()) +
  ggplot2::labs(caption = paste0("Last updated: ", allDat[[nrow(allDat),1]])) 




#---------New Daily Cases---------------


NYTlineData <- allDatDesc %>% 
  select (new_cases,name,date) %>% 
  rename (cases = "new_cases") %>%
  group_by(name) %>%
  mutate(daily = cases - lag(cases)) %>%
  ungroup() 

NYTlineDataDaily <- NYTlineData %>% 
  arrange (name, date) %>% 
  group_by(name) %>% 
  filter (daily>=10) %>% 
  mutate(days = 1 + date - date[1L]) %>%
  mutate(yvar=daily)

NYTlineDataRolling <- NYTlineData %>%
  group_by(name) %>% 
  mutate(movave=round((daily+lag(daily)+lag(daily,2)+lag(daily,3)+lag(daily,4)+lag(daily,5)+lag(daily,6))/7)) %>%
  filter (movave>=10) %>% 
  mutate(days= 1 +date - date[1L]) %>%
  mutate(yvar=movave)

#settings for plots
colorBlindPal <- c("#E69F00", "#D55E00", "#009E73", "#56B4E9", "#CC79A7", "#0072B2")   
lastday <- as.numeric(max(NYTlineDataRolling$days)+11)
xbreaks <- seq(10,lastday,by=10)
currentDate <- allDatDesc[[nrow(allDatDesc),1]]

# plot function
plotformat <- function(var1) {
  list(
    geom_line(size=1), 
    geom_point(size=0.5), 
    xlab ("\n Number of days since 10th daily cases first recorded"),
    ylab ("New Cases \n"),
    geom_text_repel(data = var1 %>% 
                      filter(days == last(days)), aes(label = name, 
                                                      x = days + 0.2, 
                                                      y = yvar, 
                                                      color = name,
                                                      fontface=2), size = 5), 
    scale_y_continuous(trans = log10_trans(),
                       breaks = c(10, 20, 50, 100, 200, 500, 1000, 2000),labels = comma),
    scale_x_continuous(breaks = xbreaks),
    annotate(geom = "text", x = 0, y = 1800, 
             label = ".", color = "#333333", size=3),
    annotate(geom = "text", x = 7.5, y = 1400, 
             label = "Miami Counties: Dade, Broward, Palm Beach\n Tampa Counties: Hillsborough, Pinellas, Pasco, Hernando 
      Orlando Counties: Orange, Seminol, Osceola, Lake\n Jacksonville Counties: Duval, Clay, St.John\n", color = "#333333", size=3),
    coord_cartesian(xlim=c(0,lastday)), 
    scale_color_manual(values=colorBlindPal),
    ggtitle("Number of new cases in Florida\n", subtitle = "Seven-day rolling average of new cases, by number of days since 10th case"),
    theme_gdocs(),
    theme(text = element_text(size=13)),
    theme(legend.position = "none"),
    theme(legend.title=element_blank()),
    labs(caption = paste0("Data Source: NYTimes   |  Last updated: ", currentDate))
  )
}
phase1 <- function() {
  list(
    annotate("segment", linetype = "solid", lwd=1,x = 52, xend = 52, y = (659-35), yend = (659+35), color = "#333333"),
    annotate("segment", linetype = "solid", lwd=1,x = 51, xend = 51, y = (296-19), yend = (296+19), color = "#333333"),
    annotate("segment", linetype = "solid", lwd=1,x = 44, xend = 44, y = (27-1.4), yend = (27+1.4), color = "#333333"),
    annotate("segment", linetype = "solid", lwd=1,x = 45, xend = 45, y = (54-2.8), yend = (54+2.8), color = "#333333"),
    annotate("segment", linetype = "solid", lwd=1,x = 43, xend = 43, y = (15-0.85), yend = (15+0.85), color = "#333333")  
  )
}



# raw case counts
NYTplotdaily <-ggplot(data = NYTlineDataDaily, aes(x=days, y=daily, colour = name)) + plotformat(NYTlineDataDaily) 

# 7 day rolling case count
NYTrolldaily <- ggplot(data = NYTlineDataRolling, aes(x=days, y=movave, color = name)) + plotformat(NYTlineDataRolling) + phase1()



#----------CHARTS & PLOTS--------------

pngFileName <- paste(currentDate," Daily Cases, NYT.png",sep="")


png(filename=pngFileName, width = 1280, height = 720)
plot(NYTrolldaily)
dev.off()

# Under Reporting

# csvFileName <- paste("Under Report ",currentDate,".csv",sep="")
# urFileName <- paste("Under Report ",currentDate,".png",sep="")

# UR_plot               #Plot of under-report
# View(reportDataFinal)   # table view of under-report

# png(filename=urFileName, width = 800, height = 600)
# plot(UR_plot)
# dev.off()

# write.csv(reportDataFinal,file=csvFileName)
