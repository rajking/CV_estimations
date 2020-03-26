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



zmeanHDT <- 13
zsdHDT <- 12.7
zmedianHDT <- 9.1
muHDT <- log(zmedianHDT)
sigmaHDT <- sqrt(2*(log(zmeanHDT) - muHDT))
cCFRBaseline <- 1.38
cCFREstimateRange <- c(1.23, 1.53)
cCFRIQRRange <- c(1.3, 1.4)



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

# Get data
# allDat <- NCoVUtils::get_ecdc_cases()
allDat <- read_excel("Florida.xlsx")
allDat$cases <- as.numeric(allDat$cases)
allDat$deaths <- as.numeric(allDat$deaths)
allDat$date <- as.Date(allDat$date)

# View(allDatDesc)


allDatDesc <- allDat %>% 
  dplyr::arrange(region, date) %>% 
  dplyr::mutate(date = lubridate::ymd(date)) %>% 
  dplyr::rename(new_cases = cases, new_deaths = deaths) %>%
  dplyr::select(date, region, new_cases, new_deaths) %>%
  dplyr::filter(region != "CANADA", 
                region != "Cases_on_an_international_conveyance_Japan")

# Do analysis
allTogetherClean2 <- allDatDesc %>%
  dplyr::group_by(region) %>%
  padr::pad() %>%
  dplyr::mutate(new_cases = tidyr::replace_na(new_cases, 0),
                new_deaths = tidyr::replace_na(new_deaths, 0)) %>%
  dplyr::group_by(region) %>%
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
  dplyr::select(region, total_cases, total_deaths, underreporting_estimate, lower,
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
  dplyr::ungroup(region) %>%
  dplyr::mutate(region = region %>% stringr::str_replace_all("_", " ")) %>% 
  dplyr::mutate(underreporting_estimate_clean = paste0(underreporting_estimate*100,
                                                       "% (",lower*100,"% - ",upper*100,"%)"))




# plot results
dataTable <- reportDataFinal %>% dplyr::select(region, underreporting_estimate_clean, total_cases, total_deaths)

dataPlot <- reportDataFinal %>% 
  dplyr::mutate(
    region = region  %>% 
      factor(levels = reportDataFinal %>% 
               dplyr::arrange(desc(underreporting_estimate)) %>% 
               dplyr::pull(region) %>% 
               unique()))

#subPlotData1 <- dataPlot %>% filter(confidence == "Countries which have reported 100 or more deaths")
#subPlotData2 <- dataPlot %>% filter(confidence == "Countries that have reported fewer than 100 deaths, but more than 10")
#subPlotData3 <- dataPlot %>% filter(confidence == "Countries that have reported fewer than or equal to 10 deaths")

plot <- dataPlot %>% 
  ggplot2::ggplot(ggplot2::aes(x = region)) +
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
  ggplot2::coord_flip()

plot


View(reportDataFinal)
















