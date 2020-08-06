#######################
# Cleaning ILO gender income gap data
# Zach Koehn
# zkoehn@stanford.edu
#######################

# load dataset and packages
library(tidyverse)
library(here)
library(countrycode)

# data from https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD?view=map
dat_raw <- read.csv(
  here(
    "data",
    "data_raw",
    "ilo_wage_gender",
    "ilostat-2020-07-24.csv"
  ),
  header=TRUE
)

str(dat_raw)

dat_wage <- dat_raw %>%
  filter(
    sex.label %in% c("Sex: Male","Sex: Female"), # remove total
    classif2.label == "Currency: 2017 PPP $", #set wage type to standardized measure
    classif1.label %in% c("Economic activity (Aggregate): Total","Economic activity (ISIC-Rev.3.1): B. Fishing"), # select the two sectors (total and fishing)
    time>2005,time<2017 # set time period
         ) %>%
  select(ref_area.label,sex.label,classif1.label,time,obs_value) %>%
  # pivots to wider data frame in order to calculate gender wage gaps
  pivot_wider(names_from = c(classif1.label,"sex.label"),values_from="obs_value") %>% #
  # simplify variable names
  rename(
    wage_total_female="Economic activity (Aggregate): Total_Sex: Female",
    wage_total_male="Economic activity (Aggregate): Total_Sex: Male",
    wage_fish_female="Economic activity (ISIC-Rev.3.1): B. Fishing_Sex: Female",
    wage_fish_male="Economic activity (ISIC-Rev.3.1): B. Fishing_Sex: Male",
    geog=ref_area.label
    ) %>%
  # compute wage gap across all sectors and for fishing
  mutate( 
    gender_wage_gap_total_annual=wage_total_female/wage_total_male,
    gender_wage_gap_fish_annual=wage_fish_female/wage_fish_male
    ) %>%
  # calculates the means rs across all years for each country
  group_by(geog) %>%
  summarize(
    mean_wage_gap_all_sectors=mean(gender_wage_gap_total_annual,na.rm=TRUE),
    mean_wage_gap_fishing_sector=mean(gender_wage_gap_fish_annual,na.rm=TRUE),
    )

# add country names and ISO codes
dat_total_employment_final <- dat_wage %>%
  mutate(
    iso3c=countrycode(geog,"country.name","iso3c"), # assign iso3c from country name
    iso3n=countrycode(iso3c,"iso3c","iso3n"), # assign iso3n from iso3c
    year_range="2006-2016" #add the year_range category
  ) %>%
  select(geog,iso3c,iso3n,mean_wage_gap_all_sectors,mean_wage_gap_fishing_sector,year_range) #only select variable 

# and write the csv 
write.csv(
  dat_total_employment_final,
  here(
    "data",
    "data_clean",
    "gender_wage_gap_mean_2006-2016.csv"
  ),
  row.names = FALSE
)

########################
# # out of curiousity, viz of how the fishing sector compares to total gender age
########################
# require(ggrepel)
# my_theme <- function() {
# 
#   # Colors
#   color.background = "white"
#   color.text = "#22211d"
# 
#   # Begin construction of chart
#   theme_bw(base_size=15) +
# 
#     # Format background colors
#     theme(panel.background = element_rect(fill=color.background, color=color.background)) +
#     theme(plot.background  = element_rect(fill=color.background, color=color.background)) +
#     theme(panel.border     = element_rect(color=color.background)) +
#     theme(strip.background = element_rect(fill=color.background, color=color.background)) +
# 
#     # Format the grid
#     theme(panel.grid.major.y = element_blank()) +
#     theme(panel.grid.minor.y = element_blank()) +
#     theme(axis.ticks       = element_blank()) +
# 
#     # Format the legend
#     theme(legend.position = "none") +
# 
#     # Format title and axis labels
#     theme(plot.title       = element_text(color=color.text, size=20, face = "bold")) +
#     theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
#     theme(axis.title.y     = element_text(size=14, color="black", face = "bold", vjust=1.25)) +
#     theme(axis.text.x      = element_text(size=10, vjust=0.5, hjust=0.5, color = color.text)) +
#     theme(axis.text.y      = element_text(size=10, color = color.text)) +
#     theme(strip.text       = element_text(face = "bold")) +
# 
#     # Plot margins
#     theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
# }
# wage_comparison <- dat_total_employment_final[complete.cases(dat_total_employment_final),] %>%
#   pivot_longer(
#     cols=c(mean_wage_gap_all_sectors,mean_wage_gap_fishing_sector),
#     names_to="sector"
#   ) %>%
#   filter(value>0) %>%
#   mutate(sector=fct_relevel(sector,"mean_wage_gap_fishing_sector","mean_wage_gap_all_sectors"))
# 
# 
# wage_comparison %>%
#   ggplot(aes(x=sector,y=value,group=geog))+
#   geom_line(aes(color = geog),  alpha = .75, size = 2) +
#   geom_point(aes(color = geog),alpha = .75, size = 4) +
#   geom_text_repel(
#     data = wage_comparison %>% filter(sector == "mean_wage_gap_fishing_sector"),
#             aes(label = geog, x = .95) , 
#     hjust = .85, fontface = "bold", color = "#888888", segment.size = .2,
#     direction = "y",nudge_x = -0.2) +
#   geom_text_repel(
#     data = wage_comparison %>% filter(sector == "mean_wage_gap_all_sectors"),
#             aes(label = geog,color=geog, x = 2.05) , 
#     hjust = 0.15, fontface = "bold", color = "#888888", segment.size = .2,
#     direction = "y",nudge_x = 0.2) +
#   labs(
#     x="sector",
#     y="Wage ratio of women's to men's",
#     title="Gender wage gap",
#     subtitle="Fishing sector compared to total"
#   ) +
#   scale_x_discrete(labels=c("Fishing sector", "All sectors")) +
#   my_theme()




