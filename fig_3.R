################################################################################
# 
#  Figure 3 Multi-panel policy plot 
# 
################################################################################


################################################################################
#  Set up
#_______________________________________________________________________________
# load packages
library(tidyverse)
library(viridis)
library(ggplot2)
library(ggpubr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(cowplot)

# directory
directory <- "/Volumes/GoogleDrive/My Drive/BFA_Papers/BFA_Justice/section_model/aquatic_food_justice_model"

raw_dat <- read.csv(
  file.path(
    directory,
    "policy_nvivo_keyword_extract.csv"
  ),
  header=TRUE
)


################################################################################
#  Keyword summaries
#_______________________________________________________________________________


# vector of territories to exclude from the plots
territories <-c(
  "VIR", #Virgin Islands United States
  "UMI", #Minor outlying islands United States
  "PRI", #Puerto Rico United States
  "MNP", #N Mariana islands United States
  "GUM", #Guam United States
  "ASM", #American Samoa United States
  "VGB", #Virgin islands UK
  "TCA", #Turks and Caicos UK
  "SGS", #South Georgia and South Sandwich Islands UK
  "SHN", # Saint Helena, Ascension and Tristan da Cunha UK
  "PCN", #Pitcairn UK
  "MSR", #Montserrat UK
  "GIB", #Gibraltar UK
  "FLK", #Falkland Islands UK
  "CYM", #Cayman Islands UK
  "IOT", #British Indian Ocean Territory UK
  "BMU", #Bermuda UK
  "AIA", #Anguilla UK
  "SJM", #Svalbard and Jan Mayen Norway
  "BVT",#Bouvet Island Norway
  "TKL", #Tokelau New Zealand
  "NIU", #Niue New Zealand
  "COK", #Cook Islands New Zealand
  "SXM", #Sint Maarten Netherlands
  "CUW", #Curacao Netherlands
  "BES", #Bonaire, Sint Eustatius and Saba Netherlands
  "ABW", #Aruba Netherlands
  "WLF", #Wallis and Fortuna France
  "SPM", #Saint Pierre and Miquelon France
  "MAF", #Saint Martin France
  "BLM", #Saint Barthelemy France
  "REU", #Reunion France
  "NCL", #New Caledonia France
  "MYT", #Mayotte France
  "MTQ", #Martinique France
  "GLP", #Guadeloupe France
  "ATF", #French Southern Territories
  "PYF", #French Polynesia France
  "GUF", #French Guiana France
  "ALA", #Aland Finland
  "GRL", #Greenland Denmark
  "FRO", #Faroe Islands Denmark
  "MAC", #Macao China
  "HKG", #Hong Kong China
  "JEY", #Jersey UK
  "IMN", #Isle of Man UK
  "GGY", #Guernsey UK
  "NFK", #Norfolk Island Australia
  "HMD", #Heard and McDonald Islands Australia
  "CCK", #Cocos Islands Australia
  "CXR", #Christmas island Australia
  "VAT",  #Vatican City
  "ESH", #Western Sahara
  "ATA"   #Antarctica
)


keyword_dat <- raw_dat %>%
  filter(
    !(iso_c3 %in% territories),
    !is.na(iso_c3),
    !is.na(keyword_category)
  ) %>%
  select(ID, Country,iso_c3,Region, Subregion, Year,System,Type,Language,
         keyword_category,keywords,Pages,count) %>%
  mutate(
    barrier=case_when(
      keyword_category %in% c("wealth","economic_safety_net","distribution") ~ "economic",
      keyword_category %in% c("gender","age","access_to_health_nutrition") ~ "social",
      keyword_category %in% c("rights_based","representation","physical_access") ~ "political"
    )
  ) %>%
  filter(!is.na(barrier)) %>%
  group_by(ID, Country,iso_c3,Region, Subregion, 
           Year, Pages,System,Type,Language,barrier,
           keyword_category) %>%
  summarize(
    keyword_category_sum = sum(count)
  ) %>%
  mutate(
    count_per_page=keyword_category_sum/Pages
  ) 



# create dataframe for 
keyword_subr_plot <- keyword_dat %>%
  mutate(
    Subregion=str_replace_all(Subregion,"Northern Africa","N.Af"),
    Subregion=str_replace_all(Subregion,"Central Africa","C.Af"),
    Subregion=str_replace_all(Subregion,"Western Africa","W.Af"),
    Subregion=str_replace_all(Subregion,"Eastern Africa","E.Af"),
    Subregion=str_replace_all(Subregion,"Southern Africa","S.Af"),
    Subregion=str_replace_all(Subregion,"Central Asia","C.As"),
    Subregion=str_replace_all(Subregion,"Western Asia","W.As"),
    Subregion=str_replace_all(Subregion,"Eastern Asia","E.As"),
    Subregion=str_replace_all(Subregion,"Southern Asia","S.As"),
    Subregion=str_replace_all(Subregion,"Southeastern Asia","SE.As"),
    Subregion=str_replace_all(Subregion,"Northern Europe","N.Eu"),
    Subregion=str_replace_all(Subregion,"Western Europe","W.Eu"),
    Subregion=str_replace_all(Subregion,"Eastern Europe","E.Eu"),
    Subregion=str_replace_all(Subregion,"Southern Europe","S.Eu"),
    Subregion=str_replace_all(Subregion,"Australia and New Zealand","AusNZ"),
    Subregion=str_replace_all(Subregion,"South America","S.Am"),
    Subregion=str_replace_all(Subregion,"Northern America","N.Am"),
    Subregion=str_replace_all(Subregion,"Central America","C.Am"),
    Subregion=str_replace_all(Subregion,"Micronesia","Micr"),
    Subregion=str_replace_all(Subregion,"Melanesia","Mela"),
    Subregion=str_replace_all(Subregion,"Polynesia","Poly"),
    Subregion=str_replace_all(Subregion,"Caribbean","Carrib")
  )

# function to summarize keywords for different barrier groups
keyword_summaries <- function(keyword_dat,keyword_group) {
  # to test
  # keyword_dat=keyword_dat_all_langs
  # keyword_group="age"
  # 
  national_policy_dat <- keyword_dat %>%
    filter(
      # System=="Consumption",
      barrier %in% keyword_group
    ) %>%
    group_by(Region,Subregion) %>%
    summarize(
      # mean_count=mean(count,na.rm=TRUE),
      # mean_pages=mean(Pages,na.rm=TRUE),
      mean_count_page=mean(count_per_page)
    ) %>%
    mutate(
      # count_page = mean_count/mean_pages,
      quantile = ifelse(mean_count_page>0,ntile(mean_count_page,5),0),
      quantile = factor(quantile)
    ) 
  
  return(national_policy_dat)
}
# summarize keyword usage for econmic, social and poitical barriers
economic_barrier_means <- keyword_summaries(keyword_subr_plot,keyword_group=c("economic"))
social_barrier_means <- keyword_summaries(keyword_subr_plot,keyword_group=c("social"))
political_barrier_means <- keyword_summaries(keyword_subr_plot,keyword_group=c("political"))


################################################################################
#  Polar plot for multipanel
#_______________________________________________________________________________


# function to create polar plots used in figure 3 multipanel
polar_plot <- function(social_difference_means) {
  # social_difference_means = economic_dist_means
  # Set a number of 'empty bar' to add at the end of each group
  empty_bar <- 6
  to_add <- data.frame( matrix(NA, empty_bar*nlevels(social_difference_means$Subregion), ncol(social_difference_means)) )
  colnames(to_add) <- colnames(social_difference_means)
  to_add$Region <- rep(levels(social_difference_means$Subregion), each=empty_bar)
  # social_difference_means <- rbind(social_difference_means, to_add)
  social_difference_means <- social_difference_means %>% arrange(Region)
  social_difference_means$id <- seq(1, nrow(social_difference_means))
  
  # Get the name and the y position of each label
  label_data <- social_difference_means
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust <- ifelse( angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle+180, angle)
  
  theme_petal <- theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    # plot.margin = unit(rep(-1,4), "cm"),
    panel.grid = element_blank(),
    legend.position = "none"
  )
  
  # regional colors 
  region_colors <- c(
    "Africa"= "#364F6B",#"#FC5185",
    "Caribbean"= "#ffb465",#"#FFA647",
    "Asia"="#7b5395",#"#70468C",
    "Oceania"="#69cbd1",#"#3FC1C9",
    "Europe"="#ffe070",#"#FFD947",
    "Americas"="#79d996"#"#57D182"
  )
  
  # Make the plot
  polar_plot <- ggplot(social_difference_means, aes(x=as.factor(id), y=mean_count_page, fill=Region)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
    geom_bar(stat="identity", alpha=1) +
    # ylim(-100,120) +
    theme_minimal() +
    theme_petal +
    coord_polar() + 
    geom_text(
      data=label_data, 
      aes(x=id, y=mean_count_page, 
          label=Subregion, hjust="outward"), color="black", fontface="bold",
      alpha=0.8, size=1.75, angle= label_data$angle, inherit.aes = FALSE 
    ) +
    ylim(c(-.6,1.5)) +
    # scale_y_continuous(expand = c(0.00, .28)) +
    scale_fill_manual(values=region_colors) 
  
  print(polar_plot)
}

# run funciton 
social_polar_plot <- polar_plot(social_difference_means=social_barrier_means)
political_polar_plot <- polar_plot(social_difference_means=political_barrier_means)
econ_polar_plot <- polar_plot(social_difference_means=economic_barrier_means)

################################################################################
#  Map for multipanel
#_______________________________________________________________________________

# load natural earth national file
world <- ne_countries

map_palette_social_diffs <- colorRampPalette(c("#FFFBFC","#fb1f62"))
map_palette_access <- colorRampPalette(c("#FBFBFC","#05182E"))


plot_keywords <- function(keyword_group,map_palette) {
  
  # 
  # 	keyword_group=c(
  # 			"political","social","economic"
  # 			)
  # map_palette=colorRampPalette(c("#FBFBFC","#05182E"))
  
  national_policy_dat <- keyword_dat %>%
    filter(
      # System=="Consumption",
      barrier %in% keyword_group
    ) %>%
    select(ID,Country,iso_c3,
           keyword_category,count_per_page) %>%
    group_by(
      ID, Country,iso_c3,Region, Subregion, Year, Pages,System,Type,Language
    ) %>%
    summarize(
      count_all_categories = mean(count_per_page)
    ) %>%
    group_by(iso_c3) %>%
    summarize(
      # mean_count=mean(count,na.rm=TRUE),
      # mean_pages=mean(Pages,na.rm=TRUE),
      mean_count_page=mean(count_all_categories)
    ) %>%
    mutate(
      # count_page = mean_count/mean_pages,
      quantile = ifelse(mean_count_page>0,ntile(mean_count_page,4),0),
      quantile = factor(quantile)
    ) 
  # load data
  PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
  map.world <- ne_countries(scale = "medium", returnclass = "sf") 
  
  policy_world <- left_join(map.world, national_policy_dat, by = c("iso_a3" = "iso_c3")) %>%
    st_transform(PROJ) %>%
    filter(continent!="Antarctica")
  
  small_islands <- c("NRU","FSM" ,"PLW" ,"MHL", "KIR", "WSM", "TON", "TUV")
  
  # Now extract island countries in Micronesia and Polynesia  to cast as points
  # dot-based solution for smaller island countries, based on script from Juliano Palacios Abrantes 
  map.islands <- policy_world %>% 
    filter(iso_a3 %in% small_islands) %>%
    st_cast("POINT") # transform country polygons into points
  unique(map.islands$iso_a3)
  # use alternative projection that preserves area a bit better than base projection
  map.not.islands <- policy_world %>% 
    filter(!iso_a3 %in% small_islands)
  
  st_crs(map.islands)==st_crs(map.not.islands)
  
  # bind together island points and large country polygons in single SF dataframe
  map.world.poly.points <- rbind(map.not.islands,map.islands)
  
  
  keyword_world_plot <- map.world.poly.points %>%
    ggplot() +
    geom_sf(aes(fill=quantile,color=quantile),size=0.1) +
    geom_sf(data=map.not.islands,color="grey40",fill=NA,size=.1) +
    scale_fill_manual(
      name="References/page \n(by percentile)",
      values = map_palette(6)[2:6],
      labels=c(
        "1-25%","26-50%","51-75%","76-100%","No data"),
      na.value="#e4ddcf"
    ) + 
    scale_color_manual(
      name="References/page \n(by percentile)",
      values = map_palette(6)[2:6],
      labels=c(
        "1-25%","26-50%","51-75%","76-100%","No data"),
      na.value="#e4ddcf"
    ) + 
    xlab(NULL)
  
  return(keyword_world_plot)
}

theme_maps <- theme(
  plot.title = element_blank(),
  # plot.title = element_text(
  # 	family = "Helvetica Neue",face="bold",
  # 	size=10,hjust=0.5
  # 	),
  # # plot.subtitle = element_text(size=8,hjust=0.5,family = "Futura Medium"),
  axis.text.x = element_blank(),  # remove the background, tickmarks, etc
  axis.text.y = element_blank(),  # remove the background, tickmarks, etc
  # legend.position = "none",
  legend.text = element_text(
    # family="Helvetica Neue",
    size=6
  ),
  legend.title = element_text(
    # family="Helvetica Neue",
    size=8
  ),
  line = element_blank(),  # remove the background, tickmarks, etc
  panel.background = element_blank()
)

unique(keyword_dat$barrier)

social_difference_plot <- plot_keywords(
  keyword_group=c(
    "political","social","economic"
  ),
  map_palette=map_palette_social_diffs)

social_difference_map <- social_difference_plot +theme_maps

social_difference_map


# load natural earth national file
world <- ne_countries

map_palette_social_diffs <- colorRampPalette(c("#FFFBFC","#fb1f62"))
map_palette_access <- colorRampPalette(c("#FBFBFC","#05182E"))


plot_keywords <- function(keyword_group,map_palette) {
  
  # 
  # 	keyword_group=c(
  # 			"political","social","economic"
  # 			)
  # map_palette=colorRampPalette(c("#FBFBFC","#05182E"))
  
  national_policy_dat <- keyword_dat %>%
    filter(
      # System=="Consumption",
      barrier %in% keyword_group
    ) %>%
    select(ID,Country,iso_c3,
           keyword_category,count_per_page) %>%
    group_by(
      ID, Country,iso_c3,Region, Subregion, Year, Pages,System,Type,Language
    ) %>%
    summarize(
      count_all_categories = mean(count_per_page)
    ) %>%
    group_by(iso_c3) %>%
    summarize(
      # mean_count=mean(count,na.rm=TRUE),
      # mean_pages=mean(Pages,na.rm=TRUE),
      mean_count_page=mean(count_all_categories)
    ) %>%
    mutate(
      # count_page = mean_count/mean_pages,
      quantile = ifelse(mean_count_page>0,ntile(mean_count_page,4),0),
      quantile = factor(quantile)
    ) 
  # load data
  PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
  map.world <- ne_countries(scale = "medium", returnclass = "sf") 
  
  policy_world <- left_join(map.world, national_policy_dat, by = c("iso_a3" = "iso_c3")) %>%
    st_transform(PROJ) %>%
    filter(continent!="Antarctica")
  
  small_islands <- c("NRU","FSM" ,"PLW" ,"MHL", "KIR", "WSM", "TON", "TUV")
  
  # Now extract island countries in Micronesia and Polynesia  to cast as points
  # dot-based solution for smaller island countries, based on script from Juliano Palacios Abrantes 
  map.islands <- policy_world %>% 
    filter(iso_a3 %in% small_islands) %>%
    st_cast("POINT") # transform country polygons into points
  unique(map.islands$iso_a3)
  # use alternative projection that preserves area a bit better than base projection
  map.not.islands <- policy_world %>% 
    filter(!iso_a3 %in% small_islands)
  
  st_crs(map.islands)==st_crs(map.not.islands)
  
  # bind together island points and large country polygons in single SF dataframe
  map.world.poly.points <- rbind(map.not.islands,map.islands)
  
  
  keyword_world_plot <- map.world.poly.points %>%
    ggplot() +
    geom_sf(aes(fill=quantile,color=quantile),size=0.1) +
    geom_sf(data=map.not.islands,color="grey40",fill=NA,size=.1) +
    scale_fill_manual(
      name="References/page \n(by percentile)",
      values = map_palette(6)[2:6],
      labels=c(
        "1-25%","26-50%","51-75%","76-100%","No data"),
      na.value="#e4ddcf"
    ) + 
    scale_color_manual(
      name="References/page \n(by percentile)",
      values = map_palette(6)[2:6],
      labels=c(
        "1-25%","26-50%","51-75%","76-100%","No data"),
      na.value="#e4ddcf"
    ) + 
    xlab(NULL)
  
  return(keyword_world_plot)
}

theme_maps <- theme(
  plot.title = element_blank(),
  # plot.title = element_text(
  # 	family = "Helvetica Neue",face="bold",
  # 	size=10,hjust=0.5
  # 	),
  # # plot.subtitle = element_text(size=8,hjust=0.5,family = "Futura Medium"),
  axis.text.x = element_blank(),  # remove the background, tickmarks, etc
  axis.text.y = element_blank(),  # remove the background, tickmarks, etc
  # legend.position = "none",
  legend.text = element_text(
    # family="Helvetica Neue",
    size=6
  ),
  legend.title = element_text(
    # family="Helvetica Neue",
    size=8
  ),
  line = element_blank(),  # remove the background, tickmarks, etc
  panel.background = element_blank()
)

unique(keyword_dat$barrier)

social_difference_plot <- plot_keywords(
  keyword_group=c(
    "political","social","economic"
  ),
  map_palette=map_palette_social_diffs)

social_difference_map <- social_difference_plot +theme_maps

social_difference_map


################################################################################
#  Keyword summaries
#_______________________________________________________________________________

# clean maps and polar plots for multipanel 
map_social_diff <- social_difference_map +
  # ylim(c(-90,90)) + 
  theme(
    legend.position = c(0.15,-0.1), 
    legend.justification = "left",
    legend.direction = "horizontal",
    legend.text=element_text(size=8)
  )



social_polar_plot <- social_polar_plot+annotate(geom="text",label="Social",x=0,y=-.6,size=6/.pt,fontface="bold")
political_polar_plot <- political_polar_plot+annotate(geom="text",label="Political",x=0,y=-.6,size=6/.pt,fontface="bold")
econ_polar_plot <- econ_polar_plot+annotate(geom="text",label="Economic",x=0,y=-.6,size=6/.pt,fontface="bold")




multipanel <- ggdraw() +
  draw_plot(
    map_social_diff,
    x=0,y=.5,hjust = 0,vjust=0.5,height = .87,width=.87
  ) +
  draw_plot(
    econ_polar_plot,
    x=.9,y = .67,height = .36,width = .36,hjust = 0.5,vjust=0.5
  ) +
  draw_plot(
    political_polar_plot,
    x=.9,y = .47,height = .36,width = .36,hjust = 0.5,vjust=0.5
  ) +
  draw_plot(
    social_polar_plot,
    x=.9,y = .25,height = .36,width = .36,hjust = 0.5,vjust=0.5
  )


# ggsave(
#   multipanel,
#   file=file.path(
#     directory,
#     "NVivo",
#     "r_outputs",
#     "figures",
#     "figure_3_multipanel.pdf"
#   ),
#   height = 7,
#   width =8.5
# )
