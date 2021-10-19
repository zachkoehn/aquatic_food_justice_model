# FBS Plots for Demand paper

library(tidyverse)
library(countrycode)
library(ggthemes)

# First load function for rebuilding from FishStat ZIP file:
rm(list=ls())

# note: Change outdir to the filepath where you want outputs to go
outdir <- "Outputs"
# for MacOS
datadir <- "/Volumes/jgephart/FishStatR/Data/FoodBalanceSheets"

df_new <- read.csv(file.path(datadir, "FoodBalanceSheets_E_All_Data", "FoodBalanceSheets_E_All_Data.csv"))
#df_hist <- read.csv(file.path(datadir, "FoodBalanceSheetsHistoric_E_All_Data", "FoodBalanceSheetsHistoric_E_All_Data.csv"))

# Filter to countries and blue foods 
# (currently excluding "Aquatic Products, Other", "Aquatic Animals, Others", "Aquatic Plants", "Meat, Aquatic Mammals"))
df_new <- df_new %>%
  #filter(Element %in% c("Protein supply quantity (g/capita/day)", "Import Quantity", "Export Quantity"), Area.Code < 1000) %>%
  filter(Item %in% c("Freshwater Fish", "Demersal Fish", "Pelagic Fish", "Marine Fish, Other", "Crustaceans", 
                     "Cephalopods", "Molluscs, Other")) %>%
  select(-ends_with("F")) %>% 
  pivot_longer(Y2014:Y2017, names_to = "year") %>%
  mutate(year = as.numeric(gsub("Y", "", year)))

#df_hist <- df_hist %>%
#  filter(Element %in% c("Protein supply quantity (g/capita/day)", "Import Quantity", "Export Quantity"), Area.Code < 1000) %>%
#  filter(Item %in% c("Freshwater Fish", "Demersal Fish", "Pelagic Fish", "Marine Fish, Other", "Crustaceans", 
#                     "Cephalopods", "Molluscs, Other")) %>%
#  select(-ends_with("F")) %>% 
#  pivot_longer(Y1961:Y2013, names_to = "year") %>%
#  mutate(year = as.numeric(gsub("Y", "", year)))

df <- df_new %>%
  # Excluding for now when we don't need the historical data
  #bind_rows(df_new, df_hist) %>%
  pivot_wider(names_from = Element, values_from = value, names_repair = "universal", values_fill = 0) %>%
  mutate(iso3c = countrycode(Area, origin = "country.name", destination = "iso3c"),
         iso3n = countrycode(Area, origin = "country.name", destination = "iso3n")) %>%
  filter(!(Area == "China")) %>% # Only include China, mainland because territories report separate data 
  mutate(net_import = Import.Quantity - Export.Quantity,
         net_export = Export.Quantity - Import.Quantity)

# Clean codes
df$iso3c[df$Area == "Eswatini"] <- "SWZ"
df$iso3n[df$Area == "Eswatini"] <- "748"

# Plot net import for China, India, Ghana, Nigeria, Peru, Brazil, U.S., Mexico, Spain, and France
df_plot <- df %>%
  filter(iso3c %in% c("CHN", "IND", "GHA", "PER", "NGA", "BRA", "USA", "MEX", "ESP", "FRA"), 
         year == 2017)
df_plot$iso3c <- factor(df_plot$iso3c, levels = c("CHN", "IND", "GHA", "NGA", "PER",  "BRA", "MEX", "USA", "ESP", "FRA"))
df_plot$Item <- factor(df_plot$Item, levels = c("Demersal Fish", "Pelagic Fish", "Marine Fish, Other", "Freshwater Fish", 
                                                 "Crustaceans", "Cephalopods", "Molluscs, Other"))

df_total <- df_plot %>%
  group_by(Area, iso3c) %>%
  summarise(Import.Quantity = sum(Import.Quantity),
            Export.Quantity = sum(Export.Quantity)) %>%
  mutate(net_import = Import.Quantity - Export.Quantity,
         net_export = Export.Quantity - Import.Quantity,
         Item = "Total") 

ggplot() +
  geom_bar(data = df_plot, aes(x = iso3c, y = Import.Quantity/1000, fill = Item), stat = "Identity") + 
  geom_bar(data = df_plot, aes(x = iso3c, y = -1*Export.Quantity/1000, fill = Item), stat = "Identity") +
  # Add net import lollipop
  geom_segment(data = df_total, aes(x= iso3c, xend= iso3c, y=0, yend= net_import/1000)) +
  geom_point(data = df_total, aes(x = iso3c, y = net_import/1000), 
             size=2, fill = "grey80", alpha=0.7) +
  geom_hline(yintercept = 0, size = 1) +
  scale_fill_manual(values = c("#364F6B", "#3FC1C9", "#57D182", "#A9D158", "#FFD947", "#FFA647", "#C93F3F")) +
  labs(x = "", y = "Trade volume (million t)") +
  theme_clean() 

# Plot multi panel
df_plot_multi <- df_plot %>%
  bind_rows(df_total) %>%
  pivot_longer(Import.Quantity:Export.Quantity) %>% 
  filter(name != "Protein.supply.quantity..g.capita.day.")
  
df_plot_multi$Item <- factor(df_plot_multi$Item, levels = c("Total", "Demersal Fish", "Pelagic Fish", "Marine Fish, Other", "Freshwater Fish", 
                                                   "Crustaceans", "Cephalopods", "Molluscs, Other"))

df_plot_multi$name[df_plot_multi$name == "Export.Quantity"] <- "Export"
df_plot_multi$name[df_plot_multi$name == "Import.Quantity"] <- "Import"

ggplot(df_plot_multi,
       aes(x = iso3c, y = value/1000, fill = name)) +
  geom_bar(position = "dodge", stat= "identity") +
  facet_wrap(~Item) +
  scale_fill_manual(values = c("#364F6B", "#FFA647")) +
  labs(fill = "", y = "Trade volume (million t live weight)", x = "") +
  theme_clean() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90))




