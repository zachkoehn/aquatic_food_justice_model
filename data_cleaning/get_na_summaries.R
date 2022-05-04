outdir <- "Outputs"

justice_dat<-read.csv("all_national_indicators.csv")
#justice_dat <- justice_dat %>% mutate(region = countrycode(iso3c, origin = "iso3c", destination = "region"))
justice_dat <- justice_dat %>% mutate(region = countrycode(iso3c, origin = "iso3c", destination = "region23"))

for (i in 7:ncol(justice_dat)){
  col_i <- colnames(justice_dat)[i]
  data_avail_summary <- justice_dat %>%
    select(country_name_en, !!sym(col_i), region) %>%
    mutate(data_avail = if_else(is.na(!!sym(col_i))==FALSE, true = "has data", false = "no data"))
  
  data_avail_table <- table(data_avail_summary$region, data_avail_summary$data_avail)
  data_avail_df <- data.frame(has_data = data_avail_table[,1], no_data = data_avail_table[,2]) %>%
    mutate(prop_has_data = has_data / (has_data + no_data),
           prop_no_data = no_data / (has_data + no_data), 
           region = row.names(data_avail_table))

  write.csv(data_avail_table, file = file.path(outdir, paste(col_i, "_data_availability.csv", sep = "")) , quote = FALSE)
  
}

dat_nas <- justice_dat %>%
  select(!starts_with("mean_prod")) %>%
  select(c(country_name_en, iso3c, fish_supply_daily_g_protein_percap:cultural_hegmony_0_low)) %>%  # replace to your needs
  rowwise() %>%
  summarise_all(~ sum(is.na(.)))

rowSums(dat_nas)

na_summary <- justice_dat %>%
  select(country_name_en, iso3c) %>%
  mutate(missing_vars = rowSums(dat_nas)) %>%
  arrange(desc(missing_vars))

write.csv(na_summary, file = file.path(outdir, "missing_data_summary.csv") , quote = FALSE)
