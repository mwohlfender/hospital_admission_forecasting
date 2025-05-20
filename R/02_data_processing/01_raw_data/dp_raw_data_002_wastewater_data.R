
# process wastewater data
# data source: https://sensors-eawag.ch/sars/laupen.html
# missing values of the viral RNA measurements in wastewater are interpolated using interpolation

# read raw data
data_wastewater_raw <- read_delim(file = paste0(Directory_Data, Path_Data_Wastewater_Raw), delim = ";")


# process data: `data_wastewater_raw`
# (a) change names of columns
# (b) filter to rows with a value different to NA in the column `quantification_flag`
# (c) add column `sars_cov2_rna_normalized`: column `sars_cov2_rna` divided by column `flow`
data_wastewater_processed <- data_wastewater_raw %>%
  dplyr::rename(c("date_day" = "...1",
                  "sars_cov2_rna" = "sars_cov2_rna [gc/(d*100000 capita)]",
                  "median_7d_sars_cov2_rna" = "median_7d_sars_cov2_rna [gc/(d*100000 capita)]",
                  "new_cases" = "new_cases [1/(d*100000 capita)]",
                  "median_7d_new_cases" = "median_7d_new_cases [1/(d*100000 capita)]",
                  "quantification_flag" = "quantification_flag [{Q: >LOQ,D: >LOD,N: <LOD}]",
                  "flow" = "flow [m^3/d]")) %>%
  mutate(sars_cov2_rna_normalized = sars_cov2_rna / flow) %>%
  filter(!(is.na(quantification_flag)))



# make sure there is a row in `data_wastewater_processed` for each day of the partial study period
sequence_days <- seq(from = ymd("2021-11-10"), to = ymd("2023-06-30"), by = "days")

data_wastewater_processed <- tibble(date_day = sequence_days) %>%
  left_join(data_wastewater_processed, by = "date_day")


# interpolate missing values
data_wastewater_zoo <- zoo(x = data_wastewater_processed$sars_cov2_rna,
                           order.by = data_wastewater_processed$date_day)

data_wastewater_approx <- tibble(fortify.zoo(na.approx(data_wastewater_zoo))) %>%
  dplyr::rename(c("date_day" = "Index", "sars_cov2_rna" = "na.approx(data_wastewater_zoo)"))


data_wastewater_normalized_zoo <- zoo(x = data_wastewater_processed$sars_cov2_rna_normalized,
                                      order.by = data_wastewater_processed$date_day)

data_wastewater_normalized_approx <- tibble(fortify.zoo(na.approx(data_wastewater_normalized_zoo))) %>%
  dplyr::rename(c("date_day" = "Index", "sars_cov2_rna_normalized" = "na.approx(data_wastewater_normalized_zoo)"))


data_wastewater_processed_approx <- data_wastewater_approx %>%
  left_join(data_wastewater_normalized_approx, by = "date_day")


# round column `sars_cov2_rna_normalized` of `data_wastewater_processed` to three digits after comma
for (ii in 1:nrow(data_wastewater_processed_approx)) {
  
  if (!(is.na(data_wastewater_processed_approx$sars_cov2_rna_normalized[ii]))) {
    
    data_wastewater_processed_approx$sars_cov2_rna_normalized[ii] <- custom_round(x = data_wastewater_processed_approx$sars_cov2_rna_normalized[ii],
                                                                                  accuracy = 0.001)
    
  }
  
}


# save `data_wastewater_processed_approx` as csv file
write_csv(x = data_wastewater_processed_approx,
          file = paste0(Directory_Data, Path_Data_Wastewater_Processed))


# remove `data_wastewater_raw`, `data_wastewater_processed`, `sequence_days`, `data_wastewater_zoo`, `data_wastewater_approx`,
# `data_wastewater_normalized_zoo`, `data_wastewater_normalized_approx` and `data_wastewater_processed_approx` from the environment
remove(data_wastewater_raw, data_wastewater_processed, sequence_days, data_wastewater_zoo, data_wastewater_approx,
       data_wastewater_normalized_zoo, data_wastewater_normalized_approx, data_wastewater_processed_approx)


