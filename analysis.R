library(googledrive)
library(httr)

source("delay_KM_function.R")

file_id <- "1yNhkChNGC-NGhtS4qHxCVm6-K2_sVGoQ" 
temp_file <- tempfile(fileext = ".rds")
drive_download(as_id(file_id), path = temp_file, overwrite = TRUE)

df <- readRDS(temp_file)

df <- df %>% #kick Som out 
  filter(country != "Somalia")

data <- delay(df, lim = 20) ##calculating delay, truncating at week 20

data <- data %>%
  mutate(fat = ifelse(fatalities == 0, "No fat", "Fat"))

km_curve(data, "event_type") # arg : data with delay AND the group
km_curve(data, "region")
km_curve(data, "fat")

###Effect of fatalities on delay per event type

data_no_fat <- data[data$fat == "No fat", ]
data_fat <- setdiff(data, data_no_fat)

km_curve(data_no_fat, "event_type") ##compare this with the plot at line 13
km_curve(data_fat, "event_type"). ##uncertainty here


###Effect fat on delay region

km_curve(data_no_fat, "region") ##compare this with the plot at line 14
km_curve(data_fat, "region"). ## uncertainty here

###baseline cox

base_cox <- surv_baseline(data)
base_cox$clog_time_plot ##1 removed in smoothing because of truncation
base_cox$KM_clog
summary(base_cox$mod)

