library(tidyverse)
library(survival)
library(survminer)

###function for calculating delay

delay <- function(data,new_event = T, lim){
  
if(new_event){
  
  data_1 <- data %>%
    filter(date_id == min(date_id))
  
  data <- data %>%
    filter(!(id %in% unique(data_1$id)))%>%
    group_by(id) %>%
    slice_min(order_by = timestamp_date, with_ties = F) %>%
    ungroup() %>%
    mutate(delay = as.numeric(difftime(timestamp_date, event_date, units = "days"))) %>%
    mutate(time = floor(delay/7)+1)%>%   ##just to avoid having events at t=0
    mutate(time = ifelse(time>lim, lim, time))
} else {
  data <- data %>%
    group_by(id) %>%
    slice_min(order_by = timestamp_date, with_ties = F) %>%
    ungroup() %>%
    mutate(delay = as.numeric(difftime(timestamp_date, event_date, units = "days"))) %>%
    mutate(time = floor(delay/7)+1) %>%
    mutate(time = ifelse(time>lim, lim, time))
}
  return(data)
}


### function for km curve

km_curve <- function(data, feature= "default") {
  
  df_surv <- data %>%
    mutate(status =1) 
  
  if(feature != "default"){
    df_surv$group <- df_surv[[feature]]
    km_fit <- survfit(Surv(time, status) ~ group, data = df_surv)
  } else {
    km_fit <- survfit(Surv(time, status) ~ 1, data = df_surv)
  }
  
  ggsurvplot(
    km_fit,
    data = df_surv,
    xlim = c(0, max(df_surv$time)),
    xlab = "Delays",
    ylab = "Survival Probability",
    break.time.by = 1,          # Create intervals on the x-axis (e.g., 0, 50, 100, 150, 200)
    conf.int = T,
    risk.table = F,
    ggtheme = theme_minimal(),
    palette = "lancet"
  )
}

###### COX REG

#preparing data

surv_baseline <- function(data){
  raw_data <- data %>% # data with delay
    rowwise() %>%
    do(data.frame(
      id = .$id,
      time = 1:.$time,
      event_type = .$event_type,
      region = .$region,
      country = .$country,
      sub_event_type = .$sub_event_type,
      location = .$location,
      interaction = .$interaction,
      fatalities = .$fatalities,
      event = c(rep(0, .$time -1), 1) 
    ))
  
  clog_time_data <- raw_data %>%
    group_by(time) %>%
    summarise(event = sum(event), total = n()) %>%
    mutate(hazard = event/total) 
  
  clog_time_plot <- clog_time_data %>%
    ggplot(aes(x = time, y = log(-log(1-hazard)))) +
    geom_point() +
    geom_smooth()
  
  mod <- glm(formula = event ~ time, family = binomial(link = "cloglog"), data = raw_data)
  
  raw_data$pred_hazard <- predict(mod, type = "response")
  
  event_cox_surv <- raw_data %>% #because no covariate effect
    distinct(time, pred_hazard) %>%
    mutate(surv = 0)
  
  for (i in 1:length(event_cox_surv$time)) {
    event_cox_surv$surv[i] <- prod(1-event_cox_surv$pred_hazard[1:i]) #survival function estimate
  }
  
  event_cox_surv_0 <- data_frame(time = 0, pred_hazard = 0, surv=1)
  event_cox_surv <- rbind(event_cox_surv,event_cox_surv_0)
  
  data$status <- 1
  km_fit <- survfit(Surv(time, status) ~ 1, data = data)
  
  KM_clog <- ggplot() +
    geom_step(aes(x = km_fit$time, y = km_fit$surv), color = "blue", linetype = "dashed", size = 1.2, data = data.frame(time = km_fit$time, surv = km_fit$surv)) +
    geom_step(aes(x = time, y = surv), color = "red", size = 1.2, data = event_cox_surv) +
    labs(title = "Kaplan-Meier vs Discrete-Time Cox Model",
         x = "Time",
         y = "Survival Probability") +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(limits =c(min(event_cox_surv$time), max(event_cox_surv$time)),breaks = seq(min(event_cox_surv$time), max(event_cox_surv$time), by = 10)) +
    theme(legend.position = "bottom") 
  
  res <- list(raw_data = raw_data, clog_time_data = clog_time_data, clog_time_plot = clog_time_plot, mod = mod, event_cox_surv = event_cox_surv, KM_clog = KM_clog)
  return(res)
}


message("All functions OK")






  


