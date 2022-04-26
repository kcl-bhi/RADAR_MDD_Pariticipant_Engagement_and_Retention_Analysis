rm(list=ls())
library("install.load")
install_load("tidyverse", "data.table", "lubridate")

# Data DIR 
DATA_DIR <- "D:/data/MDD_engagement/prepared_data/"

enrollment.info <- fread(paste0(DATA_DIR,"list_of_all_participants.csv"), data.table=F) %>%
  dplyr::mutate(enrollment_date = lubridate::ymd_hms(enrolment_date),
                withdrawal_date = lubridate::ymd_hms(withdrawal_date)) %>%
  dplyr::transmute(p_id = subject_id,
                   enrollment_date,withdrawal_date) %>%
  dplyr::mutate(delta.enrol.vs.study_start = as.numeric(as.Date(enrollment_date) - as.Date("2017-11-30 UTC")))

active.data <- fread(paste0(DATA_DIR,"active_data_summary_table.csv"), data.table=F) %>%
  dplyr::mutate(timestamp = lubridate::as_datetime(timestamp))

passive.data <- fread(paste0(DATA_DIR,"passive_data_summary_table.csv"), data.table=F) %>%
  dplyr::mutate(timestamp = lubridate::as_datetime(timestamp))

clin.assessment.data <- fread(paste0(DATA_DIR,"assessment_data_summary_table.csv"), data.table=F) %>%
  dplyr::mutate(timestamp = lubridate::as_datetime(timestamp))

demog.data <- fread(paste0(DATA_DIR,"demographics_harmonized.csv"), data.table=F) %>%
  select(-dob)

contact.log.data <- fread(paste0(DATA_DIR,"Cont_log.csv"), data.table=F) %>%
  select(-comms,-site)%>%
  dplyr::mutate(date = as.Date(date,"%Y-%m-%d %H:%M"))

ids.data <- fread(paste0(DATA_DIR,"ids_summary.csv"), data.table=F) %>%
  dplyr::mutate(date = as.Date(date,"%Y-%m-%d %H:%M:%S"))

pssuq.data <- fread(paste0(DATA_DIR,"pssuq_summary.csv"), data.table=F)

audio.notf <- fread(paste0(DATA_DIR,"audio_notification.csv"), data.table=F) %>%
  dplyr::mutate(notification_time = lubridate::as_datetime(notification_time)) %>%
  dplyr::mutate(start_time = lubridate::as_datetime(start_time)) %>%
  dplyr::mutate(end_time = lubridate::as_datetime(end_time)) %>%
  dplyr::mutate(activity='audio')

esm.notf <- fread(paste0(DATA_DIR,"esm_notification.csv"), data.table=F) %>%
  dplyr::mutate(notification_time = ifelse(notification_time==-1,NA,notification_time)) %>%
  dplyr::mutate(notification_time = lubridate::as_datetime(notification_time)) %>%
  dplyr::mutate(start_time = lubridate::as_datetime(start_time)) %>%
  dplyr::mutate(end_time = lubridate::as_datetime(end_time)) %>%
  dplyr::mutate(activity='esm') %>%
  dplyr::mutate( delta.notif.vs.start = as.numeric(start_time  - notification_time),
                 total.time.taken = as.numeric(end_time - start_time))

esm28.notf <- fread(paste0(DATA_DIR,"esm28q_notification.csv"), data.table=F) %>%
  dplyr::mutate(notification_time = ifelse(notification_time==-1,NA,notification_time)) %>%
  dplyr::mutate(notification_time = lubridate::as_datetime(notification_time)) %>%
  dplyr::mutate(start_time = lubridate::as_datetime(start_time)) %>%
  dplyr::mutate(end_time = lubridate::as_datetime(end_time)) %>%
  dplyr::mutate(activity='esm28') %>%
  dplyr::mutate( delta.notif.vs.start = as.numeric(start_time  - notification_time),
                 total.time.taken = as.numeric(end_time - start_time))

phq8.notf <- fread(paste0(DATA_DIR,"phq8_notification.csv"), data.table=F) %>%
  dplyr::mutate(notification_time = ifelse(notification_time==-1,NA,notification_time)) %>%
  dplyr::mutate(notification_time = lubridate::as_datetime(notification_time)) %>%
  dplyr::mutate(start_time = lubridate::as_datetime(start_time)) %>%
  dplyr::mutate(end_time = lubridate::as_datetime(end_time)) %>%
  dplyr::mutate(activity='phq8') %>%
  dplyr::mutate( delta.notif.vs.start = as.numeric(start_time  - notification_time),
                 total.time.taken = as.numeric(end_time - start_time)) %>%
  dplyr::mutate(sum_phq8 = s_1+s_2+s_3+s_4+s_5+s_6+s_7+s_8) 

rses.notf <- fread(paste0(DATA_DIR,"rses_notification.csv"), data.table=F) %>%
  dplyr::mutate(notification_time = ifelse(notification_time==-1,NA,notification_time)) %>%
  dplyr::mutate(notification_time = lubridate::as_datetime(notification_time)) %>%
  dplyr::mutate(start_time = lubridate::as_datetime(start_time)) %>%
  dplyr::mutate(end_time = lubridate::as_datetime(end_time)) %>%
  dplyr::mutate(activity='rses') %>%
  dplyr::mutate( delta.notif.vs.start = as.numeric(start_time  - notification_time),
                 total.time.taken = as.numeric(end_time - start_time)) %>%
  dplyr::mutate(sum_rses = as.numeric(s_1 + (3-s_2) + s_3 + s_4 + (3-s_5) + (3-s_6) + s_7 + (3-s_8) + (3-s_9) + s_10))


#ALL PARTICIPANT DATES
active.data.dates <- active.data %>%
  dplyr::group_by(p_id) %>%
  dplyr::summarise(active.task.startDate = lubridate::as_date(min(timestamp)),
                   active.task.lastDate = lubridate::as_date(max(timestamp)))


passive.data.dates <- passive.data %>%
  dplyr::group_by(p_id) %>%
  dplyr::summarise(passive.task.startDate = lubridate::as_date(min(timestamp)),
                   passive.task.lastDate = lubridate::as_date(max(timestamp)))

phone.passive.data.dates <- passive.data %>%
  dplyr::filter(dataName=='Passive_phone')%>%
  dplyr::group_by(p_id) %>%
  dplyr::summarise(phone.passive.task.startDate = lubridate::as_date(min(timestamp)),
                   phone.passive.task.lastDate = lubridate::as_date(max(timestamp)))

fitbit.passive.data.dates <- passive.data %>%
  dplyr::filter(dataName=='Passive_Fitbit')%>%
  dplyr::group_by(p_id) %>%
  dplyr::summarise(fitbit.passive.task.startDate = lubridate::as_date(min(timestamp)),
                   fitbit.passive.task.lastDate = lubridate::as_date(max(timestamp)))


clin.assessment.dates <- clin.assessment.data %>%
  dplyr::group_by(p_id) %>%
  dplyr::summarise(first.clin.assessment = lubridate::as_date(min(timestamp)),
                   last.clin.assessment = lubridate::as_date(max(timestamp)))



participant.dates = merge(active.data.dates, passive.data.dates, all=T)
participant.dates <- merge(participant.dates, enrollment.info, all = T)
participant.dates <- merge(participant.dates, clin.assessment.dates, all = T)



## Remove dummy participants
participant.dates <- participant.dates %>% dplyr::filter( ! p_id %in% c('207', '209', '227', 'RADAR-MDD-KCL-s1'))

sum(is.na(participant.dates$enrollment_date))


#### Add day and week in study in PHQ8 AND RSES

phq8.notf <- phq8.notf %>% 
  merge(participant.dates %>% select(p_id, enrollment_date), all.x = T) %>%
  dplyr::mutate(days  = as.numeric(as.Date(lubridate::ymd_hms(start_time)) - lubridate::ymd(enrollment_date )) + 1,
                week = ( days %/% 7 ) + 1)


rses.notf <-  rses.notf %>% 
  merge(participant.dates %>% select(p_id, enrollment_date), all.x = T) %>%
  dplyr::mutate(days  = as.numeric(as.Date(lubridate::ymd_hms(start_time)) - lubridate::ymd(enrollment_date )) + 1,
                week = ( days %/% 7 ) + 1)

###############################
## Data Filtering decision 
###############################
TEMP_REMOVE_SUBJECTS <- setdiff(participant.dates$p_id, enrollment.info$p_id)
# We have 64 participants who have data but no enrollment date. For now we are deleting them but it is critical to double check whether these are real or test participant

participant.dates.flt <- participant.dates %>% dplyr::filter( ! p_id %in% TEMP_REMOVE_SUBJECTS)
active.data.flt <- active.data %>% dplyr::filter( ! p_id %in% TEMP_REMOVE_SUBJECTS)
passive.data.flt <- passive.data %>% dplyr::filter( ! p_id %in% TEMP_REMOVE_SUBJECTS)
clin.assessment.data.flt <- clin.assessment.data %>% dplyr::filter( ! p_id %in% TEMP_REMOVE_SUBJECTS)
demog.data.flt <- demog.data %>% dplyr::filter( ! p_id %in% TEMP_REMOVE_SUBJECTS)
phq8.notf <- phq8.notf %>% dplyr::filter( ! p_id %in% TEMP_REMOVE_SUBJECTS)
rses.notf <- rses.notf %>% dplyr::filter( ! p_id %in% TEMP_REMOVE_SUBJECTS)