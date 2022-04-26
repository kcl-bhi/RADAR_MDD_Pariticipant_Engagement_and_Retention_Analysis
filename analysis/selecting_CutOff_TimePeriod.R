rm(list=ls())
source("loadData.R")

n_distinct(active.data.flt$p_id)
n_distinct(passive.data.flt$p_id)


#Study First active data collection
STUDY.FIRST.DATA.DAY = as.Date(min(participant.dates.flt$enrollment_date, na.rm = T))
STUDY.FIRST.DATA.DAY

#Study First active data collection
STUDY.LAST.DATA.DAY = as.Date(max(participant.dates.flt$active.task.lastDate, na.rm = T))
STUDY.LAST.DATA.DAY



#Theoritical max time in study 
days.in.study <- participant.dates.flt %>% 
  dplyr::mutate(theoritical.max = as.numeric(STUDY.LAST.DATA.DAY - enrollment_date) + 1,
                actual.days = as.numeric(active.task.lastDate - enrollment_date) + 1,
                actual.days = ifelse(is.na(actual.days), 1,  actual.days)) %>%
  select(p_id, theoritical.max, actual.days) %>%
  tidyr::gather(type, value, -p_id )



p <- ggplot(data=days.in.study, aes(x=value, fill=type)) + geom_density(alpha=0.7) + theme_bw(base_size = 14)
p <- p + xlab('Days')
p
ggsave(file='densityPlot.days.in.study.png', plot=p, width=8, height = 6, units="in")


theort.qauntiles <- quantile( days.in.study %>% dplyr::filter(type == 'theoritical.max') %>% .$value, 
                              probs = seq(0,1,.02))
theort.qauntiles

empirical.qauntiles <- quantile( days.in.study %>% dplyr::filter(type == 'actual.days') %>% .$value, 
                                 probs = seq(0,1,.05), )
empirical.qauntiles

quantile.values <- data.frame(quantile = seq(0,100,5),
                              theoritical = as.numeric(theort.qauntiles),
                              empiricial = as.numeric(empirical.qauntiles)) %>%
  tidyr::gather(type, value, -quantile) 


install_load("ggthemes")
p <- ggplot(data=quantile.values, aes(x=quantile, y=value, color = type)) + geom_point() + theme_bw(base_size = 14) + scale_colour_calc()
p + geom_vline(xintercept = 5)
ggsave(file='quantile.plot.png', plot=p, width=8, height = 6, units="in")


# Cohort 1
# first 301 days 
COHORT_1_FIRST_314Days <- days.in.study %>% filter(type == 'theoritical.max' & value >=301) %>% .$p_id %>% unique()
COHORT_1_FIRST_314Days
write.table(COHORT_1_FIRST_314Days, file = "COHORT_1_FIRST_301Days.tsv", sep = "\t", quote = F, row.names = F, col.names = F )


# Cohort 2
# first 660 days 
COHORT_2_FIRST_2Years <- days.in.study %>% filter(type == 'theoritical.max' & value >=660) %>% .$p_id %>% unique()
write.table(COHORT_2_FIRST_2Years, file = "COHORT_2_FIRST_660Days.tsv", sep = "\t", quote = F, row.names = F, col.names = F )