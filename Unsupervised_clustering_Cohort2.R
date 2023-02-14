# streaksAnalysis for biweekly surveys only
library('rjson')
library('grid')
library("ComplexHeatmap")
#install.packages("factoextra")
library("factoextra")
library("tableone")


# filter data for participants whose 2 years' obsearvation period
cohort2_ids <- read.delim("D:/PHD/data/MDD_engagement/prepared_data/COHORT_2_FIRST_2Years.tsv",header = F)[,1]
participant.dates.flt <- participant.dates.flt%>% dplyr::filter(p_id %in% cohort2_ids) 
active.data.flt <- active.data.flt %>% dplyr::filter(p_id %in% cohort2_ids) 
passive.data.flt <- passive.data.flt %>% dplyr::filter(p_id %in% cohort2_ids) 
clin.assessment.data.flt <- clin.assessment.data.flt %>% dplyr::filter(p_id %in% cohort2_ids) 
demog.data.flt <- demog.data.flt %>% dplyr::filter(p_id %in% cohort2_ids) 


# To be able to run this script you will need to run the "loadData.R" script
active.data.flt<-merge(active.data.flt, enrollment.info)
# calculate the day in study for each active records
active.data.flt$daysInStudy <- as.Date(active.data.flt$timestamp)-as.Date(active.data.flt$enrollment_date) + 1


# all participant ids of the study
select_participant <- demog.data.flt$p_id
# select only biweekly surveys: PHQ-8 and RSES
active.data.select <- active.data.flt %>%
  dplyr::filter(dataName == 'PHQ8' | dataName == 'RSES')
# change days to weeks
active.data.select <- active.data.select %>% dplyr::mutate(week = floor((daysInStudy -1)/7) + 1,
                                                           biweek = floor((daysInStudy -1)/14) + 1)
# mean of PHQ8,response, survey time, overtime
phq8.notf.cohort2<- phq8.notf %>%
  dplyr::filter(days<=660) %>%
  dplyr::group_by(p_id) %>%
  dplyr::summarise(mean_phq8_score = mean(sum_phq8),mean_phq8_response_time = median(delta.notif.vs.start)/60,mean_phq8_total_time=median(total.time.taken))
rses.notf.cohort2<- rses.notf %>%
  dplyr::filter(days<=660) %>%
  dplyr::group_by(p_id) %>%
  dplyr::summarise(mean_rses_score = mean(sum_rses),mean_rses_response_time = median(delta.notif.vs.start)/60,mean_rses_total_time=median(total.time.taken))


# select active data before 660 days
active.data.select <- active.data.select %>%
  dplyr::filter(daysInStudy <= 660 & daysInStudy>0) %>%
  dplyr::group_by(p_id, biweek) %>%
  dplyr::summarise(n = n())


tmp <- expand.grid(biweek = seq(1,48,by=1),p_id = select_participant) # num of rows = 22*614
active.data.select <- merge(active.data.select,tmp,all.x = T, all.y=T)
# summarize by week
active.data.select_01 <- active.data.select %>% mutate(active_binary = ifelse(is.na(n), 0, 1))%>%
  dplyr::select(-n)
# the first figure for active data (0,1)
tmp_matrix_01 <- active.data.select_01 %>%
  spread(biweek, active_binary)
# count the number of the complete survey data
active.data.count <-active.data.select_01 %>%
  dplyr::group_by(p_id) %>%
  dplyr::summarise(count_survey = sum(active_binary))

# summary contact log
contact.log.data.select <- merge(contact.log.data,enrollment.info)
contact.log.data.select$days <- contact.log.data.select$date-as.Date(contact.log.data.select$enrollment_date) + 1 
contact.log.data.select <-contact.log.data.select %>% 
  dplyr::filter(days<=660)%>%
  dplyr::group_by(p_id) %>%
  dplyr::summarise(num_contact = max(record_num))%>%
  dplyr::mutate(num_contact = ifelse(is.na(num_contact), 0, num_contact))

# Add baseline IDS
baseline.ids <- ids.data%>% dplyr::filter(record_num==0) %>% 
  select(p_id, ids_score)
# ids in obsearvation period
ids.data.in301days <- merge(ids.data, enrollment.info) %>%
  dplyr::filter(date - as.Date(enrollment_date)+1<=660 | record_num==0)
ids.count <-ids.data.in301days %>%
  dplyr::group_by(p_id)%>%
  dplyr::summarise(count_ids = n())

p_id_ordered <- tmp_matrix_01$p_id
tmp_matrix_01$p_id <- NULL

# kmeans clustering 
res.wss <- fviz_nbclust(tmp_matrix_01, cluster::pam, method = "wss", k.max = 10) 
res.wss
ggsave(file='Figures/Cohort2_active_optimalClusterN_WSS_method.png',plot=res.wss,width = 4, height = 4, dpi = 100)
k <- 4

km <- kmeans(as.matrix(tmp_matrix_01), centers = k)
km.cluster.allocation <- factor(km$cluster, levels = c("2","4","3","1"))
new_label <- seq(1,95,2)
new_row_title <- paste0('C', 1:k)
ht.phone.active <- ComplexHeatmap::Heatmap(as.matrix(tmp_matrix_01), show_heatmap_legend = F,
                                           split = km.cluster.allocation,
                                           col = circlize::colorRamp2(breaks = c(0,1), colors = c('#e0e0e0', '#1b9e77')),
                                           column_title_side = "top",column_title="Phone-Active data",
                                           show_row_dend = F, cluster_columns = F,cluster_rows = F,row_title = new_row_title,row_title_rot = 0,
                                           show_column_names = T, show_row_names = F,column_labels = new_label,
                                           heatmap_legend_param = list(labels_gp = grid::gpar(fontsize = 1)))
ht.phone.active


png(filename = "Figures/heatmap_clusters_PhoneActiveData_Cohort2.png",
    height = 6, width = 5, res = 200, units="in")
ht.fitbit.passive <- draw(ht.phone.active)
dev.off()

###Creating a dataframe of clusters and participant ids
rowOrder <- ComplexHeatmap::row_order(ht.phone.active)
pid_by_clusters <- sapply(rowOrder, function(x) p_id_ordered[x])
names(pid_by_clusters) <- paste0('C', 1:length(pid_by_clusters))
pid_by_clusters <- reshape2::melt(pid_by_clusters)
colnames(pid_by_clusters) = c('p_id', 'cluster')

# Link clusters with demographic,
pid_by_clusters_wmdata <- merge(pid_by_clusters ,demog.data.flt, all.x = T)
pid_by_clusters_wmdata <- merge(pid_by_clusters_wmdata ,enrollment.info, all.x = T)
pid_by_clusters_wmdata <- merge(pid_by_clusters_wmdata ,active.data.count, all.x = T)
pid_by_clusters_wmdata <- merge(pid_by_clusters_wmdata ,contact.log.data.select, all.x = T)
pid_by_clusters_wmdata <- merge(pid_by_clusters_wmdata ,ids.count, all.x = T)
pid_by_clusters_wmdata <- merge(pid_by_clusters_wmdata ,phq8.notf.cohort2, all.x = T)
pid_by_clusters_wmdata <- merge(pid_by_clusters_wmdata ,rses.notf.cohort2, all.x = T)
pid_by_clusters_wmdata <- merge(pid_by_clusters_wmdata ,pssuq.data, all.x = T)

#Add baseline IDS
pid_by_clusters_wmdata <- merge(pid_by_clusters_wmdata ,baseline.ids, all.x = T)
#Add  baseline PHQ8

baseline.phq8 <- phq8.notf %>% dplyr::filter(week %in% c(1,2)) %>% 
  select(p_id, sum_phq8) %>% 
  dplyr::group_by(p_id) %>%
  dplyr::summarise(sum_phq8 = max(sum_phq8))
pid_by_clusters_wmdata <- merge(pid_by_clusters_wmdata ,baseline.phq8, all.x = T)

#Add baseline RSES 
baseline.rses <- rses.notf %>% dplyr::filter(week %in% c(1,2)) %>% 
  select(p_id, sum_rses) %>% 
  dplyr::group_by(p_id) %>%
  dplyr::summarise(sum_rses = max(sum_rses))
pid_by_clusters_wmdata <- merge(pid_by_clusters_wmdata ,baseline.rses, all.x = T)
active_clusters <- pid_by_clusters_wmdata
# record C1 participants
C1_active <- pid_by_clusters_wmdata %>%
  dplyr::filter(cluster=="C1")%>%
  dplyr::mutate(cluster="C1_active")
# record C3 participants
C4_active <- pid_by_clusters_wmdata %>%
  dplyr::filter(cluster=="C4")%>%
  dplyr::mutate(cluster="C4_active")

#Create a variable list which we want in Table 1
#Create a variable list which we want in Table 1
listVars <- c("age", "sex","marry_align", "ETHCAT2","employment","children","edu_age_align","income_align","accommodation_occupy_align","sum_phq8",
              "comorb","depression_medication","num_contact","phone_brand","phone_status","site")

#Define categorical variables
catVars <- c("sex","marry_align", "ETHCAT2","employment","children","income_align","accommodation_occupy_align",
             "comorb","depression_medication","phone_brand","phone_status","site")
tabResults <- tableone::CreateTableOne(data=pid_by_clusters_wmdata, vars=listVars, 
                                       factorVars = catVars, 
                                       strata = c("cluster"), includeNA = F)
demo_table <- print(tabResults, nonnormal = listVars, 
                    formatOptions = list(big.mark = ","))

colnames(pid_by_clusters_wmdata)

##########
# PHQ8 - Diff in Total time taken + Response to Notification
##########
tmp.phq8.with.clusters  <- merge(pid_by_clusters, phq8.notf) %>%
  dplyr::mutate(delta.notif.vs.start = delta.notif.vs.start / 60)

listVars <- c("sum_phq8","delta.notif.vs.start", "total.time.taken")
tabResults <- tableone::CreateTableOne(data=tmp.phq8.with.clusters, 
                                       vars=listVars, strata = c("cluster"),
                                       includeNA = F)
phq_lon_table <- print(tabResults, nonnormal = c("sum_phq8","delta.notif.vs.start", "total.time.taken"), 
                       formatOptions = list(big.mark = ","))


##########
# RSES - Diff in Total time taken + Response to Notification
##########
tmp.rses.with.clusters  <- merge(pid_by_clusters, rses.notf) %>%
  dplyr::mutate(delta.notif.vs.start = delta.notif.vs.start / 60)
tmp.rses.with.clusters$sum_rses[tmp.rses.with.clusters$sum_rses>30] <- NA
listVars <- c("sum_rses","delta.notif.vs.start", "total.time.taken")
tabResults <- tableone::CreateTableOne(data=tmp.rses.with.clusters, 
                                       vars=listVars, strata = c("cluster"),
                                       includeNA = F)
rses_lon_table <- print(tabResults, nonnormal = c("sum_rses","delta.notif.vs.start", "total.time.taken"), 
                        formatOptions = list(big.mark = ","))
##########
# IDS - Diff in longitudinal IDS score
##########
tmp.ids.with.clusters  <- merge(pid_by_clusters,ids.data.in301days)
listVars <- c("ids_score")
tabResults <- tableone::CreateTableOne(data=tmp.ids.with.clusters, 
                                       vars=listVars, strata = c("cluster"),
                                       includeNA = F)
ids_lon_table <- print(tabResults, nonnormal = c("ids_score"), 
                       formatOptions = list(big.mark = ","))
all<-rbind(demo_table[1:4,], phq_lon_table[2:4,])
all<-rbind(all, demo_table[5,])
all<-rbind(all, rses_lon_table[2:4,])
all<-rbind(all,demo_table[6:nrow(demo_table),])
all<-rbind(all,ids_lon_table[2,])
write.csv(demo_table, 
          file = "Tables/Table_PhoneActiveData_Clusters_Comparison_Cohort2.csv")

# for phone passive and fitbit passive
passive.data.flt<-merge(passive.data.flt, enrollment.info)
# calculate the day in study for each passive records
passive.data.flt$daysInStudy <- as.Date(passive.data.flt$timestamp)-as.Date(passive.data.flt$enrollment_date) + 1
# all participant ids of the study
# phone data
passive.phone.data.select <- passive.data.flt %>%
  dplyr::filter(daysInStudy <= 660 & daysInStudy>0 & dataName=="Passive_phone") %>%
  dplyr::group_by(p_id, daysInStudy) %>%
  dplyr::summarise(n = n())
# fitbit data
passive.fitbit.data.select <- passive.data.flt %>%
  dplyr::filter(daysInStudy <= 660 & daysInStudy>0 & dataName=="Passive_Fitbit") %>%
  dplyr::group_by(p_id, daysInStudy) %>%
  dplyr::summarise(n = n())
select_participant<-demog.data.flt$p_id
tmp <- expand.grid(daysInStudy = seq(1,660,by=1),p_id = select_participant) 
passive.phone.data.select <- merge(passive.phone.data.select,tmp,all.x = T, all.y=T)
passive.phone.data.select <- passive.phone.data.select %>% mutate(active = ifelse(is.na(n), 0, 1))
passive.phone.data.select <- passive.phone.data.select%>%select(-n)
# count the number of days of phone passive data
passive.phone.data.count <-passive.phone.data.select %>%
  dplyr::group_by(p_id) %>%
  dplyr::summarise(count_passive = sum(active))
tmp_phone <- passive.phone.data.select %>%
  spread(daysInStudy, active)
# plot heatmap
p_id_ordered <- tmp_phone$p_id
tmp_phone$p_id <- NULL
res.wss <- fviz_nbclust(tmp_phone, cluster::pam, method = "wss", k.max = 10) 
res.wss
ggsave(file='Figures/Cohort2_phone_optimalClusterN_WSS_method.png',plot=res.wss,width = 4, height = 4, dpi = 100)

k<-4
set.seed('3435435')
km <- kmeans(as.matrix(tmp_phone), centers = k)
km.cluster.allocation <- factor(km$cluster, levels = c('3','4','2','1'))
new_label <- seq(0,659)
new_label <- ifelse(new_label%%14==0, new_label/14 +1, -1)
new_label <- ifelse(new_label!=-1, new_label*2 -1, -1)
new_label <- as.character(new_label)
new_label <- ifelse(new_label=="-1", "", new_label)
new_row_title <- paste0('C', 1:k)
ht.phone.passive <- ComplexHeatmap::Heatmap(as.matrix(tmp_phone), show_heatmap_legend = F,
                                            split = km.cluster.allocation,
                                            col = circlize::colorRamp2(breaks = c(0,1), colors = c('#e0e0e0', '#d95f02')),
                                            column_title_side = "top",column_title="Phone-Passive data",
                                            show_row_dend = F, cluster_columns = F,cluster_rows = F,row_title = new_row_title,row_title_rot = 0,
                                            show_column_names = T, show_row_names = F,column_labels = new_label,
                                            heatmap_legend_param = list(labels_gp = grid::gpar(fontsize = 1)))

ht.phone.passive

png(filename = "Figures/heatmap_clusters_PhonePassiveData_Cohort2.png",
    height = 6, width = 5, res = 200, units="in")
ht.fitbit.passive <- draw(ht.phone.passive)
dev.off()


###Creating a dataframe of clusters and participant ids
rowOrder <- ComplexHeatmap::row_order(ht.phone.passive)
pid_by_clusters <- sapply(rowOrder, function(x) p_id_ordered[x])
names(pid_by_clusters) <- paste0('C', 1:length(pid_by_clusters))
pid_by_clusters <- reshape2::melt(pid_by_clusters)
colnames(pid_by_clusters) = c('p_id', 'cluster')

# Link clusters with demographic,
pid_by_clusters_wmdata <- merge(pid_by_clusters ,demog.data.flt, all.x = T)
pid_by_clusters_wmdata <- merge(pid_by_clusters_wmdata ,enrollment.info, all.x = T)
pid_by_clusters_wmdata <- merge(pid_by_clusters_wmdata ,passive.phone.data.count, all.x = T)
pid_by_clusters_wmdata <- merge(pid_by_clusters_wmdata ,contact.log.data.select, all.x = T)
pid_by_clusters_wmdata <- merge(pid_by_clusters_wmdata ,ids.count, all.x = T)
pid_by_clusters_wmdata <- merge(pid_by_clusters_wmdata ,phq8.notf.cohort2, all.x = T)
pid_by_clusters_wmdata <- merge(pid_by_clusters_wmdata ,rses.notf.cohort2, all.x = T)
pid_by_clusters_wmdata <- merge(pid_by_clusters_wmdata ,pssuq.data, all.x = T)
# Add baseline IDS
pid_by_clusters_wmdata <- merge(pid_by_clusters_wmdata ,baseline.ids, all.x = T)
#Add  baseline PHQ8
baseline.phq8 <- phq8.notf %>% dplyr::filter(week %in% c(1,2)) %>% 
  dplyr::select(p_id, sum_phq8) %>%
  dplyr::group_by(p_id) %>%
  dplyr::slice(which.max(sum_phq8)) 
pid_by_clusters_wmdata <- merge(pid_by_clusters_wmdata ,baseline.phq8, all.x = T)


# baseline.phq8 <- phq8.notf %>% dplyr::filter(week %in% c(1,2)) %>% 
#   dplyr::select(p_id, sum_phq8, total.time.taken, delta.notif.vs.start) %>%
#   dplyr::mutate(delta.notif.vs.start = delta.notif.vs.start / 60) %>%
#   dplyr::group_by(p_id) %>%
#   dplyr::slice(which.max(sum_phq8)) 
# pid_by_clusters_wmdata <- merge(pid_by_clusters_wmdata ,baseline.phq8, all.x = T)



#Add baseline RSES 
baseline.rses <- rses.notf %>% dplyr::filter(week %in% c(1,2)) %>% 
  dplyr::select(p_id, sum_rses) %>%
  dplyr::group_by(p_id) %>%
  dplyr::slice(which.max(sum_rses))
pid_by_clusters_wmdata <- merge(pid_by_clusters_wmdata ,baseline.rses, all.x = T)
passive_clusters <- pid_by_clusters_wmdata
# record C1 participants
C1_passive <- pid_by_clusters_wmdata %>%
  dplyr::filter(cluster=="C1")%>%
  dplyr::mutate(cluster="C1_passive")
# record C3 participants
C4_passive <- pid_by_clusters_wmdata %>%
  dplyr::filter(cluster=="C4")%>%
  dplyr::mutate(cluster="C4_passive")


#Create a variable list which we want in Table 1
listVars <- c("age", "sex","marry_align", "ETHCAT2","employment","children","edu_age_align","income_align","accommodation_occupy_align","sum_phq8",
              "comorb","depression_medication","num_contact","phone_brand","phone_status","site")

#Define categorical variables
catVars <- c("sex","marry_align", "ETHCAT2","employment","children","income_align","accommodation_occupy_align",
             "comorb","depression_medication","phone_brand","phone_status","site")
tabResults <- tableone::CreateTableOne(data=pid_by_clusters_wmdata, vars=listVars, 
                                       factorVars = catVars, 
                                       strata = c("cluster"), includeNA = F)
demo_table <- print(tabResults, nonnormal = listVars, 
                    formatOptions = list(big.mark = ","))

colnames(pid_by_clusters_wmdata)
##########
# PHQ8 - Diff in Total time taken + Response to Notification
##########
tmp.phq8.with.clusters  <- merge(pid_by_clusters, phq8.notf) %>%
  dplyr::mutate(delta.notif.vs.start = delta.notif.vs.start / 60)

listVars <- c("sum_phq8","delta.notif.vs.start", "total.time.taken")
tabResults <- tableone::CreateTableOne(data=tmp.phq8.with.clusters, 
                                       vars=listVars, strata = c("cluster"),
                                       includeNA = F)
phq_lon_table <- print(tabResults, nonnormal = c("sum_phq8","delta.notif.vs.start", "total.time.taken"), 
                       formatOptions = list(big.mark = ","))


##########
# RSES - Diff in Total time taken + Response to Notification
##########
tmp.rses.with.clusters  <- merge(pid_by_clusters, rses.notf) %>%
  dplyr::mutate(delta.notif.vs.start = delta.notif.vs.start / 60)
tmp.rses.with.clusters$sum_rses[tmp.rses.with.clusters$sum_rses>30] <- NA
listVars <- c("sum_rses","delta.notif.vs.start", "total.time.taken")
tabResults <- tableone::CreateTableOne(data=tmp.rses.with.clusters, 
                                       vars=listVars, strata = c("cluster"),
                                       includeNA = F)
rses_lon_table <- print(tabResults, nonnormal = c("sum_rses","delta.notif.vs.start", "total.time.taken"), 
                        formatOptions = list(big.mark = ","))
##########
# IDS - Diff in longitudinal IDS score
##########
tmp.ids.with.clusters  <- merge(pid_by_clusters,ids.data.in301days)
listVars <- c("ids_score")
tabResults <- tableone::CreateTableOne(data=tmp.ids.with.clusters, 
                                       vars=listVars, strata = c("cluster"),
                                       includeNA = F)
ids_lon_table <- print(tabResults, nonnormal = c("ids_score"), 
                       formatOptions = list(big.mark = ","))
all<-rbind(demo_table[1:4,], phq_lon_table[2:4,])
all<-rbind(all, demo_table[5,])
all<-rbind(all, rses_lon_table[2:4,])
all<-rbind(all,demo_table[6:nrow(demo_table),])
all<-rbind(all,ids_lon_table[2,])
write.csv(demo_table, 
          file = "Tables/Table_PhonePassiveData_Clusters_Comparison_Cohort2.csv")


# Fitbit
passive.fitbit.data.select <- merge(passive.fitbit.data.select,tmp,all.x = T, all.y=T)
passive.fitbit.data.select <- passive.fitbit.data.select %>% mutate(active = ifelse(is.na(n), 0, 1))
passive.fitbit.data.select <- passive.fitbit.data.select%>%select(-n)
# count the number of days of fitbit passive data
passive.fitbit.data.count <-passive.fitbit.data.select %>%
  dplyr::group_by(p_id) %>%
  dplyr::summarise(count_fitbit = sum(active))
tmp_fitbit <- passive.fitbit.data.select %>%
  spread(daysInStudy, active)

p_id_ordered <- tmp_fitbit$p_id
tmp_fitbit$p_id <- NULL
res.wss <- fviz_nbclust(tmp_fitbit, cluster::pam, method = "wss", k.max = 10) 
res.wss
ggsave(file='Figures/Cohort2_fitbit_optimalClusterN_WSS_method.png',plot=res.wss,width = 4, height = 4, dpi = 100)

k<-4
set.seed('3435000')
km <- kmeans(as.matrix(tmp_fitbit), centers = k)
km.cluster.allocation <- factor(km$cluster, c('3','1','4','2'))
new_label <- seq(0,659)
new_label <- ifelse(new_label%%14==0, new_label/14 +1, -1)
new_label <- ifelse(new_label!=-1, new_label*2 -1, -1)
new_label <- as.character(new_label)
new_label <- ifelse(new_label=="-1", "", new_label)
new_row_title <- paste0('C', 1:k)
ht.fitbit.passive <- ComplexHeatmap::Heatmap(as.matrix(tmp_fitbit), show_heatmap_legend = F,
                                             split = km.cluster.allocation,
                                             col = circlize::colorRamp2(breaks = c(0,1), colors = c('#e0e0e0', '#e7298a')),
                                             column_title_side = "top",column_title="Fitbit-Passive data",
                                             show_row_dend = F, cluster_columns = F,cluster_rows = F,row_title = new_row_title,row_title_rot = 0,
                                             show_column_names = T, show_row_names = F,column_labels = new_label,
                                             heatmap_legend_param = list(labels_gp = grid::gpar(fontsize = 1)))
ht.fitbit.passive
png(filename = "Figures/heatmap_clusters_FitbitPassiveData_Cohort2.png",
    height = 6, width = 5, res = 200, units="in")
ht.fitbit.passive <- draw(ht.fitbit.passive)
dev.off()

#######
# Compare participant chars across clusters
######

###Creating a dataframe of clusters and participant ids
rowOrder <- ComplexHeatmap::row_order(ht.fitbit.passive)
pid_by_clusters <- sapply(rowOrder, function(x) p_id_ordered[x])
names(pid_by_clusters) <- paste0('C', 1:length(pid_by_clusters))
pid_by_clusters <- reshape2::melt(pid_by_clusters)
colnames(pid_by_clusters) = c('p_id', 'cluster')

# Link clusters with demographic,
pid_by_clusters_wmdata <- merge(pid_by_clusters ,demog.data.flt, all.x = T)
pid_by_clusters_wmdata <- merge(pid_by_clusters_wmdata ,enrollment.info, all.x = T)
pid_by_clusters_wmdata <- merge(pid_by_clusters_wmdata ,passive.fitbit.data.count, all.x = T)
pid_by_clusters_wmdata <- merge(pid_by_clusters_wmdata ,contact.log.data.select, all.x = T)
pid_by_clusters_wmdata <- merge(pid_by_clusters_wmdata ,ids.count, all.x = T)
pid_by_clusters_wmdata <- merge(pid_by_clusters_wmdata ,phq8.notf.cohort2, all.x = T)
pid_by_clusters_wmdata <- merge(pid_by_clusters_wmdata ,rses.notf.cohort2, all.x = T)
pid_by_clusters_wmdata <- merge(pid_by_clusters_wmdata ,pssuq.data, all.x = T)
# Add baseline IDS
pid_by_clusters_wmdata <- merge(pid_by_clusters_wmdata ,baseline.ids, all.x = T)
#Add  baseline PHQ8
baseline.phq8 <- phq8.notf %>% dplyr::filter(week %in% c(1,2)) %>% 
  select(p_id, sum_phq8) %>% 
  dplyr::group_by(p_id) %>%
  dplyr::summarise(sum_phq8 = max(sum_phq8))
pid_by_clusters_wmdata <- merge(pid_by_clusters_wmdata ,baseline.phq8, all.x = T)

#Add baseline RSES 
baseline.rses <- rses.notf %>% dplyr::filter(week %in% c(1,2)) %>% 
  select(p_id, sum_rses) %>% 
  dplyr::group_by(p_id) %>%
  dplyr::summarise(sum_rses = max(sum_rses))
pid_by_clusters_wmdata <- merge(pid_by_clusters_wmdata ,baseline.rses, all.x = T)
fitbit_clusters <- pid_by_clusters_wmdata
# record C1 participants
C1_fitbit <- pid_by_clusters_wmdata %>%
  dplyr::filter(cluster=="C1")%>%
  dplyr::mutate(cluster="C1_fitbit")
# record C4 participants
C4_fitbit <- pid_by_clusters_wmdata %>%
  dplyr::filter(cluster=="C4")%>%
  dplyr::mutate(cluster="C4_fitbit")

#Create a variable list which we want in Table 1
listVars <- c("age", "sex","marry_align", "ETHCAT2","employment","children","edu_age_align","income_align","accommodation_occupy_align","sum_phq8",
              "comorb","depression_medication","num_contact","phone_brand","phone_status","site")

#Define categorical variables
catVars <- c("sex","marry_align", "ETHCAT2","employment","children","income_align","accommodation_occupy_align",
             "comorb","depression_medication","phone_brand","phone_status","site")
tabResults <- tableone::CreateTableOne(data=pid_by_clusters_wmdata, vars=listVars, 
                                       factorVars = catVars, 
                                       strata = c("cluster"), includeNA = F)
demo_table <- print(tabResults, nonnormal = listVars, 
                    formatOptions = list(big.mark = ","))

colnames(pid_by_clusters_wmdata)
##########
# PHQ8 - Diff in Total time taken + Response to Notification
##########
tmp.phq8.with.clusters  <- merge(pid_by_clusters, phq8.notf) %>%
  dplyr::mutate(delta.notif.vs.start = delta.notif.vs.start / 60)

listVars <- c("sum_phq8","delta.notif.vs.start", "total.time.taken")
tabResults <- tableone::CreateTableOne(data=tmp.phq8.with.clusters, 
                                       vars=listVars, strata = c("cluster"),
                                       includeNA = F)
phq_lon_table <- print(tabResults, nonnormal = c("sum_phq8","delta.notif.vs.start", "total.time.taken"), 
                       formatOptions = list(big.mark = ","))


##########
# RSES - Diff in Total time taken + Response to Notification
##########
tmp.rses.with.clusters  <- merge(pid_by_clusters, rses.notf) %>%
  dplyr::mutate(delta.notif.vs.start = delta.notif.vs.start / 60)
tmp.rses.with.clusters$sum_rses[tmp.rses.with.clusters$sum_rses>30] <- NA
listVars <- c("sum_rses","delta.notif.vs.start", "total.time.taken")
tabResults <- tableone::CreateTableOne(data=tmp.rses.with.clusters, 
                                       vars=listVars, strata = c("cluster"),
                                       includeNA = F)
rses_lon_table <- print(tabResults, nonnormal = c("sum_rses","delta.notif.vs.start", "total.time.taken"), 
                        formatOptions = list(big.mark = ","))
##########
# IDS - Diff in longitudinal IDS score
##########
tmp.ids.with.clusters  <- merge(pid_by_clusters,ids.data.in301days)
listVars <- c("ids_score")
tabResults <- tableone::CreateTableOne(data=tmp.ids.with.clusters, 
                                       vars=listVars, strata = c("cluster"),
                                       includeNA = F)
ids_lon_table <- print(tabResults, nonnormal = c("ids_score"), 
                       formatOptions = list(big.mark = ","))
all<-rbind(demo_table[1:4,], phq_lon_table[2:4,])
all<-rbind(all, demo_table[5,])
all<-rbind(all, rses_lon_table[2:4,])
all<-rbind(all,demo_table[6:nrow(demo_table),])
all<-rbind(all,ids_lon_table[2,])
write.csv(demo_table, 
          file = "Tables/Table_FitbitPassiveData_Clusters_Comparison_Cohort2.csv")
# see C1 of 3 datastreams


# plot box plots
active_clusters$cat = "Phone-Active"
passive_clusters$cat = "Phone-Passive"
fitbit_clusters$cat = "Fitbit-Passive"

active_clusters$count = active_clusters$count_survey *14
passive_clusters$count = passive_clusters$count_passive
fitbit_clusters$count = fitbit_clusters$count_fitbit
active_clusters$total_time = active_clusters$mean_phq8_total_time + active_clusters$mean_rses_total_time
passive_clusters$total_time = passive_clusters$mean_phq8_total_time + passive_clusters$mean_rses_total_time
fitbit_clusters$total_time = fitbit_clusters$mean_phq8_total_time + fitbit_clusters$mean_rses_total_time

c1 <- active_clusters[,c("age","cluster","sum_phq8","mean_phq8_score","mean_phq8_response_time","mean_phq8_total_time","count","cat")]
c2 <- passive_clusters[,c("age","cluster","sum_phq8","mean_phq8_score","mean_phq8_response_time","mean_phq8_total_time","count","cat")]
c3 <- fitbit_clusters[,c("age","cluster","sum_phq8","mean_phq8_score","mean_phq8_response_time","mean_phq8_total_time","count","cat")]
cluster_all <- rbind(c1, c2, c3)



# box plots
# Compare different data streams
# baseline phq8
p <- ggplot(data = cluster_all, aes(x=cat, y=sum_phq8, fill=cluster)) + geom_boxplot(outlier.shape = NA,width=0.5)
p <- p + theme_bw(base_size = 15)  + ylab('Scores') +xlab('') + ggtitle("Cohort 2: Baseline PHQ8")
p <- p + theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5),
               axis.text.x = element_text(size=11, face="bold")) 
p <- p + guides(fill=FALSE)
p
ggsave("Boxplot_cohort2_baseline_phq8_compare_datastreams.png", plot=print(p), height = 6, width = 6, units="in", dpi=250)

# response phq8
p <- ggplot(data = cluster_all, aes(x=cat, y=mean_phq8_response_time, fill=cluster)) + geom_boxplot(outlier.shape = NA,width=0.5)
p <- p + theme_bw(base_size = 15)  + ylab('Minutes')+xlab('')  + ggtitle("Cohort 2: PHQ8 response time")
p <- p + theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5),
               axis.text.x = element_text(size=11, face="bold")) 
ylim1 = boxplot.stats(cluster_all$mean_phq8_response_time)$stats[c(1, 5)]
ylim1[1] = ylim1[1]*0
ylim1[2] = ylim1[2]*1.1
p <- p + coord_cartesian(ylim = ylim1)
p <- p + guides(fill=FALSE)
p
ggsave("Boxplot_cohort2_phq8_response_time_compare_datastreams.png", plot=print(p), height = 6, width = 6, units="in", dpi=250)

# PHQ8 completion time
p <- ggplot(data = cluster_all, aes(x=cat, y=mean_phq8_total_time, fill=cluster)) + geom_boxplot(outlier.shape = NA,width=0.5)
p <- p + theme_bw(base_size = 15)  + ylab('Seconds')+xlab('')  + ggtitle("Cohort 2: PHQ8 completion time")
p <- p + theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5),
               axis.text.x = element_text(size=11, face="bold")) 
ylim1 = boxplot.stats(cluster_all$mean_phq8_total_time)$stats[c(1, 5)]
ylim1[1] = ylim1[1]
ylim1[2] = ylim1[2]*1.1

p <- p + coord_cartesian(ylim = ylim1)
p <- p + guides(fill=FALSE)
p
ggsave("Boxplot_cohort2_phq8_total_time_compare_datastreams.png", plot=print(p), height = 6, width = 6, units="in", dpi=250)

# age
p <- ggplot(data = cluster_all, aes(x=cat, y=age, fill=cluster)) + geom_boxplot(outlier.shape = NA,width=0.5)
p <- p + theme_bw(base_size = 15)  + ylab('Years')+xlab('')  + ggtitle("Cohort 2: Age")
p <- p + theme(legend.title = element_blank(),plot.title = element_text(hjust = 0.5),
               axis.text.x = element_text(size=11, face="bold")) 
p <- p + guides(fill=FALSE)
p
ggsave("Boxplot_cohort2_age_compare_datastreams.png", plot=print(p), height = 6, width = 6, units="in", dpi=250)


