# filter data for participants whose 2 years' obsearvation period
# need to run the loadData.R first. Then filter by the 2nd cohort ids
library(survival)
library(survminer)
library(data.table)
library(psych)
library(plyr)
library(lmerTest)
library(sjstats)
library(dplyr)
library(geepack)
library(lmtest)

cohort2_ids <- read.delim("D:/PHD/data/MDD_engagement/prepared_data/COHORT_2_FIRST_2Years.tsv",header = F)[,1]
participant.dates.flt <- participant.dates.flt%>% dplyr::filter(p_id %in% cohort2_ids) 
active.data.flt <- active.data.flt %>% dplyr::filter(p_id %in% cohort2_ids) 
passive.data.flt <- passive.data.flt %>% dplyr::filter(p_id %in% cohort2_ids) 
clin.assessment.data.flt <- clin.assessment.data.flt %>% dplyr::filter(p_id %in% cohort2_ids) 
demog.data.flt <- demog.data.flt %>% dplyr::filter(p_id %in% cohort2_ids) 


active.data.flt<-merge(active.data.flt, enrollment.info)
# find last active data in 660 and 688 observation period
last.active.data.in.period <- active.data.flt %>%
  dplyr::group_by(p_id) %>%
  dplyr::summarise(last_active_660 = max(timestamp[timestamp <= enrollment_date+days(660)]),
                   last_active_688 = max(timestamp[timestamp <= enrollment_date+days(688)]))

# select fitbit data
fitbit.data.flt <- passive.data.flt %>% dplyr::filter(dataName=="Passive_Fitbit")
fitbit.data.flt <- merge(fitbit.data.flt, enrollment.info)
# select phone data
phone.data.flt <- passive.data.flt %>% dplyr::filter(dataName=="Passive_phone") 
phone.data.flt <- merge(phone.data.flt, enrollment.info)
# find last fitbit data in 660 and 688 observation period
last.fitbit.data.in.period <- fitbit.data.flt %>%
  dplyr::group_by(p_id) %>%
  dplyr::summarise(last_fitbit_660 = max(timestamp[timestamp <= enrollment_date+days(660)]),
                   last_fitbit_688 = max(timestamp[timestamp <= enrollment_date+days(688)]))
# find last phone data in 660 and 688 observation period
last.phone.data.in.period <- phone.data.flt %>%
  dplyr::group_by(p_id) %>%
  dplyr::summarise(last_phone_660 = max(timestamp[timestamp <= enrollment_date+days(660)]),
                   last_phone_688 = max(timestamp[timestamp <= enrollment_date+days(688)]))

# merge data with demographic table
cohort <- merge(demog.data.flt,last.active.data.in.period,all.x = T)
cohort <- merge(cohort,last.phone.data.in.period,all.x = T)
cohort <- merge(cohort,last.fitbit.data.in.period,all.x = T)
cohort <- merge(cohort,enrollment.info,all.x = T)
all_id <- cohort$p_id
# Find the baseline PHQ-8
phq_baseline <- phq8.notf %>%
  dplyr::mutate(phq_score= s_1 +s_2+s_3+s_4+s_5+s_6+s_7+s_8)%>%
  dplyr::group_by(p_id)%>%
  dplyr::summarise(phq_base = phq_score[which.min(start_time)]) %>%
  dplyr::filter(p_id %in% all_id)
cohort <- merge(cohort,phq_baseline,all.x = T) 
cohort$daysTofirst<- as.Date(cohort$enrollment_date) - as.Date("2017-11-30 UTC")


# calculate the duration in study

cohort <- cohort %>% dplyr::mutate(duration_active_in_660 = as.Date(last_active_660) - as.Date(enrollment_date)+1,
                                   duration_active_in_688 = as.Date(last_active_688) - as.Date(enrollment_date)+1,
                                   duration_phone_in_660 = as.Date(last_phone_660) - as.Date(enrollment_date) ,
                                   duration_phone_in_688 = as.Date(last_phone_688) - as.Date(enrollment_date) ,
                                   duration_fitbit_in_660 = as.Date(last_fitbit_660) - as.Date(enrollment_date) ,
                                   duration_fitbit_in_688 = as.Date(last_fitbit_688) - as.Date(enrollment_date))

cohort <- cohort %>% dplyr::mutate(duration_active_in_660 = ifelse(is.na(duration_active_in_660) |is.infinite(duration_active_in_660) ,0,duration_active_in_660),
                                   duration_active_in_688 =ifelse(is.na(duration_active_in_688) |is.infinite(duration_active_in_688),0,duration_active_in_688),
                                   duration_phone_in_660 = ifelse(is.na(duration_phone_in_660)|is.infinite(duration_phone_in_660),0,duration_phone_in_660),
                                   duration_phone_in_688 = ifelse(is.na(duration_phone_in_688)|is.infinite(duration_phone_in_688),0,duration_phone_in_688),
                                   duration_fitbit_in_660 =ifelse(is.na(duration_fitbit_in_660)|is.infinite(duration_fitbit_in_660),0,duration_fitbit_in_660),
                                   duration_fitbit_in_688 =ifelse(is.na(duration_fitbit_in_688)|is.infinite(duration_fitbit_in_688),0,duration_fitbit_in_688))

# dead=1 censor = 0
cohort_censor <- cohort %>% dplyr::mutate(censorStatus_active = ifelse(duration_active_in_688<660,1,0),
                                          censorStatus_phone = ifelse(duration_phone_in_688<660,1,0),
                                          censorStatus_fitbit = ifelse(duration_fitbit_in_688<660,1,0))

# KM curve for all data
# active
fit1 <- survfit(Surv(cohort_censor$duration_active_in_660, event = censorStatus_active) ~ 1, data=cohort_censor)
ggsurvplot(fit1, data=cohort_censor)

# phone
fit2 <- survfit(Surv(cohort_censor$duration_phone_in_660, event = censorStatus_phone, type = "right") ~ 1, data=cohort_censor)
ggsurvplot(fit2, data=cohort_censor,surv.median.line = "hv")
# fitbit
fit3 <- survfit(Surv(cohort_censor$duration_fitbit_in_660, event = censorStatus_fitbit, type = "right") ~ 1, data=cohort_censor)
ggsurvplot(fit3, data=cohort_censor,surv.median.line = "hv")
# Combine 3 survival graphs
fitlist <- list(s1=fit1,s2=fit2,s3=fit3)
p <- ggsurvplot_combine(fitlist, data=cohort_censor,legend.title = "",
                        legend.labs=c('Phone-Active', 'Phone-Passive', 'Fitbit-Passive'),
                        palette=c('#1b9e77', '#d95f02', '#e7298a'),
                        conf.int = TRUE, pval=T,xlab = "Duration in study (Days)",legend = c(.15,.25))

p
ggsave("Figures/KM_corhor2_3_datastreams.png", plot=print(p), height = 6, width = 6, units="in", dpi=250)

# Cox-PH model
# age 
cohort_censor$age_cat = '>60' # >60
cohort_censor$age_cat[cohort_censor$age<=59] = '50-59' #50-59
cohort_censor$age_cat[cohort_censor$age<=49] = '40-49' # 40-49
cohort_censor$age_cat[cohort_censor$age<=39] = '30-39' # 30-39
cohort_censor$age_cat[cohort_censor$age<=29] = '<30' # <30

#phq base
# cohort_censor$phq_cat = 'NA' # na no records
cohort_censor$phq_cat[cohort_censor$phq_base>=0] = 'no depressive symptoms'  # no depressive symptoms
cohort_censor$phq_cat[cohort_censor$phq_base>=5] =  'mild'  # mild
cohort_censor$phq_cat[cohort_censor$phq_base>=10] = 'moderate' # moderate
cohort_censor$phq_cat[cohort_censor$phq_base>=15] = 'moderately severe' # moderately severe
cohort_censor$phq_cat[cohort_censor$phq_base>=20] = 'severe' # severe

# income num
cohort_censor$income_num[cohort_censor$income_num>=3] = 3 # combine >=3 to one cat

# order factors
cohort_censor$age_cat <- factor(cohort_censor$age_cat, levels=c('<30','30-39','40-49','50-59','>60'))

# remove na, not to say
cohort_censor$income_cat[cohort_censor$income_align=="15,000-55,000"] = "15,000-55,000"
cohort_censor$income_cat[cohort_censor$income_align=="more than 55000"] = "more than 55000"
cohort_censor$income_cat[cohort_censor$income_align=="below minimum"] = "below minimum"
# acc_cat
cohort_censor$accommodation_cat[cohort_censor$accommodation_occupy_align=="Own outright/with mortgage"]="Own outright/with mortgage"
cohort_censor$accommodation_cat[cohort_censor$accommodation_occupy_align=="Renting"]="Renting"
cohort_censor$accommodation_cat[cohort_censor$accommodation_occupy_align=="Living rent-free"]="Living rent-free"

cohort_censor$marry_align[cohort_censor$marry_align=="Married/cohabiting/Long term relationship"] = "Married"
cohort_censor$marry_align[cohort_censor$marry_align=="Single/separated/divorced/widowed"] = "Single"
# edu cat
summary(cohort_censor$edu_age_align)
levels(cohort_censor$children)
cohort_censor$children_cat[cohort_censor$children=="Yes"] = "Yes"
cohort_censor$children_cat[cohort_censor$children=="No"] = "No"
#
cohort_censor$accommodation_satisfy <- as.factor(cohort_censor$accommodation_satisfy)
cohort_censor$accommodation_satisfy <- factor(cohort_censor$accommodation_satisfy, levels = c("Very satisfied" ,"Fairly satisfied","Neither satisfied nor dissatisfied","Slightly dissatisfied" ,"Very dissatisfied"))

cohort_censor$income_cat <- factor(cohort_censor$income_cat, levels = c("below minimum" ,"15,000-55,000","more than 55000"))
cohort_censor$accommodation_cat <- factor(cohort_censor$accommodation_cat, levels = c("Own outright/with mortgage" ,"Renting","Living rent-free"))
cohort_censor$marry_align <- factor(cohort_censor$marry_align, levels = c("Single" ,"Married"))

# 
cohort_censor$employment_cat[cohort_censor$employment == "Yes"] = "Yes"
cohort_censor$employment_cat[cohort_censor$employment == "No"] = "No"

cohort_censor$employment_cat = factor(cohort_censor$employment_cat)
# cohort_censor$employment_cat[cohort_censor$employment == "Employed"] = "Employed"
# cohort_censor$employment_cat[cohort_censor$employment == "Unemployed"] = "Unemployed"
# cohort_censor$employment_cat[cohort_censor$employment == "Student"] = "Student"
# cohort_censor$employment_cat[cohort_censor$employment == "Retired"] = "Retired"
# cohort_censor$employment_cat = factor(cohort_censor$employment_cat)

cohort_censor$comorb_cat <- factor(cohort_censor$comorb)
cohort_censor$depression_medication_cat <-factor(cohort_censor$depression_medication)

cohort_censor$phone_brand <- factor(cohort_censor$phone_brand, levels = c("Other" ,"Motorola", "Samsung"))
chisq.test(cohort_censor$age_cat,cohort_censor$employment_cat)

# race subgroup
# cohort_censor <- cohort_censor %>%
#   filter(site != "ciber")

# cohort_censor$ETHCAT2 <- factor(cohort_censor$ETHCAT2, levels = c("White native", "White Other", "Asian ethnic group", "Black ethnic group", "Mixed ethnic background", "Other"))
# cohort_censor$eth_cat="Other"
# cohort_censor$eth_cat[cohort_censor$ETHCAT2 =="White native"] = "White native"
# cohort_censor$eth_cat <- factor(cohort_censor$eth_cat)
# phone status
# cohort_censor$phone_status_cat <- factor(cohort_censor$phone_status)
cohort_censor$phone_status_cat = 0
cohort_censor$phone_status_cat[cohort_censor$phone_status==0 | cohort_censor$phone_status==3] = 0
cohort_censor$phone_status_cat[cohort_censor$phone_status==1] = 1
#cohort_censor$phone_status_cat[cohort_censor$phone_status==2] = 2
cohort_censor$phone_status_cat = factor(cohort_censor$phone_status_cat)
# active
res.cox <- coxph(Surv(duration_active_in_660, event = censorStatus_active) ~  age_cat+sex+ marry_align+employment_cat+children_cat+edu_age_align +
                   income_cat +accommodation_cat  +phq_base+comorb_cat +  depression_medication_cat+site + phone_status + phone_brand , data = cohort_censor)
summary(res.cox)
cox.zph(res.cox)
plot(cox.zph(res.cox))
ggforest(res.cox,data = cohort_censor)

plot(cox.zph(res.cox)[1])
cohort_censor_spilt <- survSplit(Surv(duration_active_in_660,censorStatus_active)~.,
                                 data = cohort_censor, cut =c(320),episode = "tgroup",id="id",zero = -1)
cohort_censor_spilt$age_cat <- factor(cohort_censor_spilt$age_cat, levels = c('30-39','40-49','50-59','>60','<30'))
res.cox <- coxph(Surv(duration_active_in_660, event = censorStatus_active) ~age_cat:strata(tgroup)+sex+ marry_align+employment_cat+children_cat+edu_age_align +
                   income_cat +accommodation_cat  +phq_base+comorb_cat +  depression_medication_cat+site + phone_status + phone_brand , data = cohort_censor_spilt)
summary(res.cox)
cox.zph(res.cox)

# output results

hr_result <- summary(res.cox)[["conf.int"]]
hr_result_p <- summary(res.cox)[["coefficients"]]
result =  matrix(nrow=nrow(hr_result),ncol=3)
result[,1] = row.names(hr_result)
for (i in 1:nrow(hr_result)){
  result[i,2] = paste0(round(hr_result[i,1],2)," (", round(hr_result[i,3], 2),"-", round(hr_result[i,4],2),")" )
  result[i,3] = round(hr_result_p[i,5], 2)
}


# passive
res.cox <- coxph(Surv(duration_phone_in_660, event = censorStatus_phone)  ~  age_cat+sex+ marry_align+employment_cat+children_cat+edu_age_align +
                   income_cat +accommodation_cat  +phq_base+comorb_cat +  depression_medication_cat+site + phone_status+phone_brand , data = cohort_censor)
summary(res.cox)
cox.zph(res.cox)
ggforest(res.cox,data = cohort_censor)
# output results
hr_result <- summary(res.cox)[["conf.int"]]
hr_result_p <- summary(res.cox)[["coefficients"]]
result =  matrix(nrow=nrow(hr_result),ncol=3)
result[,1] = row.names(hr_result)
for (i in 1:nrow(hr_result)){
  result[i,2] = paste0(round(hr_result[i,1],2)," (", round(hr_result[i,3], 2),"-", round(hr_result[i,4],2),")" )
  result[i,3] = round(hr_result_p[i,5], 2)
}


# fitbit
res.cox <- coxph(Surv(duration_fitbit_in_660, event = censorStatus_fitbit)  ~  age_cat+sex+ marry_align+employment_cat+children_cat+edu_age_align +
                   income_cat +accommodation_cat  +phq_base+comorb_cat +  depression_medication_cat+site +phone_status+ phone_brand , data = cohort_censor)
summary(res.cox)
cox.zph(res.cox)
ggforest(res.cox,data = cohort_censor)
plot(cox.zph(res.cox)[5])
cohort_censor_spilt <- survSplit(Surv(duration_fitbit_in_660,censorStatus_fitbit)~.,
                                 data = cohort_censor, cut =c(450),episode = "tgroup",id="id",zero = -1)
cohort_censor_spilt$children_cat <- factor(cohort_censor_spilt$children_cat, levels = c("Yes","No"))
res.cox <- coxph(Surv(duration_fitbit_in_660, event = censorStatus_fitbit) ~  age_cat+sex+ marry_align+employment_cat+children_cat:strata(tgroup)+edu_age_align +
                   income_cat +accommodation_cat  +phq_base+comorb_cat +  depression_medication_cat+site + phone_status + phone_brand , data = cohort_censor_spilt)
summary(res.cox)
cox.zph(res.cox)
# output results

hr_result <- summary(res.cox)[["conf.int"]]
hr_result_p <- summary(res.cox)[["coefficients"]]
result =  matrix(nrow=nrow(hr_result),ncol=3)
result[,1] = row.names(hr_result)
for (i in 1:nrow(hr_result)){
  result[i,2] = paste0(round(hr_result[i,1],2)," (", round(hr_result[i,3], 2),"-", round(hr_result[i,4],2),")" )
  result[i,3] = round(hr_result_p[i,5], 2)
}

