# summary demographics of all samples and 3 study sites
library(data.table)
library(psych)
library(plyr)
library(lmerTest)
library(sjstats)
library(dplyr)

demo <-  demog.data.flt
baseline.phq8 <- phq8.notf %>% dplyr::filter(week %in% c(1,2)) %>% 
  select(p_id, sum_phq8) %>% 
  dplyr::group_by(p_id) %>%
  dplyr::summarise(sum_phq8 = max(sum_phq8))

contact.log.data.select <- merge(contact.log.data,enrollment.info)
contact.log.data.select$days <- contact.log.data.select$date-as.Date(contact.log.data.select$enrollment_date) + 1 
contact.log.data.select <-contact.log.data.select %>% 
  dplyr::filter(days<=301)%>%
  dplyr::group_by(p_id) %>%
  dplyr::summarise(num_contact = max(record_num))%>%
  dplyr::mutate(num_contact = ifelse(is.na(num_contact), 0, num_contact))

demo <- merge(demo, baseline.phq8,all.x = T)
demo <- merge(demo, enrollment.info, all.x = T)
demo <- merge(demo, contact.log.data.select, all.x = T)

colnames(demo)
listVars <- c("sum_phq8","delta.enrol.vs.study_start","num_contact","age", "sex","marry_align","children","edu_age_align" ,"income_align", "accommodation_occupy_align","accommodation_satisfy")
#Define categorical variables
catVars <- c("sex","marry_align","children","income_align","accommodation_occupy_align","accommodation_satisfy")
tabResults <- tableone::CreateTableOne(data=demo, vars=listVars, 
                                       factorVars = catVars, 
                                        includeNA = F)
demo_table <- print(tabResults, nonnormal = listVars, 
                    formatOptions = list(big.mark = ","))
write.csv(demo_table, 
          file = "Tables/Demo_cohort1_all.csv")

tabResults <- tableone::CreateTableOne(data=demo, vars=listVars, 
                                       factorVars = catVars, 
                                       strata=c("site"),
                                       includeNA = F)
demo_table2 <- print(tabResults, nonnormal = listVars,
                    formatOptions = list(big.mark = ","))

write.csv(demo_table2, 
          file = "Tables/Demo_cohort1_sites.csv")

# number of total, kcl, ciber, vumc
sum(demo$site == "kcl"|demo$site == "ciber"|demo$site == "vumc") # total
sum(demo$site == "kcl") # KCL
sum(demo$site == "ciber") # CIBER
sum(demo$site == "vumc") # VUmc
## age 
# total
mean(demo$age)
sd(demo$age)
# kcl
mean(demo$age[demo$site=="kcl"])
sd(demo$age[demo$site=="kcl"])
# ciber
mean(demo$age[demo$site=="ciber"])
sd(demo$age[demo$site=="ciber"])
# vumc
mean(demo$age[demo$site=="vumc"])
sd(demo$age[demo$site=="vumc"])

## Female
# total
sum(demo$sex=="female")
# kcl
sum(demo$sex[demo$site=="kcl"]=="female")
# ciber
sum(demo$sex[demo$site=="ciber"]=="female")
# vumc
sum(demo$sex[demo$site=="vumc"]=="female")

## Marital status
summary(demo$marry_align) # total
summary(demo$marry_align[demo$site=="kcl"])
summary(demo$marry_align[demo$site=="ciber"])
summary(demo$marry_align[demo$site=="vumc"])

## children
summary(demo$children)
summary(demo$children[demo$site=="kcl"])
summary(demo$children[demo$site=="ciber"])
summary(demo$children[demo$site=="vumc"])

## edu age
mean(demo$edu_age_align)
sd(demo$edu_age_align)
mean(demo$edu_age_align[demo$site=="kcl"])
sd(demo$edu_age_align[demo$site=="kcl"])
mean(demo$edu_age_align[demo$site=="ciber"])
sd(demo$edu_age_align[demo$site=="ciber"])
mean(demo$edu_age_align[demo$site=="vumc"])
sd(demo$edu_age_align[demo$site=="vumc"])

## edu degree
summary(demo$edu_degree_align)
summary(demo$edu_degree_align[demo$site=="kcl"])
summary(demo$edu_degree_align[demo$site=="ciber"])
summary(demo$edu_degree_align[demo$site=="vumc"])

## accomodation type
summary(demo$accommodation_occupy_align)
summary(demo$accommodation_occupy_align[demo$site=="kcl"])
summary(demo$accommodation_occupy_align[demo$site=="ciber"])
summary(demo$accommodation_occupy_align[demo$site=="vumc"])
## accomodation satisfaction
summary(demo$accommodation_satisfy)
summary(demo$accommodation_satisfy[demo$site=="kcl"])
summary(demo$accommodation_satisfy[demo$site=="ciber"])
summary(demo$accommodation_satisfy[demo$site=="vumc"])
## Household income
summary(demo$income_align)
summary(demo$income_align[demo$site=="kcl"])
summary(demo$income_align[demo$site=="ciber"])
summary(demo$income_align[demo$site=="vumc"])
## Number of contrubutors to income
summary(as.factor(demo$income_num))
summary(as.factor(demo$income_num[demo$site=="kcl"]))
summary(as.factor(demo$income_num[demo$site=="ciber"]))
summary(as.factor(demo$income_num[demo$site=="vumc"]))

# Kruskal test
kruskal.test(demo$age, demo$site)
kruskal.test(demo$sex, demo$site)
kruskal.test(demo$marry_align, demo$site)
kruskal.test(demo$children, demo$site)
kruskal.test(demo$edu_age_align, demo$site)
kruskal.test(demo$edu_degree_align, demo$site)
kruskal.test(demo$accommodation_occupy_align, demo$site)
kruskal.test(demo$accommodation_satisfy, demo$site)
kruskal.test(demo$income_align, demo$site)
kruskal.test(demo$income_num, demo$site)
