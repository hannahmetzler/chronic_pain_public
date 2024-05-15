#load R-libraries (already in analysis_report.Rmd)
# library(dplyr)
# library(tidyr)

#choose analysis ####

analysis="LOCF"
#PP: Per protocol (keep only complete cases).
#LOCF: Intention to treat: last observation carried forward.  
#AV: All values: Keep all existing values per time point

# maybe check for dealing with missing data: http://juliejosse.com/wp-content/uploads/2018/06/DataAnalysisMissingR.html#11)_descriptive_statistics_with_missing_values

#read and format data ###

dt0t1 <- read.csv2('data/data_pain_t0t1.csv', header=T)
d_read <- read.csv2('data/data_pain_t0t2t3.csv', header=T)
head(d_read)

#format variables
d_all <- d_read %>%
  mutate(subid = factor(subid), 
         date = as.Date(date, format = "%m/%d/%Y"),
         month = format(date, "%m"), 
         group = factor(group, levels=c(0,1), labels = c("control", "treatment")),
         gender = factor(gender, levels = c(0,1,999), labels = c("men", "women", "Not assessed")),
         education = factor(education, order = TRUE, levels = c(1:5, 999), 
                            labels = c("None", "Junior high school", "Vocational school", "High school", "University", "Not assessed")),
         occupation = factor(occupation, levels = c(1:8,999), 
                             labels = c("Pupil/Student", "Retraining", "Worker", "Employee", "Self-employed", "House wife/husband", "Unemployed", "Retired","Not assessed")),
         marital_status = factor(marital_status, levels = c(1:6,999), 
                                 labels = c("Single", "Married", "Partnership", "Living separately", "Divorced", "Widowed","Not assessed")),
         income = factor(income, levels = c(1:3,999), labels = c("Below 800 Euro", "800-1800 Euro", "Above 1800 Euro", "Not assessed")),
         debt = factor(debt, levels = c(0:3,999), labels = c("No", "Below 10.000 Euro", "10.000-20.000 Euro","Above 20.000 Euro", "Not assessed")),
         pain_family_member = factor( pain_family_member, levels = c(0,1,999), labels = c("No", "Yes","Not assessed")),
         pain_duration_years = as.integer(pain_duration_years),
         pain_subj_control = factor(pain_subj_control, order = TRUE, levels = c(1:5,999)),
         #are you currently on sick leave
         sick_leave = factor(sick_leave, levels=c(0,1,999),labels = c("yes", "no", "Not assessed")),
         #are you planning to apply or have you applied for retirement?
         retired = factor(retired, levels = c(0:4,999), 
                                  labels=c("No", "I will apply", "I have applied", "Retired for limited time", "Retired definitively", "Not assessed")),
         
         #expectations for (T0) or success of (T3) therapy (medical and psychological): 1=none, 5 = high expectations 
         #expectation.success_medt = factor(expectation.success_medt, order = TRUE, levels = c(1:5,999)),
         #expectation.success_psyt = factor(expectation.success_psyt, order = TRUE, levels = c(1:5,999)),
         #cpsq = factor(cpsq, order = TRUE, levels = c(1:4)),#subjektive Beeinträchtigung (4 Level)
         #mpss_stage = factor(mpss_stage, order = TRUE, levels = c(1:3,999), labels=c(1,2,3,"Not assessed")), #Schmerz-Chronifizierung (3 Stadien)
         season=factor(ifelse(month %in% c("01", "02", "03"), 1,
                              ifelse(month %in% c("04", "05","06"), 2,
                                     ifelse(month %in% c("07", "08", "09"), 3,
                                            ifelse(month %in% c("10", "11", "12"), 4, NA)))),
                       levels=c(1:4), labels=c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec")))
# write.csv2(d_read, file='./data/data_pain_t0t2t3.csv', row.names=FALSE)
# save(d_read, file='./data/data_pain_t0t2t3.RData')


# #combine similar categories into 1 for marital status and occupation
# d_all <- d_all  %>%
#   #married and partnership into one category
#   mutate(marital_status = recode(marital_status, "Married"="Married/Partnership", "Partnership"="Married/Partnership", 
#                                  "Living separately"="Living separately/Divorced", "Divorced"="Living separately/Divorced"))%>%
#   #worker and employee into one category
#   mutate(occupation = recode(occupation, "Worker"="Worker/Employee", "Employee"="Worker/Employee"))


#filtering according to analysis type ####

if(analysis =="PP") {
  print("Per protocol analysis Life quality\n
        - Further NAs need to be excluded for analysis of:\n
        pain_family_member, specialist_visits, lost_days,  expectation.success_medt or _psyt, pt_motivation, soms_n and soms_intensity")
  #per protocol: keep only complete cases
  #d <- d_all[complete.cases(d_all),] #this filters all complete cases per time point, but subjects with missing values at T3 still exist at T2
  d <- d_all %>%
    group_by(subid)%>%
    filter(!any(is.na(lq_phys))) #if any lq_phys value is empty, delete the subject
  n_pp <- n_distinct(d$subid); n_pp #23 subjects of 38 left
  
  #data with 1 time point (for all variables that stay the same, like gender, education and to look at each time point separately)
  d0 <- d %>%
    filter(time=="T0")%>%
    droplevels()
  d2 <- d%>%
    filter(time=="T2")%>%
    droplevels()
  d3 <- d%>%
    filter(time=="T3")%>%
    droplevels()  
  #time point 0 and 3
  d03 <- d %>%
    filter(time!="T2")%>%
    droplevels()
  d02 <- d %>%
    filter(time!="T3")%>%
    droplevels()

} else if (analysis =="LOCF") {
  print("LOCF analysis")
  #intention to treat, version last observation carried forward: replace empty values with last value
  d <-d_all %>%
    group_by(subid)%>%
    #for all variables assessed at 3 time points: 
    fill(lq_phys:wi_hypochondria) %>% # (list =x:y) before, fill missing values in all variables from lq_phys to mpss_stage with last available value
    ungroup() 
  n_locf <- n_distinct(d$subid); n_locf
  
  #data with 1 time point (for all variables that stay the same, like gender, education)
  d0 <- d %>%
    filter(time=="T0")%>%
    droplevels()
  # #careful: this T2 and T3 data frame only include replaced values for 
  # #c("lq_phys", "lq_psych", "lq_social", "lq_envir", "lq_overall", "cpsq", "vas_pain_intensity", "soms_n", "soms_intensity", "hads_anxiety", "hads_depression", "wi_hypochondria")
  # #all others (occupation, specialist_visits:expectation.success_psyt, mpss_stage, pt_motivation) need to be replaced if only the d2 or d3 data frame is analysed
  # d2 <- d%>% 
  #    filter(time=="T2")%>%
  #   droplevels()
  # d3 <- d%>%
  #   filter(time=="T3")%>%
  #   droplevels()  
  #time point 0 and 3
  d03 <- d_all %>%
    filter(time!="T2")%>%
    group_by(subid)%>%
    fill(occupation, specialist_visits:expectation.success_psyt, mpss_stage)%>%
    dplyr::select(subid, time, group, gender, occupation, mpss_stage, expectation.success_medt, expectation.success_psyt, specialist_visits, lost_days)%>%
    arrange(time, subid)%>%
    ungroup()%>%
    droplevels()
  d02 <- d_all %>%
    filter(time!="T3")%>%
    group_by(subid)%>%
    fill(pt_motivation)%>%
    arrange(time, subid)%>%
    ungroup()%>%
    droplevels()
  
} else if(analysis == "AV"){
  #data with 1 time point (for all variables that stay the same, like gender, education and to look at each time point separately)
  d0 <- d_all %>%
    filter(time=="T0")%>%
    droplevels()
  d2 <- d_all %>%
    filter(time=="T2")%>%
    droplevels()
  d3 <-  d_all %>%
    filter(time=="T3")%>%
    droplevels()  
  #time point 0 and 3
  d03 <- d_all %>%
    filter(time!="T2")%>%
    droplevels()
  d02 <- d_all %>%
    filter(time!="T3")%>%
    droplevels()
}

# #checks: 
# as.data.frame(cbind(spread(d_all03[, c("subid", "time", "lost_days")], key=time, value=lost_days), spread(d03[,c("lost_days","subid", "time")], key=time, value=lost_days)))
# summary(d)
# xtabs(~time +lost_days, data=d)
# as.data.frame(d[1:nrow(d),])
# as.data.frame(d[30:nrow(d),])
# as.data.frame(d[59:nrow(d),])

# calculate differences between time points ####

#calculate differences between time points for all T0-T2-T3 variables 
diffs <- d %>%
  arrange(subid, time)%>%
  group_by(subid)%>%
  mutate_at(vars(pt_motivation:lq_overall, vas_pain_intensity:wi_hypochondria),#variables for which diff is to be calculated
            list(diff = ~c(diff(.), NA))) %>% #diff subtracts 2nd-1st and puts it in T0 lines, subtracts 3rd-2nd and puts in T2 lines, leaves NA in T3 lines
  #delete time point variables (keep only differences)
  dplyr::select(-c(lq_phys, lq_psych, lq_social, lq_envir, lq_overall, vas_pain_intensity, soms_n, soms_intensity, hads_anxiety, hads_depression, mpss_stage, wi_hypochondria))%>%
  ungroup()%>% 
  filter(time !="T3") %>% #delete lines with NAs, i.e. all T3 lines
  mutate(diff = factor(ifelse(time == "T0", "T2-T0", "T3-T2"))) %>% #rename time to time differences
  dplyr::select(-c(time))#delete time point variable

diffs <- diffs[,c(1,2,ncol(diffs),4:ncol(diffs)-1)] #reorder columns: put the "diffs" variable (levels: T2-T0 and T3-T2) in 3rd column

#calculate diffs for continous variables assessed at T0-T3
diffs03 <- d03 %>%
  arrange(subid,time)%>%
  group_by(subid)%>%
  mutate_at(vars(specialist_visits:lost_days),#variables for which diff is to be calculated
            list(diff03 = ~c(diff(.), NA))) %>% #diff subtracts 2nd-1st and puts it in T0 lines, leaves NA in T2 lines
  #delete time point variables (keep only differences)
  dplyr::select(c(subid, group, gender, specialist_visits_diff03:lost_days_diff03))%>%
  ungroup()%>% 
  na.omit  #delete lines with NAs

#calculate diffs for continous variables assessed at T0-T2: pt_motivation
diffs02 <- d02 %>%
  arrange(subid,time)%>%
  group_by(subid)%>%
  mutate(pt_motivation = diff(pt_motivation))%>%
  dplyr::select(c(subid, pt_motivation, group))%>%
  filter(row_number()==1)%>%
  ungroup()

