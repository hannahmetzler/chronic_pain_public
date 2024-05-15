#descriptive analysis

#load libraries
# library(psych)#for descriptives by group describeBy
# library(pastecs) #for stat.desc
# # library(tidyr)
# library(gmodels)#for high and low ci thresholds
# library(ggplot2)

#turn of scientific notation of numbers as x e01
options(scipen=999)

#load formatted data from last script
# source("scripts/01_prepare_data.R")

#do you want to write tables to Excel or not? 
excel = TRUE # TRUE or FALSE
figures = FALSE


#n total and n in each group ####
ntot = count(d0)[[1]] 
ncontrol = count(subset(d0, group=="control"))[[1]]
ntreatment = count(subset(d0, group=="treatment"))[[1]]

# categorical socio-economic variables at T0 ####

# extract all variable names except subid, time and group
names_catvarst0 <- names(d0[unlist(lapply(d0, is.factor))] %>% dplyr::select(-c(subid, group)))

#count cases in each category per group
count.tables <- lapply(d0[,names_catvarst0], function(x) ftable(xtabs(~ x + d0$group)))

#this function does the same but with variable name as row header, 
#function downloaded from: https://rstudio-pubs-static.s3.amazonaws.com/1208_c6418af16d8347f7a4dd3ece7979f37c.html
multi.xtabs <- function(df, vars, group.var, ...) {
  sapply(simplify = FALSE,
         vars,                                                        # Loop for each element of vars
         function(var) {
           formula <- as.formula(paste("~", var, "+", group.var))   # Create formula
           xtabs(data = df, formula, ...)                           # Give formula to xtabs
         }
  )
}

#count cases in each category per group with above function
count.tables <- multi.xtabs(df = d0, vars = names_catvarst0, group.var = "group")

#transfrom frequency into proportion
prop.tables <- lapply(count.tables, prop.table, margin = 2)

# add count and proportion tables together
t <- lapply(count.tables, as.data.frame)
p <- lapply(prop.tables, as.data.frame)

tp <- list()
for (i in seq_along(t)){ 
  #t and p both have column name "Freq" for 3rd column, change it to "Proportion" in p
  colnames(p[[i]])[3] <- "Percent"
  
  # Proportion to Percent, and round to 2 digits
  p[[i]][3] <- round(p[[i]][3]*100, digits=2)
  
  #join t and p into one data.frame within the list
  tp[[i]] <- inner_join(t[[i]], p[[i]])
  
  #same name for all variable columns, so they can be joined into one data.frame
  colnames(tp[[i]])[1] <- "Categories"
  colnames(tp[[i]])[3] <- "Frequency"
  tp[[i]][4] <- round(tp[[i]][4])
  
  #order by Category instead of group (for narrow table with control/treatment in seperate lines)
  tp[[i]] <- tp[[i]] %>%
    arrange(Categories)
  
  # for wide table with control/treatment in separate columns for both frequency and proportion)
  # tp[[i]] <- tp[[i]] %>% 
  #   gather(variable, value, c(Frequency, Percent)) %>%
  #   unite(temporary, group, variable) %>%
  #   spread(temporary, value)
  # 
  # #change column order (group Frequency and Propotion together, instead of control and treatment together)
  # tp[[i]]  <-tp[[i]] [,c("Categories", "control_Frequency", "treatment_Frequency", "control_Percent","treatment_Percent")]
  # 
}

#for writing to excel: 
if(excel) {

  #levels of all categorical variables
  nlev <- sapply(d0[,names_catvarst0], function(factor) length(levels(factor)))
  
  #repeat each name as often as the factor has levels (* 2 for both groups for narrow table, but not wide table format)
  Variable <- rep(names_catvarst0, nlev*2)
  
  # add variable name in first colun and join all dataframes in tp into one dataframe
  table_freq <- cbind(Variable, do.call(rbind, tp))
  #adapt this for Table format Julia wants: brackets and percent, all in one column
  freq_csv <- table_freq
  freq_csv$Percent <- paste("(", freq_csv$Percent, "%)", sep="") #y only put so csv2 can read the brackets - delete in excel
  freq_csv$Percent <- paste(freq_csv$Frequency, freq_csv$Percent)
  freq_csv$Frequency <- NULL
  
  write.csv2(x=subset(freq_csv, group=="control"), file = "output/T0Categorical_variables_control.csv", row.names=F)
  write.csv2(x=subset(freq_csv, group=="treatment"), file = "output/T0Categorical_variables_treatment.csv", row.names=F)
}



#for RMarkdown: name the dataframes in tp
names(tp) <- names_catvarst0

# continous variables assessed only at T0 ####
#complete descriptives by group, CIs: t-distributed (not normally distributed)
#age norm test ok
by(d0$age, d0$group, function(X)  round(stat.desc(X, norm = TRUE, basic = TRUE), digits=2))
if(figures) {ggplot(d0, aes(x=age, fill=group))+  geom_histogram(binwidth= 5, position="dodge")}

#descriptives
desc_age <- d0 %>%
  group_by(group, time)%>%
  summarise(n=n(), 
            mean = round(mean(age), 2), 
            median = round(median(age),2), 
            sd = round(sd(age), 2), 
            min = round(min(age), 2), 
            max = round(max(age), 2), 
            lowCI = round(mean-1.96*sd/sqrt(n()),2), 
            hiCI = round(mean+1.96*sd/sqrt(n()),2)); desc_age
if(excel) {write.csv2(x=t(desc_age), file = "output/age.csv")}

#pain duration norm test significant!  No NV
if(figures) {ggplot(d0, aes(x=pain_duration_years, fill=group))+  geom_histogram(position="dodge")}
by(d0$pain_duration_years, d0$group, function(X)  round(stat.desc(X, norm = TRUE, basic = TRUE), digits=2))
quant_painyears <- d0 %>%  
  group_by(time, group) %>% 
  summarise(n= n(), 
            Q25 = quantile(pain_duration_years, probs=0.25),
            Q50 = quantile(pain_duration_years, probs=0.5),
            Q75 = quantile(pain_duration_years, probs=0.75),
            Q100 = quantile(pain_duration_years, probs=1))
if(excel) {write.csv2(x=t(quant_painyears), file = "output/pain_duration_years.csv")}


#categorical variables that change from T0-T3: expectations and evaluation of therapy

quant_psyt <- d03 %>% 
  #exclude subjects with NAs at T0
  group_by(subid)%>%
  mutate(nas = sum(is.na(expectation.success_psyt))) %>% #sum of NAs per subject
  filter(nas == 0)%>% #only subjects without NA for any time point
  group_by(time, group) %>% 
  summarise(n= n(), 
            Q25 = quantile(expectation.success_psyt, probs=0.25),
            Q50 = quantile(expectation.success_psyt, probs=0.5),
            Q75 = quantile(expectation.success_psyt, probs=0.75),
            Q100 = quantile(expectation.success_psyt, probs=1))%>%
  ungroup()%>%
  filter(row_number()!=3)
quant_medt <- d03 %>% 
  #exclude subjects with NAs at T0
  group_by(subid)%>%
  mutate(nas = sum(is.na(expectation.success_medt))) %>% #sum of NAs per subject
  filter(nas == 0)%>% #only subjects without NA for any time point
  group_by(time, group) %>% 
  summarise(n= n(), 
            Q25 = quantile(expectation.success_medt, probs=0.25),
            Q50 = quantile(expectation.success_medt, probs=0.5),
            Q75 = quantile(expectation.success_medt, probs=0.75),
            Q100 = quantile(expectation.success_medt, probs=1)); quant_medt


# categorical variables that change from T0-T2-T3: mpss and cpsq ####

#calculate proportion for mpss
mpss_prop <- d03 %>%
  group_by(group, time, mpss_stage) %>%
  summarise(nresp = n()) %>%
  mutate(proportion = round(nresp/sum(nresp)*100)) %>%
  ungroup()%>%
  dplyr::select(-nresp); mpss_prop
if(excel) {write.csv2(mpss_prop, file="output/mpss_proportions.csv", row.names=F)}

#calculate proportion for cpsq
cpsq_prop <- d %>%
  group_by(group, time, cpsq) %>%
  summarise(nresp = n()) %>%
  mutate(proportion = round(nresp/sum(nresp)*100)) %>%
  ungroup()%>%
  dplyr::select(-nresp); cpsq_prop
if(excel) {write.csv2(cpsq_prop, file="output/cpsq_proportions.csv",row.names=F)}

# Continous variables assessed at T0 and T3: specialist visits, lost days ####

#specialist_visits - no NV distribution
by(d03$specialist_visits, d03$group, function(X)  round(stat.desc(X, norm = TRUE, basic = TRUE), digits=2))
if(figures) {ggplot(d03, aes(x=specialist_visits, fill=group))+  geom_histogram(position="dodge")}
#quantiles as descriptives
quant_visits <- d03 %>%  
  group_by(time, group) %>% 
  summarise(n= n(), 
            Q25 = quantile(specialist_visits, probs=0.25),
            Q50 = quantile(specialist_visits, probs=0.5),
            Q75 = quantile(specialist_visits, probs=0.75),
            Q100 = quantile(specialist_visits, probs=1), 
            mean = mean(specialist_visits), 
            SD = sd(specialist_visits))
if(excel) {write.csv2(x=t(quant_visits), file = "output/specialist_visits.csv")}

#change scores 
change_sickness_behav_desc <- diffs03 %>%  
  group_by(group) %>% 
  summarise_at(c("specialist_visits_diff03", "lost_days_diff03"), list(~mean(.), ~sd(.)))
change_sickness_behav_desc <- lsr::tFrame(change_sickness_behav_desc) 
names(change_sickness_behav_desc)<- c("Group", "Group")


# lost_days  - no NV
by(d03$lost_days, d03$group, function(X)  round(stat.desc(X, norm = TRUE, basic = TRUE), digits=2))
if(figures) {ggplot(d03, aes(x=lost_days, fill=group))+  geom_histogram(bins = 15, position="dodge")}
#quantiles as descriptives
quant_lostwd <- d03 %>% 
  #exclude subjects with NAs at T0
  group_by(subid)%>%
  mutate(nas = sum(is.na(lost_days))) %>% #sum of NAs per subject
  filter(nas == 0)%>% #only subjects without NA for any time point
  #calculate quantiles per group and time
  group_by(time, group) %>% 
  summarise(n= n(), 
            Q25 = quantile(lost_days, probs=0.25),
            Q50 = quantile(lost_days, probs=0.5),
            Q75 = quantile(lost_days, probs=0.75),
            Q100 = quantile(lost_days, probs=1)); quant_lostwd
if(excel) {write.csv2(x=t(quant_lostwd), file = "output/lost_days.csv")}

# continous variables T0-T2: psychotherapy motivation ####

# pt motivation: NV is ok
dptm <- d02 %>%
  #exclude subjects with NAs at T0
  group_by(subid)%>%
  mutate(nas = sum(is.na(pt_motivation))) %>% #sum of NAs per subject
  filter(nas == 0)%>%#only subjects without NA for any time point
  droplevels()

if(figures) {ggplot(dptm, aes(x=pt_motivation, fill=group))+  geom_histogram(binwidth=15, position="dodge")}
by(dptm$pt_motivation, paste(dptm$group, dptm$time), function(X)  round(stat.desc(X, norm = TRUE, basic = TRUE), digits=2))

desc_pt_mot <- dptm %>%
  #calculate quantiles per group/time
  group_by(group, time)%>%
  summarise(n=n(), 
            mean = round(mean(pt_motivation), 2), 
            median = round(median(pt_motivation),2), 
            sd = round(sd(pt_motivation), 2), 
            min = round(min(pt_motivation), 2), 
            max = round(max(pt_motivation), 2), 
            lowCI = round(mean-1.96*sd/sqrt(n()),2), 
            hiCI = round(mean+1.96*sd/sqrt(n()),2)); desc_pt_mot
if(excel) {write.csv2(x=t(desc_pt_mot), file = "output/pt_motivation.csv")}

# continuous variables that change from T0-T2-T3 ####

#soms pain intensity
#descriptives
desc_somsintensity <- d %>%
  group_by(group, time)%>%
  summarise(n=n(), 
            mean = round(mean(soms_intensity), 2), 
            median = round(median(soms_intensity),2), 
            sd = round(sd(soms_intensity), 2), 
            min = round(min(soms_intensity), 2), 
            max = round(max(soms_intensity), 2), 
            lowCI = round(mean-1.96*sd/sqrt(n()),2), 
            hiCI = round(mean+1.96*sd/sqrt(n()),2));  desc_somsintensity

#soms number of symptoms
desc_somsn <- d %>%
  group_by(group, time)%>%
  summarise(n=n(), 
            mean = round(mean(soms_n), 2), 
            median = round(median(soms_n),2), 
            sd = round(sd(soms_n), 2), 
            min = round(min(soms_n), 2), 
            max = round(max(soms_n), 2), 
            lowCI = round(mean-1.96*sd/sqrt(n()),2), 
            hiCI = round(mean+1.96*sd/sqrt(n()),2));  desc_somsn

#vas pain intensity 
#NV ok
if(figures) {ggplot(d, aes(x=vas_pain_intensity, fill=group))+  geom_histogram(binwidth=2, position="dodge")}
by(d$vas_pain_intensity, paste(d$group, d$time), function(X)  round(stat.desc(X, norm = TRUE, basic = TRUE), digits=2))
#descriptives
desc_vas <- d %>%
  group_by(group, time)%>%
  summarise(n=n(), 
            mean = round(mean(vas_pain_intensity), 2), 
            median = round(median(vas_pain_intensity),2), 
            sd = round(sd(vas_pain_intensity), 2), 
            min = round(min(vas_pain_intensity), 2), 
            max = round(max(vas_pain_intensity), 2), 
            lowCI = round(mean-1.96*sd/sqrt(n()),2), 
            hiCI = round(mean+1.96*sd/sqrt(n()),2));  desc_vas

#life quality
#NV  ok
if(figures) {ggplot(d, aes(x=lq_overall, fill=group))+  geom_histogram(binwidth=12.5, position="dodge")}
by(d$lq_overall, paste(d$group, d$time), function(X)  round(stat.desc(X, norm = TRUE, basic = TRUE), digits=2))
#descriptives
desc_lq <- d %>%
  group_by(group, time)%>%
  summarise(n=n(), 
            mean = round(mean(lq_overall), 2), 
            median = round(median(lq_overall),2), 
            sd = round(sd(lq_overall), 2), 
            min = round(min(lq_overall), 2), 
            max = round(max(lq_overall), 2), 
            lowCI = round(mean-1.96*sd/sqrt(n()),2), 
            hiCI = round(mean+1.96*sd/sqrt(n()),2)); desc_lq
desc_lq_wholegroup <- d %>%
  group_by(time)%>%
  summarise(n=n(), 
            mean = round(mean(lq_overall), 2), 
            median = round(median(lq_overall),2), 
            sd = round(sd(lq_overall), 2), 
            min = round(min(lq_overall), 2), 
            max = round(max(lq_overall), 2), 
            lowCI = round(mean-1.96*sd/sqrt(n()),2), 
            hiCI = round(mean+1.96*sd/sqrt(n()),2))
desc_lq_wholestudy <- d %>%
  summarise(
            mean = round(mean(lq_overall), 2), 
            median = round(median(lq_overall),2), 
            sd = round(sd(lq_overall), 2), 
            min = round(min(lq_overall), 2), 
            max = round(max(lq_overall), 2), 
            lowCI = round(mean-1.96*sd/sqrt(n()),2), 
            hiCI = round(mean+1.96*sd/sqrt(n()),2))

#depression
#NV ok
if(figures) {ggplot(d, aes(x=hads_depression, fill=group))+  geom_histogram(binwidth=3, position="dodge")}
by(d$hads_depression, paste(d$group, d$time), function(X)  round(stat.desc(X, norm = TRUE, basic = TRUE), digits=2))
#descriptives
desc_dep <- d %>%
  group_by(group, time)%>%
  summarise(n=n(), 
            mean = round(mean(hads_depression), 2), 
            median = round(median(hads_depression),2), 
            sd = round(sd(hads_depression), 2), 
            min = round(min(hads_depression), 2), 
            max = round(max(hads_depression), 2), 
            lowCI = round(mean-1.96*sd/sqrt(n()),2), 
            hiCI = round(mean+1.96*sd/sqrt(n()),2));desc_dep

#anxiety
#NV ok
if(figures) {ggplot(d, aes(x=hads_anxiety, fill=group))+  geom_histogram(binwidth=3, position="dodge")}
by(d$hads_anxiety, paste(d$group, d$time), function(X)  round(stat.desc(X, norm = TRUE, basic = TRUE), digits=2))
#descriptives
desc_anx <- d %>%
  group_by(group, time)%>%
  summarise(n=n(), 
            mean = round(mean(hads_anxiety), 2), 
            median = round(median(hads_anxiety),2), 
            sd = round(sd(hads_anxiety), 2), 
            min = round(min(hads_anxiety), 2), 
            max = round(max(hads_anxiety), 2), 
            lowCI = round(mean-1.96*sd/sqrt(n()),2), 
            hiCI = round(mean+1.96*sd/sqrt(n()),2)); desc_anx

#hypochondria
#NV ok
if(figures) {ggplot(d, aes(x=wi_hypochondria, fill=group))+  geom_histogram(binwidth=2, position="dodge")}
by(d$wi_hypochondria, paste(d$group, d$time), function(X)  round(stat.desc(X, norm = TRUE, basic = TRUE), digits=2))
#descriptives
desc_hyp <- d %>%
  group_by(group, time)%>%
  summarise(n=n(), 
            mean = round(mean(wi_hypochondria), 2), 
            median = round(median(wi_hypochondria),2), 
            sd = round(sd(wi_hypochondria), 2), 
            min = round(min(wi_hypochondria), 2), 
            max = round(max(wi_hypochondria), 2), 
            lowCI = round(mean-1.96*sd/sqrt(n()),2), 
            hiCI = round(mean+1.96*sd/sqrt(n()),2)); desc_hyp

if(excel) {
  write.csv2(x=t(desc_vas), file = "output/vas.csv")
  write.csv2(x=t(desc_lq), file = "output/life_quality.csv")
  write.csv2(x=t(desc_lq), file = "output/depression.csv")
  write.csv2(x=t(desc_vas), file = "output/anxiety.csv")
  write.csv2(x=t(desc_lq), file = "output/hypochondria.csv")
  }


# look at distribution of differences between time points within each treatment group (to check normality, skewness...) ####

#control group change T0-T2: all except VAS pain intensity
#changes: all life quality variables reduced, pain -1 of 10, number of symptoms and intensity increased, anxiety, depression and hypochondria stable,
round(stat.desc(with(subset(diffs, group=="control"&diff=="T2-T0"), cbind(lq_overall_diff, vas_pain_intensity_diff, hads_anxiety_diff, hads_depression_diff, wi_hypochondria_diff)), basic=F, norm=T), digits=2)

#control group change T2-T3: all variables normally distributed 
#changes: life quality increases, symptoms and intensity decreased, nothing else changes (vas, anxiety, depression, hypochondria)
round(stat.desc(with(subset(diffs, group=="control"&diff=="T3-T2"), cbind(lq_phys_diff,   lq_psych_diff,  lq_social_diff, lq_envir_diff,  lq_overall_diff, vas_pain_intensity_diff,
                                                                          soms_n_diff, soms_intensity_diff, hads_anxiety_diff, hads_depression_diff, wi_hypochondria_diff)), basic=F, norm=T), digits=2)

#treatment group change T0-T2: lq_phsych_diff, lq_social_diff and vas not normally distributed (check with graph)
#changes: life quality increases, pain -1 of 10, number of symptoms and intensity increased, anxiety, depression and hypochondria probably stable (slight decrease),
round(stat.desc(with(subset(diffs, group=="treatment"&diff=="T2-T0"), cbind(lq_phys_diff,   lq_psych_diff,  lq_social_diff, lq_envir_diff,  lq_overall_diff, vas_pain_intensity_diff,
                                                                            soms_n_diff, soms_intensity_diff, hads_anxiety_diff, hads_depression_diff, wi_hypochondria_diff)), basic=F, norm=T), digits=2)

#treatment group change T2-T3: depression not normally distributed
#changes: life quality increased physical, psychological and environment, but not socially (because treatment ceased) and not overall, symptoms and intensity decreased, nothing else changes (vas, anxiety, depression, hypochondria)
round(stat.desc(with(subset(diffs, group=="treatment"&diff=="T3-T2"), cbind(lq_phys_diff,   lq_psych_diff,  lq_social_diff, lq_envir_diff,  lq_overall_diff, vas_pain_intensity_diff,
                                                                            soms_n_diff, soms_intensity_diff, hads_anxiety_diff, hads_depression_diff, wi_hypochondria_diff)), basic=F, norm=T), digits=2)


#distributions of variables at T1 in each group (for baseline comparisons) ####
#control group: all variables normally distributed
round(stat.desc(with(subset(d0, group=="control"), cbind(age,lq_phys, lq_psych, lq_social, lq_envir, lq_overall, vas_pain_intensity, soms_n, soms_intensity,
                                                           hads_anxiety, hads_depression, wi_hypochondria)), basic=F, norm=T), digits=2)

#treatment group: all variables except except soms_intensity are normally distributed
round(stat.desc(with(subset(d0, group=="treatment"), cbind(age, lq_phys, lq_psych, lq_social, lq_envir, lq_overall, vas_pain_intensity, soms_n, soms_intensity,
                                                           hads_anxiety, hads_depression, wi_hypochondria)), basic=F, norm=T), digits=2)

# # all continuous variables in one big table with important statistics for the dissertation ####
# #(ignore the warning created by function ci(), all is well)
# desc_bytime <- d %>%
#   group_by(group,time)%>%
#   mutate(n=n())%>%
#   summarise_at(.vars = vars(lq_phys: lq_overall, vas_pain_intensity:wi_hypochondria), #variables to summarise
#                funs(mean(.,na.rm = T), sd(., na.rm=T), median(., na.rm=T), #mean, sd, median
#                     lowCI =ci(., na.rm=T)[2], hiCI =ci(., na.rm=T)[3])) #95% CI (gmodels library)
# 
# #reorder so that all parameters for one questionnaire follow each other
# ncols <- ncol(desc_bytime);
# nstats <- 5 #number of different statistic parameters (mean, sd, ...) calculated
# nvars <- (ncols-2)/nstats
# sort <- function(x){seq(x, ncols, nvars)} #Function that creates numbers in steps of 11 (because 11 variables)
# desc_bytime <- desc_bytime[,c(1,2,sapply(c(3:13), sort))] #first variable is in 3rd column
# desc_bytime[,c(3:ncol(desc_bytime))] <- round(desc_bytime[,c(3:ncol(desc_bytime))],digits=2) #round to 2 digits
# 
# if(excel) {write.csv2(t(desc_bytime), file="output/descriptives_by_time.csv")} #print this to excel table

# 
#levene tests

levenetest_T0 <- as.data.frame(t(sapply(d0[,c("age", "lq_overall", "lq_phys", "lq_psych", "lq_social", "lq_envir", "vas_pain_intensity", "hads_depression", "hads_anxiety", "wi_hypochondria", "pt_motivation")], function(x)round(unlist(leveneTest(x~d0$group, var.equal=T))[c("Df1", "Df2", "F value1", "Pr(>F)1")],digits=3))))

dlevene02 <- diffs %>% filter(diff=="T2-T0")
levenetest_T2T0 <- as.data.frame(t(sapply(dlevene02[,c("lq_phys_diff", "lq_psych_diff", "lq_social_diff", "lq_envir_diff", "lq_overall_diff", "vas_pain_intensity_diff", "hads_anxiety_diff", "hads_depression_diff", "wi_hypochondria_diff")], function(x)round(unlist(leveneTest(x~dlevene02$group, var.equal=T))[c("Df1", "Df2", "F value1", "Pr(>F)1")],digits=3))))

dlevene03 <- diffs %>% filter(diff=="T3-T2")
levenetest_T3T2 <- as.data.frame(t(sapply(dlevene03[,c("lq_phys_diff", "lq_psych_diff", "lq_social_diff", "lq_envir_diff", "lq_overall_diff", "vas_pain_intensity_diff", "hads_anxiety_diff", "hads_depression_diff", "wi_hypochondria_diff")], function(x)round(unlist(leveneTest(x~dlevene03$group, var.equal=T))[c("Df1", "Df2", "F value1", "Pr(>F)1")],digits=3))))
