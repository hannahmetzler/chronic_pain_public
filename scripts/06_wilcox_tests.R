#wilcox test.psych
# library(dplyr)

# source("scripts/01_prepare_data.R")

# transform variables to numeric in all required data frames
dwil <- d %>%
  mutate(cpsq = as.numeric(cpsq))

dwil03 <- d03 %>%
  mutate(mpss_stage = as.numeric(mpss_stage)) %>% 
  mutate(expectation.success_medt = as.numeric(expectation.success_medt), 
         expectation.success_psyt = as.numeric(expectation.success_psyt), 
         time = as.factor(time))

dwil0 <- d0 %>%
  mutate(expectation.success_medt = as.numeric(expectation.success_medt), 
         expectation.success_psyt = as.numeric(expectation.success_psyt),
         time = as.factor(time))

#separate subset for control and treatment group d03
dc <- subset(dwil03, group=="control")%>% droplevels()
dt <- subset(dwil03, group=="treatment")%>% droplevels()


# to calculate effect sizes for 2 time point paired Wilcox signed rank test (treatment effect) for ordinal variables: proportion of patients that decreased, increased, stayed the same
#control group
dc_diff = dc %>%  
  group_by(subid) %>% 
  mutate(mpss_stage = case_when(
    mpss_stage[2]-mpss_stage[1] < 0 ~ "lower stage", 
    mpss_stage[2]-mpss_stage[1] > 0 ~ "higher stage", 
    TRUE~"no change"),
    expectation.success_medt = case_when(
      expectation.success_medt[2]-expectation.success_medt[1] < 0 ~ "higher expectations", 
      expectation.success_medt[2]-expectation.success_medt[1] > 0 ~ "lower expectations", 
      TRUE~"no change"),
    expectation.success_psyt = case_when(
      expectation.success_psyt[2]-expectation.success_psyt[1] < 0 ~ "higher expectations", 
      expectation.success_psyt[2]-expectation.success_psyt[1] > 0 ~ "lower expectations", 
      TRUE~"no change")) %>% 
  slice(1) 
#treatment group
dt_diff = dt %>%  
  group_by(subid) %>% 
  mutate(mpss_stage = case_when(
    mpss_stage[2]-mpss_stage[1] < 0 ~ "lower stage", 
    mpss_stage[2]-mpss_stage[1] > 0 ~ "higher stage", 
    TRUE~"no change"), 
    expectation.success_medt = case_when(
      expectation.success_medt[2]-expectation.success_medt[1] < 0 ~ "higher expectations", 
      expectation.success_medt[2]-expectation.success_medt[1] > 0 ~ "lower expectations", 
      TRUE~"no change"),
    expectation.success_psyt = case_when(
      expectation.success_psyt[2]-expectation.success_psyt[1] < 0 ~ "higher expectations", 
      expectation.success_psyt[2]-expectation.success_psyt[1] > 0 ~ "lower expectations", 
      TRUE~"no change")) %>% 
  slice(1) 

# wide data frame format for paired wilcox tests
dc_wide = dc %>% 
  pivot_wider(names_from = time, names_sep = "_", 
              values_from = c(mpss_stage, lost_days, specialist_visits, expectation.success_medt, expectation.success_psyt), 
              id_cols = subid)
dt_wide = dt %>% 
  pivot_wider(names_from = time, names_sep = "_", 
              values_from = c(mpss_stage, lost_days, specialist_visits, expectation.success_medt, expectation.success_psyt), 
              id_cols = subid)

#wilcox tests 

#pain duration at T0
w_pd0 <- round(as.numeric(t(unlist(wilcox.test(pain_duration_years ~ group, data=dwil0, conf.int =TRUE)))[c(1:2,9)]), digits=3)

#cpsq
wcpsq <- round(as.numeric(t(unlist(wilcox.test(cpsq ~ group, data=dwil, conf.int =TRUE)))[c(1:2,9)]), digits=3)




#mpss
wmpss0 <- round(as.numeric(t(unlist(wilcox.test(mpss_stage ~ group, data=dwil03, conf.int =TRUE)))[c(1:2,9)]), digits=3)[c(1:3)]

wmpss_c03 <- round(as.numeric(t(unlist(wilcox.test(Pair(mpss_stage_T0, mpss_stage_T3) ~ 1, data=dc_wide, conf.int =T)))[c(1:2,9)]), digits=3)[c(1:3)]

wmpss_t03 <- round(as.numeric(t(unlist(wilcox.test(Pair(mpss_stage_T0, mpss_stage_T3) ~ 1, data=dt_wide, conf.int =T)))[c(1:2,9)]), digits=3)[c(1:3)]




#success expectation
w0m <- round(as.numeric(t(unlist(wilcox.test(expectation.success_medt ~ group, data=dwil0, conf.int =TRUE)))[c(1:2,9)]), digits=3)
w0p <- round(as.numeric(t(unlist(wilcox.test(expectation.success_psyt ~ group, data=dwil0, conf.int =TRUE)))[c(1:2,9)]), digits=3)

w_c03m <- round(as.numeric(t(unlist(wilcox.test(Pair(expectation.success_medt_T0,  expectation.success_medt_T3) ~ 1, data=dc_wide, conf.int =TRUE)))[c(1:2,9)]), digits=3)
w_t03m <- round(as.numeric(t(unlist(wilcox.test(Pair(expectation.success_medt_T0,  expectation.success_medt_T3) ~ 1, data=dt_wide, conf.int =TRUE)))[c(1:2,9)]), digits=3)
w_t03p <- round(as.numeric(t(unlist(wilcox.test(Pair(expectation.success_psyt_T0,  expectation.success_psyt_T3) ~ 1, data=dt_wide, conf.int =TRUE)))[c(1:2,9)]), digits=3)[c(1:3)]

#sickness related behaviors
w_s0 <- round(as.numeric(t(unlist(wilcox.test(specialist_visits ~ group, data=dwil0, conf.int =TRUE)))[c(1:2,9)]), digits=3)
w_gs <- round(as.numeric(t(unlist(wilcox.test(specialist_visits_diff03 ~ group, data=diffs03, conf.int=T)))[c(1:2,9)]), digits=3)

w_l0 <- round(as.numeric(t(unlist(wilcox.test(lost_days ~ group, data=dwil0, conf.int =TRUE)))[c(1:2,9)]), digits=3)
w_gl <- round(as.numeric(t(unlist(wilcox.test(lost_days_diff03 ~ group, data=diffs03, conf.int =TRUE)))[c(1:2,9)]), digits=3)

