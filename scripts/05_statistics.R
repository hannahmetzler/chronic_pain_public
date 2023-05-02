#statistics

##load formatted data
source("scripts/01_prepare_data.R")

#libraries: 
library(dplyr) #for basic data handling
library(ez) #for ANOVAS
library(car)#for LeveneTest
library(psych)

excel = TRUE

# #correlations ####

# Depression (HADS D-Score) and LQ, VAS and LQ

#across all time points and groups
corr.test(d[,c("lq_overall", "hads_depression","vas_pain_intensity")],  method="pearson")
corr.test(d[,c("lq_overall", "hads_depression","vas_pain_intensity")],  method="spearman") #ist LQ wirklich kontinuierlich?

ggplot(d, aes(x=lq_overall, fill=group))+  geom_histogram(binwidth=5, position="dodge")+scale_x_continuous(breaks=seq(0,100,12.5))

#per time point
by(d[,c("lq_overall", "hads_depression","vas_pain_intensity")], d$time, corr.test, method="spearman")

#per time point and group
by(d[,c("lq_overall", "hads_depression","vas_pain_intensity")], paste(d$time, d$group), corr.test, method="spearman")

#correlation table
r_lq_dep <- d %>%
  group_by(time)%>%
  summarise(r= round(unlist(corr.test(cbind(lq_overall, hads_depression)))$r2,3),
            p= round(unlist(corr.test(cbind(lq_overall, hads_depression)))$p2,3))
r_lq_dep
r_lq_vas <- d %>%
  group_by(time)%>%
  summarise(r= round(unlist(corr.test(cbind(lq_overall, vas_pain_intensity)))$r2, 3),
            p= round(unlist(corr.test(cbind(lq_overall, vas_pain_intensity)))$p2,3))
r_lq_vas

#write tables to excel
if(excel){
var_names <- rep(c("LQ-Depression", "LQ-VAS"), each=dim(r_lq_dep)[1])
write.csv2(x=cbind(var_names, data.frame(rbind(r_lq_dep, r_lq_vas))) , file = "output/correlations_combined_groups.csv")
}


# Testing assumptions for t-tests and ANOVAS #### 

#normal distribution of differences between time points? Not in both groups
with(diffs, by(lq_overall_diff, group, shapiro.test))
# #log transformation - does not help
# diffs_log <- d %>%
#   mutate(lq_overall_log = log(lq_overall+0.01))%>%
#   group_by(subid, group)%>% #calculate difference per subject (also keep the group variable)
#   summarise(T2T0 = lq_overall_log[time=="T2"]-lq_overall_log[time=="T0"],
#             T3T2 = lq_overall_log[time=="T3"]-lq_overall_log[time=="T2"])%>%
#   gather(key = "diff", value="lq_overall_log", -subid, -group)
# #does not help at all - it was better before
# with(diffs_log, by(lq_overall_log, group, shapiro.test))

#test of homogeneity of variances of baseline and differences between time points
leveneTest(lq_overall ~ group, data=d0) # baseline variances homogeneous
levenetest_T0 <- as.data.frame(t(sapply(d0[,c("age", "lq_overall", "lq_phys","lq_psych","lq_social","lq_envir","vas_pain_intensity", 
                                                "hads_depression", "hads_anxiety", "wi_hypochondria", "pt_motivation")], 
                                          function(x)round(unlist(leveneTest(x~d0$group, var.equal=T))[c("Df1", "Df2", "F value1", "Pr(>F)1")],digits=3))))
levenetest_T2T0 <- as.data.frame(t(sapply(diffs02[,c("age", "lq_overall", "lq_phys","lq_psych","lq_social","lq_envir","vas_pain_intensity", 
                                              "hads_depression", "hads_anxiety", "wi_hypochondria", "pt_motivation")], 
                                        function(x)round(unlist(leveneTest(x~d02$group, var.equal=T))[c("Df1", "Df2", "F value1", "Pr(>F)1")],digits=3))))
levenetest_T3T2 <- as.data.frame(t(sapply(d03[,c("age", "lq_overall", "lq_phys","lq_psych","lq_social","lq_envir","vas_pain_intensity", 
                                              "hads_depression", "hads_anxiety", "wi_hypochondria", "pt_motivation")], 
                                        function(x)round(unlist(leveneTest(x~d03$group, var.equal=T))[c("Df1", "Df2", "F value1", "Pr(>F)1")],digits=3))))

# baseline difference t-tests ####

t.test(lq_overall~group, data = D0, var.equal=T); #t-test

# multiple t-tests at the same time
ttest_groups_T0 <- as.data.frame(t(sapply(d0[,c("age", "lq_overall", "lq_phys","lq_psych","lq_social","lq_envir","vas_pain_intensity",
                                                "hads_depression", "hads_anxiety", "wi_hypochondria", "pt_motivation")],
                       function(x)round(unlist(t.test(x~d0$group, var.equal=T)[c("estimate","parameter", "p.value","statistic","conf.int")]),digits=3))))
n <-as.data.frame(xtabs(~group, d0))$Freq #calculate n per posture group for cohen's d
ttest_groups_T0$cohens.d <- (ttest_groups_T0[,"statistic.t"]*sqrt(1/n[1]+1/n[2]))
ttest_groups_T0[,c(1:3, 5:8)] <- round(ttest_groups_T0[,c(1:3, 5:8)], 2) #round all values except p to 2 digits
ttest_groups_T0

 
# ANOVAs ####

# #define function to calculate partial eta squared: F*dfn/F*dfn+dfd see Lakens 2013 page 6
peta <- function(x) {a$ANOVA[x,"F"]*a$ANOVA[x, "DFn"]/(a$ANOVA[x,"F"]*a$ANOVA[x, "DFn"]+a$ANOVA[x, "DFd"])}

a <- ezANOVA(data=d, dv=lq_overall, wid = subid, within=time, between=group,  type=3); a$ANOVA[,"peta"] <- c(peta(1), peta(2), peta(3)); a
#anova on differences
a<- ezANOVA(data=diffs, dv=lq_overall_diff, wid = subid, within=diff, between=group,  type=3); a$ANOVA[,"peta"] <- c(peta(1), peta(2), peta(3)); a


#t-tests on changes T2-T0 for all continuous variables with 3 time points: only life quality changes
#compare with p-value of 0.025 (Bonferroni corrected), because we test two differences
d_temp <- diffs %>%   filter(diff == "T2-T0")
t.test(lq_overall_diff~group, var.equal=T, data = subset(diffs, diff == "T2-T0"))

as.data.frame(t(sapply(d_temp[,c("lq_overall_diff", "lq_phys_diff","lq_psych_diff","lq_social_diff","lq_envir_diff","vas_pain_intensity_diff",
                                 "hads_depression_diff", "hads_anxiety_diff", "wi_hypochondria_diff")],
                                        function(x)round(unlist(t.test(x~d_temp$group, var.equal=T)[c("estimate","parameter", "p.value","statistic","conf.int")]),digits=2))))
#same for T3-T2: again only life quality changes, except physical life quality, and lq_overall
d_temp <- diffs %>%   filter(diff == "T3-T2")
as.data.frame(t(sapply(d_temp[,c("lq_overall_diff", "lq_phys_diff","lq_psych_diff","lq_social_diff","lq_envir_diff","vas_pain_intensity_diff",
                                 "hads_depression_diff", "hads_anxiety_diff", "wi_hypochondria_diff")],
                       function(x)round(unlist(t.test(x~d_temp$group, var.equal=T)[c("estimate","parameter", "p.value","statistic","conf.int")]),digits=2))))


#posthoc t-test for interaction
pairwise.t.test(diffs3T_02$lq_overall_diff, g=diffs3T_02$group, p.adjust.method = "holm", paired = F)

#posthoc t-test for time
pairwise.t.test(d$lq_overall, g=d$time, p.adjust.method = "holm", paired = T)

# #t-tests between groups
as.data.frame(t(sapply(diffs[,c("lq_overall_diff", "cortdiff3_2log")],
                                    function(x)round(unlist(t.test(x~diffs$group, var.equal=T)[c("estimate","parameter", "p.value","statistic","conf.int")]),digits=2))))

#join the 3 t-test dataframes
tdiffvs0 <- as.data.frame(rbind(tdiffvs0_c, tdiffvs0_t, tdiffvs0_p))
#calculate cohen's dz=t/sqrt(n))
tdiffvs0$dz <- round(with(tdiffvs0, statistic.t/sqrt(parameter.df+1)), digits=2); tdiffvs0

d$timeXgroup <- as.factor(with(d, paste(as.character(time), as.character(group), sep = "_")))
posthoc <- with(d, pairwise.t.test(lq_overall, g=timeXgroup, p.adjust.method = "holm", paired = T))


#robust alternative to t-test to non-NV distributed variables, or non-continuous variables (ranked)

#pain duration in years, specialist visits, lost_days,

#categorical - chi-square: mpss_stage, cpsq, expectations/success,
