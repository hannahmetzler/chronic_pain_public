# figures 

#load packages and previous scripts
library(ggplot2)
library(scales)
library(RColorBrewer)
library(ggpubr)
library(cowplot) #for multiple figures
library(gridExtra) #for multiple figures
library(dplyr)

#load data
source("scripts/01_prepare_data.R") # all paths are relative the the analysis_runner.Rmd RMarkdown file!

# format variables for Graphs: start with capital letters ####
dp <- d %>% #dp = data for plots
  mutate(group = factor(group, labels = c("Control", "Treatment")),
        gender = factor(gender, labels = c("Men", "Women", "Not assessed")),
        time = dplyr::recode(time, T2 = "T1", T3="T2"), #renamed for plots in thesis, where Julia does not include reports about T1
        expectation.success_medt = factor(expectation.success_medt, levels=c(1:5, 999), labels = c("none", "2", "3", "4", "high", "Not assessed")),
         expectation.success_psyt = factor(expectation.success_psyt, levels=c(1:5, 999),labels = c("none", "2", "3", "4", "high", "Not assessed")),
         pain_subj_control = factor(pain_subj_control, levels=c(1:5, 999),labels = c("not at all", "2", "3", "4", "very well","Not assessed")),
         cpsq = factor(cpsq, order = TRUE, levels = c(1:4)),#subjektive Beeinträchtigung (4 Level)
         mpss_stage = factor(mpss_stage, order = TRUE, levels = c(1:3,999), labels=c(1,2,3,"Not assessed")))%>% #Schmerz-Chronifizierung (3 Stadien)
  dplyr::rename(Group = group, 
                Gender = gender, 
                Education= education, 
                Occupation = occupation, 
                Marital_status = marital_status, 
                Age = age, 
                Time = time, 
                Income = income, 
                Debt = debt)
#data subsets

#data for T0 only
dp0 <- dp %>%  filter(Time=="T0") %>% droplevels()

#T0 and T3
dp03 <- d03 %>% #dp = data for plots
  mutate(group = factor(group, labels = c("Control", "Treatment")),
         gender = factor(gender, labels = c("Men", "Women", "Not assessed")),
         #for master thesis, Julia does not include T1, therefore rename later time points
         time = dplyr::recode(time, T2 = "T1", T3="T2")) %>%
  dplyr::rename(Group = group, 
                Gender = gender, 
                Occupation = occupation, 
                Time = time) 
#T0 and T2
dp02 <- d02 %>% #dp = data for plots
  mutate(group = factor(group, labels = c("Control", "Treatment")),
         gender = factor(gender, labels = c("Men", "Women", "Not assessed")),
         time = dplyr::recode(time, T2 = "T1", T3="T2")) %>%
  dplyr::rename(Group = group, 
                Gender = gender, 
                Time = time)

# figure settings: ####
#colours
display.brewer.all()#see palettes, choose one with its name, and select certain colours with numbers
cols6 <- rev(brewer.pal(9, "YlGnBu")[c(2,3,4,6,7,9)]) #for plots with many categories
groupcol <- cols6[c(1,4)] #for plots with treatment vs. control group: contrasting colours from the set above
#cols6 <- rev(brewer.pal(12,"Set3")[c(1,5,3,6,12,10,11)])#colours from set3: 
increasing <- brewer.pal(8,"YlGnBu")[c(4:8)]#colours from set YlGnbu = increasing blue tones
s = 15 # text size

#blank white plot for when I need an empty space in a collection of multiple plots
blank <- plot(0,type='n',axes=FALSE,ann=FALSE)

# functions to create plots with variables with 1 time point only or continous variables####

#categorial plot function with 1 time point
cat_plot_1t <- function(variable, legendtitle){
  if(missing(legendtitle)){
    legendtitle = deparse(substitute(variable))
    legendtitle = substr(legendtitle, 5, nchar(legendtitle))
  }
  ggplot(dp0, aes(x=Group, fill=variable)) + #dataset and variables to plot
    geom_bar(stat="count", position="fill")  + #which statistic (count cases, and show as proportion)
    labs(y="Proportion", fill=legendtitle) + #axes and title labels
    scale_fill_manual(values=cols6, na.value="grey", guide=guide_legend(keywidth = 2, keyheight = 2))+
    theme_bw() +   #Remove dark background
    theme(text=element_text(size=s),legend.text=element_text(size=s), #set size of text in general an in legend
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
          legend.position="right", plot.margin = unit(rep(0.5, 4), "cm"))
}

#continous plots with 3 time points 
cont_plot_23t <- function(df, variable, legendtitle, ylim, binwidth, dotsize){
  if(missing(legendtitle)){
    legendtitle = deparse(substitute(variable))
    legendtitle = substr(legendtitle, 5, nchar(legendtitle))
  }
  ggplot(df, aes(x=Time, y=variable, fill=Group))+
    stat_summary(fun = mean, geom="bar", size=1.3, position=position_dodge(0.95),alpha = 0.5, aes(colour=Group))+#bars
    geom_dotplot(binwidth = binwidth, dotsize=dotsize, binaxis = "y", stroke=1, stackdir = "center", position="dodge", show.legend=F)+
    stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.3, size = 1.3, position=position_dodge(0.95),aes(colour=Group), show.legend=F)+
    scale_y_continuous(name=legendtitle, limits=c(0,ylim))+ #axes labels 
    scale_fill_manual(values=groupcol)+scale_color_manual(values=groupcol)+
    theme_bw() +   #Remove dark background
    theme(text=element_text(size=s-2),legend.text=element_text(size=s-2), axis.title.x=element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
          legend.position="right", plot.margin=unit(rep(0.7,4), "cm"))
}

# categoric socio economic variables at T0 ####
plot_gender <- cat_plot_1t(dp0$Gender)
plot_edu <- cat_plot_1t(dp0$Education)
plot_job <- cat_plot_1t(dp0$Occupation)
plot_marital <- cat_plot_1t(dp0$Marital_status, "Marital status")
plot_income <- cat_plot_1t(dp0$Income)
plot_debt <- cat_plot_1t(dp0$Debt)

plot_socio_eco <- plot_grid(plot_gender, plot_edu, plot_job, plot_marital, plot_income, plot_debt, align="hv", ncol=1, 
                           axis = "lr")
pdf('./figures/socio_eco_vars.pdf', height=21, width=6); plot_socio_eco; dev.off()

# #add all socio eco vars together into one plot
# plot_socio_eco <- ggdraw() +
#   draw_plot(plot_grid(plot_gender, plot_edu, plot_job, plot_marital, align="v", ncol=1), 
#             x=0.05, y=0, width=1, height=1) +
#   draw_plot_label(c("A", "B", "C", "D"), c(rep(0,4)), c(1,.75, .5, .25), size = 18)
# pdf('./figures/socio_eco_vars.pdf', height=7, width=7); plot_socio_eco; dev.off()

# age at T0 ####
s=15
plot_age <- ggplot(dp0, aes(x=Group, y=Age, fill=Group)) +  
  #plot bars
  stat_summary(fun = mean, geom="bar", size=1.3, position=position_dodge(0.95),alpha = 0.5, aes(colour=Group))+
  #plot individual data points
  geom_dotplot(binwidth = 2, dotsize=0.7, stroke=1.5, binaxis = "y", stackdir = "center")+
  #geom_point(shape=16, position=position_jitterdodge(jitter.width=0.4, dodge.width =0.95), size = 2, show.legend=F)+   
  #plot error bars
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.3, size = 1.3, position=position_dodge(0.95),aes(colour=Group), show.legend=F)+
  scale_fill_manual(values=groupcol)+scale_color_manual(values=groupcol)+
  coord_cartesian(ylim=c(28, 71)) + #axes#y-axis limits
  theme_bw() +   #Remove dark background
  theme(text=element_text(size=s-3),legend.text=element_text(size=s-3),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="none")

pdf('./figures/age.pdf', height = 2.33, width = 2.33); plot_age; dev.off()

# diagnostics pain at T0 (Schmerzanamnese) ####
plot_pain_fam <- cat_plot_1t(dp0$pain_family_member, "Pain in a family member")
plot_sick <- cat_plot_1t(dp0$sick_leave,   "Sick leave at T0")
plot_retired <- cat_plot_1t(dp0$retired,    "Retired")

plot_pain_control <- ggplot(dp0, aes(x=Group, fill=pain_subj_control)) + #dataset and variables to plot
  geom_bar(stat="count", position="fill", na.rm= FALSE)  + #which statistic (count cases, and show as proportion)
  labs(y="Proportion", fill="Can you influence\n your pain?") + #axes and title labels
  scale_fill_manual(values=c(cols6[1:5], "grey"), na.value="grey", guide=guide_legend(keywidth = 2, keyheight = 2),
                    limits = c("not at all","2", "3", "4", "very well", "NA"))+
  theme_bw() +   #Remove dark background
  theme(text=element_text(size=s),legend.text=element_text(size=s), #set size of text in general an in legend
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="right", plot.margin = unit(rep(0.5, 4), "cm"))

plot_pain <- plot_grid(plot_pain_fam, plot_sick, plot_retired, plot_pain_control, align="hv", ncol=1, label_size=18)
pdf('./figures/pain_diagnostics_t0.pdf', height=14, width=6.5); plot_pain; dev.off()

plot_painyears <- ggplot(dp0, aes(x=Group, y=pain_duration_years, fill=Group))+
  geom_boxplot(aes(colour=Group), size=1, alpha=0.5)+
  geom_dotplot(binwidth = 1, dotsize=1.3, binaxis = "y", stroke=2, stackdir = "center", position="dodge", show.legend=F)+
  scale_y_continuous(name="Pain duration in years") +#, limits=c(0,125))+ #axes labels
  scale_fill_manual(values=groupcol)+scale_color_manual(values=groupcol)+
  theme_bw() +   #Remove dark background
  theme(text=element_text(size=16),legend.text=element_text(size=16), axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="right", plot.margin=unit(rep(0.7,4), "cm"))
pdf('./figures/pain_duration_years.pdf', height=4.5, width=6); plot_painyears; dev.off()

# #put all of them together
# plot_pain <- ggdraw()+
#   draw_plot(plot_pain_cat, x=0, y=0.4, width=1, height=0.6)+
#   draw_plot(plot_painyears, x=0, y=0, width=0.53, height=0.4)+
#   draw_plot_label(c("E"), x=0, y=0.4, size = 18) 
# pdf('./figures/pain_diagnostics_t0.pdf', height=10, width=12); plot_pain; dev.off()

# life quality T0-T2-T3 for entire group####

#life quality total (overall) in the entire group
plot_lq_overall <- ggplot(dp, aes(x=Time, y=lq_overall))+ 
  stat_summary(fun = mean, geom="bar", size=1.3, position=position_dodge(0.95),alpha = 0.5, colour="#1D91C0", fill="#1D91C0")+#bars
  geom_dotplot(binwidth = 5, dotsize=0.5, binaxis = "y", stroke=1, stackdir = "center", position="dodge", show.legend=F, fill="#1D91C0")+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.3, size = 1.3, position=position_dodge(0.95), show.legend=F,colour="#1D91C0")+
  scale_y_continuous(name="Overall life quality", limits=c(0,125))+ #axes labels 
  theme_bw() +   #Remove dark background
  theme(text=element_text(size=s),legend.text=element_text(size=s), axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="right", plot.margin=unit(rep(0.7,4), "cm"))
plot_lq_phys <- ggplot(dp, aes(x=Time, y=lq_phys))+ 
  stat_summary(fun = mean, geom="bar", size=1.3, position=position_dodge(0.95),alpha = 0.5, colour="#1D91C0", fill="#1D91C0")+#bars
  geom_dotplot(binwidth = 5, dotsize=0.5, binaxis = "y", stroke=1, stackdir = "center", position="dodge", show.legend=F, fill="#1D91C0")+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.3, size = 1.3, position=position_dodge(0.95), show.legend=F,colour="#1D91C0")+
  scale_y_continuous(name="Physical life quality", limits=c(0,125))+ #axes labels 
  theme_bw() +   #Remove dark background
  theme(text=element_text(size=s),legend.text=element_text(size=s), axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="right", plot.margin=unit(rep(0.7,4), "cm"))
plot_lq_psy <- ggplot(dp, aes(x=Time, y=lq_psych))+ 
  stat_summary(fun = mean, geom="bar", size=1.3, position=position_dodge(0.95),alpha = 0.5, colour="#1D91C0", fill="#1D91C0")+#bars
  geom_dotplot(binwidth = 5, dotsize=0.5, binaxis = "y", stroke=1, stackdir = "center", position="dodge", show.legend=F, fill="#1D91C0")+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.3, size = 1.3, position=position_dodge(0.95), show.legend=F,colour="#1D91C0")+
  scale_y_continuous(name="Psychological life quality", limits=c(0,125))+ #axes labels 
  theme_bw() +   #Remove dark background
  theme(text=element_text(size=s),legend.text=element_text(size=s), axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="right", plot.margin=unit(rep(0.7,4), "cm"))
plot_lq_envir <- ggplot(dp, aes(x=Time, y=lq_envir))+ 
  stat_summary(fun = mean, geom="bar", size=1.3, position=position_dodge(0.95),alpha = 0.5, colour="#1D91C0", fill="#1D91C0")+#bars
  geom_dotplot(binwidth = 5, dotsize=0.5, binaxis = "y", stroke=1, stackdir = "center", position="dodge", show.legend=F, fill="#1D91C0")+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.3, size = 1.3, position=position_dodge(0.95), show.legend=F,colour="#1D91C0")+
  scale_y_continuous(name="Environemental life quality", limits=c(0,125))+ #axes labels 
  theme_bw() +   #Remove dark background
  theme(text=element_text(size=s),legend.text=element_text(size=s), axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="right", plot.margin=unit(rep(0.7,4), "cm"))
plot_lq_soc <- ggplot(dp, aes(x=Time, y=lq_social))+ 
  stat_summary(fun = mean, geom="bar", size=1.3, position=position_dodge(0.95),alpha = 0.5, colour="#1D91C0", fill="#1D91C0")+#bars
  geom_dotplot(binwidth = 5, dotsize=0.5, binaxis = "y", stroke=1, stackdir = "center", position="dodge", show.legend=F, fill="#1D91C0")+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.3, size = 1.3, position=position_dodge(0.95), show.legend=F,colour="#1D91C0")+
  scale_y_continuous(name="Social life quality", limits=c(0,125))+ #axes labels 
  theme_bw() +   #Remove dark background
  theme(text=element_text(size=s),legend.text=element_text(size=s), axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="right", plot.margin=unit(rep(0.7,4), "cm"))
plot_lq_all <- ggdraw()+
  draw_plot(plot_grid(plot_lq_overall, blank, plot_lq_phys, plot_lq_psy, plot_lq_soc, plot_lq_envir, ncol=2, nrow=3), x=0)+
  draw_plot_label(c("A", "", "B", "C", "D", "E"), x=rep(c(0, 0.5), 3), y=rep(c(1, 2/3, 1/3), each=2), size = 18) 

pdf('./figures/life_quality_wholegroup.pdf', width=7, height=9.5); plot_lq_all; dev.off()

# life quality T0-T2-T3 per group ####
#lq overall per group
plot_lq_total <- cont_plot_23t(dp, dp$lq_overall, "Overall life quality", 125, 5, 0.4)
#save the legend
legend <- get_legend(plot_lq_total)
#remove the legend from plot_lq
plot_lq_total <- plot_lq_total + theme(legend.position="none")

#lq physical
plot_lq_phys <- cont_plot_23t(dp, dp$lq_phys, "Physical life quality", 125, 5, 0.4)+ theme(legend.position="none")
# lq psychological
plot_lq_psy <- cont_plot_23t(dp, dp$lq_psych, "Psychological life quality", 125, 5, 0.4)+ theme(legend.position="none")
#lq social
plot_lq_soc <- cont_plot_23t(dp, dp$lq_social, "Social life quality", 125, 5, 0.4)+ theme(legend.position="none")
#lq environmental
plot_lq_env <- cont_plot_23t(dp, dp$lq_envir, "Environmental life quality", 125, 5, 0.4)+ theme(legend.position="none")
plot_lq <- ggdraw()+
  draw_plot(plot_grid(plot_lq_total, legend, plot_lq_phys, plot_lq_psy, plot_lq_soc, plot_lq_env, ncol=2, nrow=3), x=0)+
  draw_plot_label(c("A", "", "B", "C", "D", "E"), x=rep(c(0, 0.5), 3), y=rep(c(1, 2/3, 1/3), each=2), size = 18) 

pdf('./figures/life_quality.pdf', width=7, height=9.5); plot_lq; dev.off()

# psychological disorders/health T0-T2-T3 ####
#anxiety
plot_anx <- ggplot(dp, aes(x=Time, y=hads_anxiety,fill=Group))+
  stat_summary(fun = mean, geom="bar", size=1.3,position=position_dodge(0.95),alpha = 0.5, aes(colour=Group))+#bars
  geom_dotplot(binwidth=1, dotsize=0.3, binaxis = "y", stroke=1.5, stackdir = "center", position="dodge", show.legend=F)+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.3, size = 1.3, position=position_dodge(0.95),aes(colour=Group), show.legend=F)+
  scale_y_continuous(name="Anxiety")+  #axes labels
  scale_fill_manual(values=groupcol)+scale_color_manual(values=groupcol)+
  theme_bw() +   #Remove dark background
  theme(text=element_text(size=s-2),legend.text=element_text(size=s-2),axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="bottom", plot.margin = unit(rep(0.5,4), "cm"))
#get legend from anxiety plot
legend_bottom <- get_legend(plot_anx)
#remove the legend from plot_lq
plot_anx <- plot_anx + theme(legend.position="none")

#depression
plot_dep <- ggplot(dp, aes(x=Time, y=hads_depression,fill=Group))+
  stat_summary(fun = mean, geom="bar", size=1.3,position=position_dodge(0.95),alpha = 0.5, aes(colour=Group))+#bars
  geom_dotplot(binwidth=1, dotsize=0.35, binaxis = "y", stroke=1.5, stackdir = "center", position="dodge", show.legend=F)+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.3, size = 1.3, position=position_dodge(0.95),aes(colour=Group), show.legend=F)+
  scale_y_continuous(name="Depression")+  #axes labels
  scale_fill_manual(values=groupcol)+scale_color_manual(values=groupcol)+
  theme_bw() +   #Remove dark background
  theme(text=element_text(size=s-2),legend.text=element_text(size=s-2),axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="none", plot.margin = unit(rep(0.5,4), "cm"))
#Hypochondria
plot_hypo <- ggplot(dp, aes(x=Time, y=wi_hypochondria,fill=Group))+
  stat_summary(fun = mean, geom="bar", size=1.3,position=position_dodge(0.95),alpha = 0.5, aes(colour=Group))+#bars
  geom_dotplot(binwidth=1, dotsize=0.25, binaxis = "y", stroke=1.5, stackdir = "center", position="dodge", show.legend=F)+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.3, size = 1.3, position=position_dodge(0.95),aes(colour=Group), show.legend=F)+
  scale_y_continuous(name="Hypochondria")+ #axes labels
  scale_fill_manual(values=groupcol)+scale_color_manual(values=groupcol)+
  theme_bw() +   #Remove dark background
  theme(text=element_text(size=s-2),legend.text=element_text(size=s-2), axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="none", plot.margin = unit(rep(0.5,4), "cm"))

plot_psy <- plot_grid(plot_anx, plot_dep, plot_hypo, legend, ncol=2, labels=c("A", "B", "C"), label_size=18)
pdf('./figures/psychological_health.pdf', width=7, height=7); plot_psy; dev.off()
#organized into one row (exclude legend in plot3 line above)
# plot_psy <- ggdraw() +
#   draw_plot(plots3,        x=0,    y=0.1, width=1, height=6/7) +
#   draw_plot(legend_bottom, x=0.02, y=0,   width=0.4, height=1/7)
#pdf('./figures/psychological_health.pdf', width=10, height=4); plot_psy; dev.off()


# pain VAS T0-T2-T3 ####

#VAS pain intensity
plot_vas <- ggplot(dp, aes(x=Time, y=vas_pain_intensity,fill=Group))+
  stat_summary(fun = mean, geom="bar", size=1.3,position=position_dodge(0.95),alpha = 0.5, aes(colour=Group))+#bars
  geom_dotplot(binwidth=0.5, dotsize=0.3, binaxis = "y", stroke=1.5, stackdir = "center", position="dodge", show.legend=F)+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.3, size = 1.3, position=position_dodge(0.95),aes(colour=Group), show.legend=F)+
  scale_y_continuous(name="VAS pain intensity")+ #axes labels
  scale_fill_manual(values=groupcol)+scale_color_manual(values=groupcol)+
  theme_bw() +   #Remove dark background
  theme(text=element_text(size=s),legend.text=element_text(size=s),axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="right")
pdf('./figures/VAS_pain_intensity.pdf', width=7, height=5); plot_vas; dev.off()

#SOMS number of symptoms - leave it out - not included in dissertation
plot_somsn <- ggplot(dp, aes(x=Time, y=soms_n,fill=Group))+
  stat_summary(fun = mean, geom="bar", size=1.3,position=position_dodge(0.95),alpha = 0.5, aes(colour=Group))+#bars
  geom_dotplot(binwidth=2, dotsize=0.3, binaxis = "y", stroke=1.5, stackdir = "center", position="dodge", show.legend=F)+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.3, size = 1.3, position=position_dodge(0.95),aes(colour=Group), show.legend=F)+
  scale_y_continuous(name="SOMS symptom number")+ #axes labels
  scale_fill_manual(values=groupcol)+scale_color_manual(values=groupcol)+
  theme_bw() +   #Remove dark background
  theme(text=element_text(size=s),legend.text=element_text(size=s),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="top")

#SOMS symptom intensity - leave it out - not included in dissertation
plot_somsintensity <- ggplot(dp, aes(x=Time, y=soms_intensity,fill=Group))+
  stat_summary(fun = mean, geom="bar", size=1.3,position=position_dodge(0.95),alpha = 0.5, aes(colour=Group))+#bars
  geom_dotplot(binwidth=10, dotsize=0.2, binaxis = "y", stroke=1.5, stackdir = "center", position="dodge", show.legend=F)+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.3, size = 1.3, position=position_dodge(0.95),aes(colour=Group), show.legend=F)+
  scale_y_continuous(name="SOMS symptom intensity")+ #axes labels
  scale_fill_manual(values=groupcol)+scale_color_manual(values=groupcol)+
  theme_bw() +   #Remove dark background
  theme(text=element_text(size=s),legend.text=element_text(size=s),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="top")  #save the legend

# pain: change in categorical variables across time: CPSQ T0-T2-T3 and MPSS total T0-T3 ####

#cpsq
plot_cpsq <- ggplot(dp, aes(x=cpsq,fill=cpsq))+
  facet_grid(Group~Time)+
  geom_histogram(stat="count", position="dodge")+
  scale_y_continuous(breaks=c(2,4,6,8,10), name="Count")+ xlab("CPQS Level")+
  scale_fill_manual(values=increasing, na.value="grey",guide=guide_legend(byrow=F,ncol=1, keyheight=1.8))+
  theme_bw() +
  theme(text=element_text(size=s),legend.text=element_text(size=s),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="none")
pdf('./figures/cpsq.pdf', width=7, height=5); plot_cpsq; dev.off()

#cpsq 2 levels adjacent combined for only 2 levels total
dp <- dp %>% 
  mutate(cpsq2 = ifelse(cpsq > 3, 2, 1), 
         cpsq2 = factor(cpsq2, order = TRUE, levels = c(1:2), labels = c("1 & 2", "3 & 4")))

plot_cpsq2 <- ggplot(dp, aes(x=(cpsq2),fill=(cpsq2)))+
  facet_grid(Group~Time)+
  geom_histogram(stat="count", position="dodge")+
  scale_y_continuous(breaks=c(2,4,6,8,10, 12), name="Count")+ xlab("CPSQ Combined Levels")+
  scale_fill_manual(values=increasing, breaks=c(1,2), na.value="grey",
                    guide=guide_legend(byrow=F,ncol=1, keyheight=1.8))+
  theme_bw() +
  theme(text=element_text(size=s),legend.text=element_text(size=s),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="none")
pdf('./figures/cpsq_combinedlevels.pdf', width=7, height=5); plot_cpsq2; dev.off()

#mpss
dp03$mpss_stage <- factor(dp03$mpss_stage)
plot_mpss <- ggplot(subset(dp03), aes(x=mpss_stage,fill=mpss_stage))+
  facet_grid(Group~Time)+
  geom_histogram(stat="count", position="dodge")+#bars
  scale_y_continuous(breaks=c(2,4,6,8,10), name="Count")+
  xlab("MPSS stage")+
  scale_fill_manual(values=increasing, na.value="grey",guide=guide_legend(byrow=F,ncol=1, keyheight=1.8))+
  theme_bw() + #Remove dark background
  theme(text=element_text(size=s),legend.text=element_text(size=s),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="none")
pdf('./figures/mpss.pdf', width=4.5, height=4); plot_mpss; dev.off()


# plot correlations T0-T2-T3 ####

#per group: depression and LQc
corr_dlq_g <- ggplot(dp, aes(x=lq_overall, y=hads_depression, colour=Group, shape=Group))+
  facet_grid(Group~Time)+ 
  geom_point(size=2.5, position=position_dodge(1))+ 
  geom_smooth(method='lm', se=T)+
  ylab("Depression")+xlab("Overall LQ")+
  theme_bw() +
  scale_colour_manual(values=groupcol)+
  theme(text=element_text(size=s),legend.text=element_text(size=s),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="none")

#per group: vas ad LQ
corr_vaslq_g <- ggplot(dp, aes(x=lq_overall, y=vas_pain_intensity, colour=Group, shape=Group))+
  facet_grid(Group~Time)+
  geom_point(size=2.5, position=position_dodge(1))+
  geom_smooth(method='lm', se=T)+
  ylab("Pain intensity VAS")+xlab("Overall LQ")+
  scale_colour_manual(values=groupcol)+
  theme_bw() +
  theme(text=element_text(size=s),legend.text=element_text(size=s),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="none")

#whole sample: depression and LQ
dp$all <- factor(rep(1,nrow(dp)), levels=1, labels="All")
corr_dlq_w <- ggplot(dp, aes(x=lq_overall, y=hads_depression))+
  facet_grid(all~Time)+
  geom_point(aes(colour=Group, shape=Group), size=2.5, position=position_dodge(width=1))+ 
  geom_smooth(method='lm', se=T, colour="black")+
  ylab("Depression")+xlab("Overall LQ")+
  scale_colour_manual(values=groupcol)+
  theme_bw() +
  theme(text=element_text(size=s),legend.text=element_text(size=s),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="top", legend.justification="left",  plot.margin = unit(c(0.2, 0.2, 1, 0.2), "cm"))
#whole sample: VAS and LQ
corr_vaslq_w <- ggplot(dp, aes(x=lq_overall, y=vas_pain_intensity))+
  facet_grid(all~Time)+ 
  geom_point(aes(colour=Group, shape=Group),size=2.5, position=position_dodge(width=1))+ 
  geom_smooth(method='lm', se=T, colour="black")+
  ylab("Pain intensity VAS")+xlab("Overall LQ")+
  scale_colour_manual(values=groupcol)+
  theme_bw() +
  theme(text=element_text(size=s),legend.text=element_text(size=s),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="top", legend.justification="left",  plot.margin = unit(c(0.2, 0.2, 1, 0.2), "cm"))


#pdf('./figures/corr_dep_LQ.pdf', height=10, width=9)
plot_corr_dep_lq <- plot_grid(corr_dlq_w, corr_dlq_g, ncol=1, rel_heights=c(1,1.35), labels=c("A", "B"), label_size=18)
#dev.off()

#pdf('./figures/corr_vas_LQ.pdf', height=10, width=9)
plot_corr_vas_lq <- plot_grid(corr_vaslq_w, corr_vaslq_g, ncol=1, rel_heights=c(1,1.35), labels=c("A", "B"), label_size=18)
#dev.off()

#whole sample: soms and LQ
corr_somsilq_w <- ggplot(dp, aes(x=lq_overall, y=soms_intensity))+
  facet_grid(all~Time)+ 
  geom_point(aes(colour=Group, shape=Group),size=2.5, position=position_dodge(width=1))+  
  geom_smooth(method='lm', se=T, colour="black")+
  ylab("SOMS intensity")+xlab("Overall LQ")+
  scale_colour_manual(values=groupcol)+
  theme_bw() +
  theme(text=element_text(size=s),legend.text=element_text(size=s),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="top", legend.justification="left",  plot.margin = unit(c(0.2, 0.2, 1, 0.2), "cm"))
corr_somsnlq_w <- ggplot(dp, aes(x=lq_overall, y=soms_n))+
  facet_grid(all~Time)+ 
  geom_point(aes(colour=Group, shape=Group),size=2.5, position=position_dodge(width=1))+ 
  geom_smooth(method='lm', se=T, colour="black")+
  ylab("SOMS number of symptoms")+xlab("Overall LQ")+
  scale_colour_manual(values=groupcol)+
  theme_bw() +
  theme(text=element_text(size=s),legend.text=element_text(size=s),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="top", legend.justification="left",  plot.margin = unit(c(0.2, 0.2, 1, 0.2), "cm"))
corr_dp_w <- ggplot(dp, aes(x=vas_pain_intensity, y=hads_depression))+
  facet_grid(all~Time)+
  geom_point(aes(colour=Group, shape=Group), size=2.5, position=position_dodge(width=1))+ 
  geom_smooth(method='lm', se=T, colour="black")+
  ylab("Depression")+xlab("VAS pain intensity")+
  scale_colour_manual(values=groupcol)+
  theme_bw() +
  theme(text=element_text(size=s),legend.text=element_text(size=s),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="top", legend.justification="left",  plot.margin = unit(c(0.2, 0.2, 1, 0.2), "cm"))
pdf('./figures/correlations.pdf', height=4, width=9)
corr_dlq_w
corr_vaslq_w
corr_somsnlq_w
corr_somsilq_w
corr_dp_w
dev.off()


# histogram testing across time of year T0-T2-T3 ####
#proportion per season
prop.season <- dp %>% 
  group_by(Group, Time, season) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# #add categories with 0 occurences in the figure - too much work for month - not done
# empty <- data.frame(
#   Group = factor(c("Control", "Treatment", "Treatment", "Treatment")),
#   Time = factor(c("T0","T0", "T2", "T2")), 
#   season = factor(c(NA, NA, "Jan-Mar", NA)), 
#   n = rep(0,4), 
#   freq = rep(0,4))
# prop.season <- rbind(as.data.frame(prop.season), empty)

plot_season <- ggplot(prop.season, aes(x = season, y=freq, colour=Group, group=Group)) +
  geom_line(aes(linetype=Group), size=1)+
  geom_point(aes(shape=Group), size=3)+
  facet_grid(~Time)+
  labs(y="Proportion", x="Season")+scale_y_continuous(limits=c(0, 0.55))+
  scale_colour_manual(values=groupcol, drop=FALSE)+
  scale_linetype_manual(values=c(6,1))+
  theme_bw()+ 
  theme(text=element_text(size=25), axis.text=element_text(size=11),
        legend.position="top", legend.justification="left",  plot.margin = unit(c(0.2, 0.2, 1, 0.2), "cm"))

#proportion of patients per month
prop.month <- dp %>% 
  group_by(Group, Time, month) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

#plot the proportion as bars
plot_month <- ggplot(prop.month, aes(x = month, y=freq, colour=Group, group=Group)) +
  geom_line(aes(linetype=Group), size=1)+
  geom_point(aes(shape=Group), size=3)+
  facet_grid(~Time)+
  labs(y="Proportion", ymin=0, x="Month")+scale_y_continuous(limits=c(0, 0.55))+
  scale_colour_manual(values=groupcol, drop=FALSE)+
  scale_linetype_manual(values=c(6,1))+
  theme_bw()+ 
  theme(text=element_text(size=25), axis.text=element_text(size=12),
        panel.grid.minor= element_blank(),
        legend.position="none", legend.justification="left",  plot.margin = unit(c(0.2, 0.2, 1, 0.2), "cm"))

plot_time_of_year <- plot_grid(plot_season, plot_month, ncol=1, rel_heights=c(1.1,1), labels=c("A", "B"), label_size=18)
pdf('./figures/time_of_year.pdf', width=11, height=10); plot_time_of_year; dev.off()

#boxplot life quality across time of year
plot_lq_season <- ggplot(dp, aes(x=season, y=lq_overall, fill=Group))+
  geom_boxplot(size=0.7)+
  scale_fill_manual(values=groupcol)+
  labs(y="Life Quality", x="Month")+
  theme_bw()+ 
  theme(text=element_text(size=s),legend.text=element_text(size=s),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="right", legend.justification="left",  plot.margin = unit(c(0.2, 0.2, 1, 0.2), "cm"))
pdf('./figures/life_quality_by_season.pdf', width=6, height=4.5); plot_lq_season; dev.off()

plot_lq_season_T <- ggplot(dp, aes(x=season, y=lq_overall, fill=Group))+
  geom_boxplot(size=0.7)+
  facet_grid(rows=vars(Time))+
  scale_fill_manual(values=groupcol)+
  labs(y="Life Quality", x="Month")+
  theme_bw()+ 
  theme(text=element_text(size=s),legend.text=element_text(size=s),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="none", legend.justification="left",  plot.margin = unit(c(0.2, 0.2, 1, 0.2), "cm"))

pdf('./figures/life_quality_by_season_perT.pdf', width=7, height=8); plot_lq_season_T; dev.off()

#plot_grid(month_lines, month_histo, ncol=1, rel_heights=c(2,3))

# expectations about therapy (T0) and evaluation of therapy (T3) ####
#categorical plot function with 2 time points - delete NAs in scale_x_discrete when LOCF analysis
cat_plot_23t <- function(df, variable, legendtitle, ylim){
  if(missing(legendtitle)){
    legendtitle = deparse(substitute(variable))
    legendtitle = substr(legendtitle, 5, nchar(legendtitle))
  }
  if (analysis == "AV") {
    existing_levels <- c("none",2,3,4,"high", NA)
  } else {
    existing_levels <- c("none",2,3,4,"high")
  }
  ggplot(df, aes(x=variable, fill=variable))+
    facet_grid(Group~Time)+
    geom_histogram(stat="count", position="dodge") +#bars
    scale_y_continuous(breaks=seq(2,ylim,2), limits=c(0,ylim)) +
    scale_x_discrete(limits=existing_levels)+
    labs(y="Proportion", x=legendtitle) +
    scale_fill_manual(values=increasing, na.value="grey")+
    theme_bw() + 
    theme(text=element_text(size=s),legend.text=element_text(size=s),
          panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
          legend.position="none")
}

#make plots
dp03$expectation.success_medt <- factor(dp03$expectation.success_medt)
dp03$expectation.success_psyt <- factor(dp03$expectation.success_psyt)
d_eet <- dp03 %>% #expectation evaluation therapy
  dplyr::select(subid, expectation.success_medt, expectation.success_psyt, Group, Time)%>%
  mutate(Time = factor(Time, labels = c(unique(paste("Expectation at",Time)))))
plot_medt <- cat_plot_23t(d_eet, d_eet$expectation.success_medt, "Medical Therapy", 12) + 
  ggtitle("Medical Therapy") +xlab("")
plot_medt <- plot_grid(plot_medt, labels=c("A"), label_size=18)

#psy therapy control at t0
dc0 <- subset(d_eet, Group=="Control" & Time=="Expectation at T0")
cg_t0 <- cat_plot_23t(dc0, dc0$expectation.success_psyt, "Psychological Therapy", 8)+
  ggtitle("Psychological Therapy")+xlab("")

#psy therapy treatment at t0 and t3
dt03 <- subset(d_eet, Group=="Treatment") %>%
  droplevels()
tg_t03 <-  cat_plot_23t(dt03, dt03$expectation.success_psyt, "Psychological Therapy", 8) +xlab("")
plot_psyt <- plot_grid(plot_grid(cg_t0, blank, ncol=2, rel_widths=c(1.22,1)), tg_t03, rel_heights = c(1.1,1), ncol=1, labels="B", label_size=18)
#psy and med therapy together
plot_exp_eval <- plot_grid(plot_medt, plot_psyt, ncol=1, rel_heights = c(1,1.15))
pdf('./figures/expectations_evaluation_therapy_T2.pdf', height=12.5, width=7); plot_exp_eval; dev.off()

# occupation at T0 and T3 ####
plot_occ03 <- ggplot(dp03, aes(x=Time, fill=Occupation)) + #dataset and variables to plot
  facet_grid(~Group) +
  geom_bar(stat="count", position="fill")  + #which statistic (count cases, and show as proportion)
  labs(y="Proportion") + #axes and title labels
  scale_fill_manual(values=rev(brewer.pal(9, "YlGnBu")[c(2,3,4,6,7,8,9)]), na.value="grey", guide=guide_legend(keywidth = 2, keyheight = 2))+
  theme_bw() +   #Remove dark background
  theme(text=element_text(size=s),legend.text=element_text(size=s), #set size of text in general an in legend
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="right", plot.margin = unit(rep(0.5, 4), "cm"))
pdf('./figures/occupation_change.pdf', height=4, width=6); plot_occ03; dev.off()

# psychotherapy motivation at T0 and T2 #### 
plot_ptmot <- cont_plot_23t(dp02, dp02$pt_motivation, "Psychotherapy motivation", 210, 5, 0.8)
pdf('./figures/pt_motivation.pdf', height=4.5, width=6); plot_ptmot; dev.off()

# sickness behaviours T0 and T3 (Krankheitsverhalten) ####
plot_visits <- ggplot(dp03, aes(x=Time, y=specialist_visits, fill=Group))+
  geom_boxplot(aes(colour=Group), size=1, alpha=0.5, position=position_dodge(0.9))+
  geom_dotplot(binwidth = 5, dotsize=0.2, binaxis = "y", stroke=2, stackdir = "center", position="dodge", show.legend=F)+
  scale_y_continuous(name="Specialist Visits") +#, limits=c(0,125))+ #axes labels
  scale_fill_manual(values=groupcol)+scale_color_manual(values=groupcol)+
  theme_bw() +   #Remove dark background
  theme(text=element_text(size=16),legend.text=element_text(size=16), axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="bottom", plot.margin=unit(rep(0.7,4), "cm"))
#lost_days
plot_lostdays <- ggplot(dp03, aes(x=Time, y=lost_days, fill=Group))+
  geom_boxplot(aes(colour=Group), size=1, alpha=0.5, position=position_dodge(0.9))+
  geom_dotplot(binwidth = 5, dotsize=1, binaxis = "y", stroke=2, stackdir = "center", position="dodge", show.legend=F)+
  scale_y_continuous(name="Lost days") +#, limits=c(0,125))+ #axes labels
  scale_fill_manual(values=groupcol)+scale_color_manual(values=groupcol)+
  theme_bw() +   #Remove dark background
  theme(text=element_text(size=16),legend.text=element_text(size=16), axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="none", plot.margin=unit(rep(0.7,4), "cm"))
plot_sickbeh <- plot_grid(plot_visits, plot_lostdays, labels=c("A", "B"), label_size=18, align = "h", axis="b")
pdf('./figures/sickness_behaviour.pdf', height=4, width=8); plot_sickbeh; dev.off()

#save ./figures for use in RMarkdown: 
save.image("~/Documents/Analyse_Julia_Sonnleitner/pain/figures/pain_plots.RData")
