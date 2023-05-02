#analysis and figures for T0-T1 participants

#load R-libraries
library(dplyr)
library(pastecs) #for stat.desc
library(ggplot2)
library(scales)

#read data and prepare the data ####
dt0t1 <- read.csv2('../data/data_pain_t0t1.csv', header=T)
summary(dt0t1)

#format variables
d <- dt0t1 %>%
  mutate(subid = factor(subid)) %>%
  mutate(group = factor(group, levels=c(0,1), labels = c("control", "treatment"))) %>%
  mutate(gender = factor(gender, levels = c(0,1), labels = c("men", "women"))) %>%
  mutate(cpsq = factor(cpsq, order = TRUE, levels = c(1:5)))
summary(d)
# save(d, file='../data/data_pain_t0t1.RData')

#calculate differences for all T0-T2-T3 variables
diffs <- d %>%
  arrange(subid,time)%>%
  group_by(subid)%>%
  mutate_at(vars(cpsq:hads_depression),#variables for which diff is to be calculated
            funs(diff = c(diff(.), NA)))%>% #diff subtracts 2nd-1st and puts it in T0 lines, 3-2nd put in T2 lines, leaves NA in T3 lines
  #delete time point variables (keep only differences)
  select(-c(cpsq:hads_depression))%>%
  ungroup()%>% 
  na.omit %>% #delete lines with NAs, i.e. all T3 lines
  mutate(diff = factor(ifelse(time == "T0", "T2-T0", "T3-T2"))) %>% #rename time to time differences
  select(-time)#delete time point variable
diffs <- diffs[,c(1,2,length(diffs),3:length(diffs)-1)] #put diffs variable in 3rd column
head(diffs)

#descriptive analysis ####

#do you want to write tables to Excel or not? 
excel = TRUE # TRUE or FALSE

#CPSQ
#calculate proportion for cpsq
cpsq_prop <- d %>%
  group_by(time, cpsq) %>%
  summarise(nresp = n()) %>%
  mutate(proportion = nresp/sum(nresp)*100) %>%
  ungroup()%>%
  select(-nresp); cpsq_prop
if(excel) {write.csv(cpsq_prop, file="output/cpsq_proportions_t0t1.csv",row.names=F)}

#continuous variables that change from T0-T1: anxiety, depression, VAS

#look at distribution of differences between time points: vas not normally distributed
round(stat.desc(with(diffs, cbind(vas_pain_intensity_diff, hads_anxiety_diff, hads_depression_diff)), basic=F, norm=T), digits=2)

#table with important statistics for the dissertation
#(ignore the warning created by function ci(), all is well)
desc_bytime <- d %>%
  group_by(time)%>%
  mutate(n=n())%>%
  summarise_at(.vars = vars(vas_pain_intensity, hads_anxiety, hads_depression), #variables to summarise
               funs(mean(.,na.rm = T), sd(., na.rm=T), median(., na.rm=T), #mean, sd, median
                    lowCI =ci(., na.rm=T)[2], hiCI =ci(., na.rm=T)[3])) #95% CI (gmodels library)

#reorder so that all parameters for one questionnaire follow each other
sort3 <- function(x){seq(x,dim(desc_bytime)[2],3)} #Funktion that creates numbers in steps of 3 (because 3 variables), as often as the number of different statistic parameters (mean, sd, ...)
desc_bytime <- desc_bytime[,c(1,2,sapply(c(2:4), sort3))]
desc_bytime <- round(desc_bytime[,c(3:length(desc_bytime))],digits=2) #round to 2 digits
if(excel) {write.csv(t(desc_bytime), file="output/descriptives_by_time_t0t1.csv")} #print this to excel table

#figures ####
#format variables for Graphs: start with capital letters
dp <- d %>% #dp = data plots
  dplyr::rename(Group=group, 
                CPSQ = cpsq, 
                Time = time)

#plot parameters ####
s = 25 # text size
cols <- rev(brewer.pal(9, "YlGnBu")[7]) #only the colour of the Treatment group
#cols <- rev(brewer.pal(12,"Set3")[10])
increasing <- brewer.pal(8,"YlGnBu")[c(4:8)]

pdf('figures/changes_continuous_vars_t0t1.pdf', height=7, width=7)
#VAS pain intensity
ggplot(dp, aes(x=Time, y=vas_pain_intensity, fill=Group))+
  stat_summary(fun.y = mean, geom="bar", size=1.3,position=position_dodge(0.95),alpha = 0.5, aes(colour=Group))+#bars
  geom_dotplot(binwidth=0.5, dotsize=0.3, binaxis = "y", stroke=2, stackdir = "center", position="dodge", show.legend=F)+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2, size = 1.3, position=position_dodge(0.95), show.legend=F, aes(colour=Group))+
  scale_y_continuous(name="VAS pain intensity")+ #axes labels
  scale_fill_manual(values=cols)+scale_color_manual(values=cols)+
  theme_bw() +   #Remove dark background
  theme(text=element_text(size=s),legend.text=element_text(size=s),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="none")
#anxiety
ggplot(dp, aes(x=Time, y=hads_anxiety,fill=Group))+
  stat_summary(fun.y = mean, geom="bar", size=1.3,position=position_dodge(0.95),alpha = 0.5, aes(colour=Group))+#bars
  geom_dotplot(binwidth=2, dotsize=0.17, binaxis = "y", stroke=2, stackdir = "center", position="dodge", show.legend=F)+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2, size = 1.3, position=position_dodge(0.95),aes(colour=Group), show.legend=F)+
  scale_y_continuous(name="Anxiety")+ #axes labels
  scale_fill_manual(values=cols)+scale_color_manual(values=cols)+
  theme_bw() +   #Remove dark background
  theme(text=element_text(size=s),legend.text=element_text(size=s),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="none")
#depression
ggplot(dp, aes(x=Time, y=hads_depression,fill=Group))+
  stat_summary(fun.y = mean, geom="bar", size=1.3,position=position_dodge(0.95),alpha = 0.5, aes(colour=Group))+#bars
  geom_dotplot(binwidth=2, dotsize=0.17, binaxis = "y", stroke=2, stackdir = "center", position="dodge", show.legend=F)+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2, size = 1.3, position=position_dodge(0.95),aes(colour=Group), show.legend=F)+
  scale_y_continuous(name="Depression")+ #axes labels
  scale_fill_manual(values=cols)+scale_color_manual(values=cols)+
  theme_bw() +   #Remove dark background
  theme(text=element_text(size=s),legend.text=element_text(size=s),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="none")
dev.off()

pdf('figures/change_cpsq_t0t1.pdf')
#cpsq
ggplot(dp, aes(x=CPSQ,fill=CPSQ))+
  facet_grid(~Time)+
  geom_histogram(stat="count", position="dodge")+
  scale_y_continuous(breaks=c(2,4,6,8,10), name="Count")+
  scale_fill_manual(values=increasing, na.value="grey",guide=guide_legend(byrow=F,ncol=1, keyheight=1.8))+
  theme_bw() +
  theme(text=element_text(size=s),legend.text=element_text(size=s),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#remove grid lines
        legend.position="none")
dev.off()
