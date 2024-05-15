#icd10

library(dplyr); library(ggplot2); library(cowplot); library(tidyr)

#read and prepare data ####
icd0 <- read.csv2('data/icd10.csv', header=T) 


icd1 = icd0 %>%
  arrange(axis1)%>%
  rename(Group = group)%>%
  mutate(Group = factor(Group, labels = c("Control n=21", "Treatment n=17")),
         diagnosis_type = factor(diagnosis_type, levels = c(0:3), 
                                 labels = c("All patients:\nChronic pain disorder\nwith somatic and\npsychological factors", 
                                            "Depressive disorders",                                        
                                            "Neurotic, stress-\nrelated and\nsomatoform disorders", 
                                            "Personality disorders\n(axis 2)")))
  
summary(icd1)

#number of comorbid disorders on axis 1
ncomorbid1 <- icd1 %>%
  filter(axis1 == "None")%>%
  arrange(subid)%>%
  group_by(subid)%>%
  #count number of "none" lines, subtract this from 3 possible comorbid axis 1 disorders, 3 "none" means no comorbid disorder
  mutate(n1 = 3-n())%>%
  filter(row_number() <=1)%>%
  #keep only subid and n
  select(subid, n1)%>%
  ungroup()

#subid 10 has 4 disorders (3 comorbid), therefore no none => add 4 to column n
ncomorbid1 <- add_row(ncomorbid1, subid=10, n1=3)%>%
  arrange(subid)
as.data.frame(ncomorbid1)#correct

#add to icd dataframe
icd2 <- left_join(icd1, ncomorbid1)%>%
  arrange(n_axis1)
summary(icd2)

#add comorbid disorders on axis 2 for calculating total number of comorbid disorders
ncomorbid2 <- icd2 %>%
  filter(n_axis1==1)%>%
  mutate(n2 = ifelse(axis2 !="None", 1, 0))%>%
  select(subid, n2)

#add to icd dataframe
icd3 <- left_join(icd2, ncomorbid2)%>%
  mutate(n = n1+n2)%>%
  mutate(n=factor(n))
summary(icd3)

icd <- icd3
#replace codes with disorder names for axis 1
icd$axis1<- recode_factor(icd$axis1,
                          "F45.41"="Chronic pain disorder with\nsomatic and psychological factors",
                          "F11.2"= "Mental and behavioural\ndisorders due to use of opioids",
                          "F33"=   "Recurrent depressive\ndisorder",
                          "F34.1"= "Dysthymia",
                          "F17"=   "Mental and behavioural disorders\ndue to use of tobacco",
                          "F32"=   "Depressive episode",
                          "F40"=   "Phobic anxiety disorders",
                          "F41.0"= "Panic disorder",
                          "F43.1"= "PTSD",
                          "F43.2"= "Adjustment disorder",
                          "F45.2"= "Hypochondrial disorder",
                          "F45.3"= "Somatoform autonomic\ndysfunction",
                          "F55.2"= "Abuse of non-dependence-\nproducing substances",
                          "F90.0"= "Hyperkinetic disorders")

#replace codes with disorder names for axis 2
icd$axis2 <- recode_factor(icd$axis2,
                           "F61"="Mixed and other\npersonality disorders",
                           "F60.5 "="Anankastic\npersonality disorder",
                           "F60.8" ="Narcissistic\npersonality disorders",
                           "F60.30"="Emotionally unstable\npersonality disorder")

#figure settings ####
manycolours = rep(brewer.pal(9, "YlGnBu")[c(2:9)] ,2) #colours
s=11

# axis 1 figure: all comorbid disorders in one figure ####
comorbid123 <- icd %>%
  filter(axis1 != "None")%>% 
         #n_axis1 != "1")%>%
  arrange(n)

#make figure
icd_axis1 <- ggplot(comorbid123, aes(x=axis1, fill=n))+
  geom_bar()+ coord_flip()+
  facet_grid(~Group)+
  ggtitle("Axis 1 comorbid disorders")+
  scale_fill_manual(values=manycolours[c(2,3,4,6)], name="Comorbid disorders per patient (axis 1 & 2 combined)")+
  scale_y_continuous(breaks=c(0,5,10,15,17,20,21))+
  theme_bw() +
  theme(text=element_text(size=s),legend.text=element_text(size=s), 
        axis.title=element_blank(),
        panel.grid.minor = element_blank(),legend.position="bottom")
icd_axis1



# axis 1 figure: number of comorbid disorders axis 1 ####
ncomorbid <- icd %>%
  group_by(subid)%>%
  #keep only first row of each subject
  filter(row_number() <=1)
ncomorbid$n <- factor(ncomorbid$n)

#make figure n disorders
icd_nocomorbid <- ggplot(ncomorbid, aes(x=n, fill=n))+
  geom_bar()+ coord_flip()+
  facet_grid(~Group)+
  ggtitle("Axis 1 comorbid disorders per patient")+
  scale_fill_manual(values=manycolours[c(2,3,4,6)])+
  scale_y_continuous(breaks=seq(2,12,2))+
  theme_bw() +
  theme(text=element_text(size=s),legend.text=element_text(size=s), axis.title=element_blank(),
        panel.grid.minor = element_blank(),legend.position="none")

icd_nocomorbid


# axis 2 figure ####
#filter none
axis2 <- icd %>%
    filter(axis2 != "None")
#make figure
icd_axis2 <- ggplot(axis2, aes(x=axis2, fill=n))+
  geom_bar()+ coord_flip()+
  facet_grid(~Group)+
  ggtitle("Axis 2 comorbid disorders")+
  scale_fill_manual(values=manycolours[c(3,4,6)])+ #exclude colour for zero (2 in manycolours)
  theme_bw() +
  theme(text=element_text(size=s),legend.text=element_text(size=s), axis.title=element_blank(),
        panel.grid.minor = element_blank(),legend.position="none")
icd_axis2


#figure: grouped by diagnosis type ####
#filter none
type <- icd %>%
  filter(diagnosis_type != "NA")
#make figure
icd_type <- ggplot(type, aes(x=diagnosis_type, fill=n))+
  geom_bar()+ coord_flip()+
  facet_grid(~Group)+
  ggtitle("Type of diagnosis")+
  scale_fill_manual(values=manycolours[c(2,3,4,6,8)],name="N comorbid disorders per patient across axis 1 and 2")+
#  scale_y_continuous(breaks=c(0,5,10,15,17,20,21))+
  theme_bw() +
  theme(text=element_text(size=s),legend.text=element_text(size=s), 
        axis.title=element_blank(),
        panel.grid.minor = element_blank(),legend.position="none")
icd_type


#combine and print figures including the type of diagnosis (instead of number of diagnoses per patient) ####
icd_diagnoses <- ggdraw()+
  draw_plot(icd_axis1,  x=0, y=0.37, width=1, height=0.63)+
  draw_plot(icd_axis2,  x=0, y=0, width=0.5, height=0.35)+
  draw_plot(icd_type,   x=0.5, y=0, width=0.5, height=0.35)+
  draw_plot_label(c("A", "B", "C"), 
                  x=c(0, 0, 0.5), 
                  y=c(1, 0.35, 0.35), 
                  size = 18) 
pdf('figures/icd_diagnoses.pdf', height=10, width = 8.5); icd_diagnoses; dev.off()



# # figure of only 1st comorbid disorder axis 1 - not used ####
# #filter n_axis1 == 2, becuase the first one is chronic pain for everyone, the "main" disorder for this study
# comorbid1 <- icd %>%
#   filter(n_axis1 == 2)%>%#| n_axis1 == 3)
#   filter(axis1 != "None")
# #make figure
# icd_1stcomorbid <- ggplot(comorbid1, aes(x=axis1, fill=axis1))+
#   geom_bar()+ coord_flip()+
#   facet_grid(~Group)+
#   ggtitle("Axis 1: First comorbid disorder")+
#   scale_fill_manual(values=manycolours)+
# 
#   theme_bw() +
#   theme(text=element_text(size=s),legend.text=element_text(size=s), axis.title=element_blank(),
#         panel.grid.minor = element_blank(),legend.position="none")
# icd_1stcomorbid
# 
# # figure of only 2nd comorbid disorder axis 1 - not used ####
# #filter n_axis ==3 (3 for 2nd comorbid disorder because 1 is the main disorder chronic pain)
# comorbid2 <- icd %>%
#   filter(n_axis1 == 3)%>%
#   filter(axis1 != "None")
# #figure
# icd_2ndcomorbid <- ggplot(comorbid2, aes(x=axis1, fill=axis1))+
#   geom_bar()+ coord_flip()+
#   facet_grid(~Group)+
#   ggtitle("Axis 1: Second comorbid disorder")+
# 
#   scale_fill_manual(values=manycolours)+
#   scale_y_continuous(limits = c(0,8))+
#   theme_bw() +
#   theme(text=element_text(size=s),legend.text=element_text(size=s), axis.title=element_blank(), 
#         panel.grid.minor = element_blank(),legend.position="none")
# icd_2ndcomorbid
# 

# patients per intermediate category of disorder ####

#read and prepare data 
cat0 <- read.csv2('data/icd10_largecategories.csv', header=T) 

cat1 = cat0 %>%
  rename(Group = group)%>%
  mutate(Group = factor(Group, labels = c("Control n=21", "Treatment n=17")))
       
levels(cat1$disorder) <- c("Accentuation of\npersonality traits", "Anancastic\npersonality disorder", 
                          "Anxiety", "Combined\npersonality disorder", "Depression", 
                          "Emotionally unstable\npersonality disorder", "Harmful use of non-\ndependence producing substances", 
                          "Hyperkinetic disorders", "Mental and behavioural\ndisorders caused by opioids", 
                          "Mental and behavioural disorders\ncaused by tobacco", "Narcissistic\npersonality disorder", 
                          "Somatoform disorders other\nthan chronic pain", "Trauma & stress")
#as.vector(levels(cat1$disorder), mode="expression")

# axis 1 categories figure
cataxis1 <- cat1 %>%
  filter(axis == 1) %>% 
  arrange(frequency)
cataxis1$All <- rep("Entire sample n=38", nrow(cataxis1))

#entire sample
plot.cataxis1all <- ggplot(cataxis1, aes(x=frequency, y=reorder(disorder, frequency), fill=disorder))+
  geom_bar(stat="identity")+ 
  facet_grid(~All)+
  ggtitle("Axis 1 disorders")+
  scale_fill_manual(values=manycolours)+
  theme_bw() +
  theme(text=element_text(size=s),legend.text=element_text(size=s), axis.title=element_blank(),
        panel.grid.minor = element_blank(),legend.position="none")
plot.cataxis1all

#make figure per group
plot.cataxis1 <- ggplot(cataxis1, aes(x=frequency, y=reorder(disorder, frequency), fill=disorder))+
  geom_bar(stat="identity")+ 
  facet_grid(~Group)+
  ggtitle("")+
  xlim(c(0,20))+
  scale_fill_manual(values=manycolours)+
  theme_bw() +
  theme(text=element_text(size=s),legend.text=element_text(size=s), axis.title=element_blank(),
        axis.text.y = element_blank(), panel.grid.minor = element_blank(),legend.position="none")
plot.cataxis1

# #put axis 1 plots together
# graphics.off()
# pdf('figures/icd10_axis1_categories.pdf', height=4, width=8)
# plot_grid(plot.cataxis1all, plot.cataxis1, rel_widths=c(1.3,1))
# dev.off()

# Axis 2 categories figure
cataxis2 <- cat1 %>%
  filter(axis == 2) %>% 
  arrange(frequency) 
cataxis2$All <- rep("Entire sample n=38", nrow(cataxis2))


#entire sample
plot.cataxis2all <- ggplot(cataxis2, aes(x=frequency, y=reorder(disorder, frequency), fill=disorder))+
  geom_bar(stat="identity")+ 
  facet_grid(~All)+
  ggtitle("Axis 2 disorders")+
  scale_fill_manual(values=manycolours)+
  theme_bw() +
  theme(text=element_text(size=s),legend.text=element_text(size=s), axis.title=element_blank(),
        panel.grid.minor = element_blank(),legend.position="none")
plot.cataxis2all

#per group
plot.cataxis2 <- ggplot(cataxis2, aes(x=frequency, y=reorder(disorder, frequency), fill=disorder))+
  geom_bar(stat="identity")+ 
  facet_grid(~Group)+
  ggtitle("")+
  xlim(c(0,6))+
  scale_fill_manual(values=manycolours)+
  theme_bw() +
  theme(text=element_text(size=s),legend.text=element_text(size=s), axis.title=element_blank(),
        axis.text.y=element_blank(),panel.grid.minor = element_blank(),legend.position="none")
plot.cataxis2

#with text labels
#plot_grid(plot.cataxis2all, plot.cataxis2, rel_widths=c(1,1.5))

# #no labels for group plot
# graphics.off()
# pdf('figures/icd10_axis2_categories.pdf', height=4, width=7.5)
# plot_grid(plot.cataxis2all, plot.cataxis2, rel_widths=c(1,1.1))
# dev.off()




