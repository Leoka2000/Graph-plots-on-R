library("lme4")
library("lmerTest")
library("lattice")
library("plyr")
library("dplyr")
library("ggplot2")
library("emmeans")
library("Rmisc")
library("multcomp")
library("reshape2")
library("tidyr")
library("Hmisc")
library("stringr")

#install.packages("stringr")
#library("R.matlab")
#install.packages("rmatio")
#library("rmatio")

#install.packages('nortest')
#library(nortest)
#install.packages('goftest')
library("goftest")

########################

# theme for plots
max.theme <- theme_classic() + 
  theme(
    text=element_text(size=12),
    axis.text=element_text(size=12),
    axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5),
    axis.title=element_text(size=14, face="bold"),
    axis.title.y=element_text(vjust=1),
    legend.text=element_text(size=12),
    legend.title=element_text(size=14, face="bold"),
    axis.line.x = element_line(colour = 'black', size=1, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=1, linetype='solid')) 


###########
# NOTES
# COLOUR BLIND FRIENDLY PALETTE FOR PLOTS
# The palette with black:
cbbPalette2 <- c("#009E73","#E69F00","#56B4E9","#CC79A7","#F0E442")
cbbPalette1 <- c("#D55E00", "#56B4E9", "#009E73","#F0E442", "#0072B2", "#E69F00", "#CC79A7")
cbbPalette <- c("#D55E00", "#56B4E9", "#009E73","#F0E442", "#0072B2", "#E69F00", "#CC79A7")

# New Files

##Loading data


                                #change the path to the computer
my_data <- ldply( .data = list.files('C:/Users/olreu', 
                                         pattern = '(test)', full.names = TRUE),
                      .fun = read.csv, header = TRUE)

#change the path to the computer
my_data2 <- ldply( .data = list.files('C:/Users/olreu', 
                                     pattern = '(Data_organized_3)', full.names = TRUE),
                  .fun = read.csv, header = TRUE, sep=';')
str(my_data)

# group the data
grouped<- ddply(my_data2, c("Animal","Condition","Age"), summarise, Pearsons=mean(Person.s.value))

grouped$Genotype<-as.factor(grouped$Condition)
grouped$Age<-as.factor(grouped$Age)



# two-way anova
res.aov2 <- aov(Pearsons ~ Genotype + Age, data = grouped)
summary(res.aov2)

Geno_Age<-interaction(grouped$Genotype, grouped$Age)
res.aov3 <- aov(Pearsons ~ Genotype * Age, data = grouped)
summary(res.aov3)


library(multcomp)
summary(glht(res.aov2, linfct = mcp(Genotype = "Tukey")))



#######
# all measurements (multiple numbers per mouse)


# groups
df1<- summarySE(grouped, measurevar = "Pearsons", groupvars = c("Genotype","Age"))
#df2<- summarySE(grouped, measurevar = "Pearson", groupvars= c("Condition", "Age", "Sex"))

ggplot(data=df1, aes(x=Age, y= Pearsons, fill=Genotype)) + 
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  geom_errorbar(aes(ymin=Pearsons-se, ymax=Pearsons+se), colour="black", width=.1,  position=position_dodge(.9)) +
  ylab("Pearsons") +
  scale_fill_manual(values=cbbPalette1) + 
  max.theme

#ggplot(data=subset(df2, Condition=="Tg"), aes(x=interaction(Condition, Sex), y= RATIO, fill=Age)) + 
 # geom_bar(stat="identity", position=position_dodge(), colour="black") +
#  geom_errorbar(aes(ymin=RATIO-se, ymax=RATIO+se), colour="black", width=.1,  position=position_dodge(.9)) +
 # ylab("RATIO") +
#  scale_fill_manual(values=cbbPalette1) + 
#  max.theme

# Leo TO DO
# rearrange graph to 3,6, 12
# rearrange WT and then Tg

# repeat the above code for Suzie and Sandrines data




## STATS

Gen_Age<- interaction(my_data2$Condition, my_data2$Age)

#Ratio
m1 = lmer(RATIO ~ (1|Animal), my_data2,REML=FALSE)
m2 = lmer(RATIO ~ Condition + (1|Animal), my_data2,REML=FALSE)
m3 = lmer(RATIO ~ Age + (1|Animal), my_data2,REML=FALSE)
m4 = lmer(RATIO ~ Gen_Age + (1|Animal), my_data2,REML=FALSE)

m.anova <- anova(m1, m2, m3, m4)
print(m.anova)

ratio.pv<- glht(m4, mcp(Gen_Age= "Tukey"))
summary(ratio.pv)


old_transgenics<-subset(my_data2, Age!="3 Months")

Sex_Cond<- interaction(old_transgenics$Condition, old_transgenics$Condition)

#Sex
m1a = lmer(RATIO ~ (1|Animal), old_transgenics,REML=FALSE)
m2a = lmer(RATIO ~ Sex + (1|Animal), old_transgenics,REML=FALSE)
m3a = lmer(RATIO ~ Condition + (1|Animal), old_transgenics,REML=FALSE)
m4a = lmer(RATIO ~ Sex_Cond + (1|Animal), old_transgenics,REML=FALSE)

ma.anova <- anova(m1a, m2a, m3a, m4a)
print(ma.anova)
