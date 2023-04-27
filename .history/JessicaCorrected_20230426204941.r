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
jessicaData <- ldply( .data = list.files('C:/Users/olreu', 
                                         pattern = '(Data for Stats-2.csv)', full.names = TRUE),
                      .fun = read.csv, header = TRUE, sep=',')
str(jessicaData)


# group the data
CD13<- ddply(jessicaData, c("Animal","Condition","Age"), summarise, CD13AREA= mean(CD13..Area),
            CD31AREA= mean(CD31..Area), Ratio=mean(RATIO))

CD13$Age[CD13$Age=="6 Months "]<-"6 Months"
uniqueAges <- unique(CD13$Age)

# Manually specify levels for Age and CD13PercentageArea columns
#CD13$Age <- as.factor(CD13$Age)
#CD13$Condition<- as.factor(CD13$Condition)


# Check the levels of Age and CD13PercentageArea columns after conversion



# two-way anova
res.aov2 <- aov(CD13$CD13AREA ~ CD13$Condition + CD13$Age, data = CD13)
summary(res.aov2)

Geno_Age<-interaction(CD13$Condition, CD13$Age)
res.aov3 <- aov(CD13$CD13AREA~ CD13$Condition * CD13$Age, data = CD13)
summary(res.aov3)


library(multcomp)
summary(glht(res.aov2, linfct = mcp(Condition = "Tukey")))


#######
# all measurements (multiple numbers per mouse)

# groups
#The key difference between a vector and other data types in R Studio is that a vector can only hold values of the same data type.
age_order <- c("3 Months", "6 Months", "12 Months")

CD13$Age <- factor(CD13$Age, levels = age_order)

df1 <- summarySE(CD13, measurevar = "CD13AREA", groupvars = c("Condition", "Age"))

df1 <- df1 %>% rename(`CD13 Area` = CD13AREA)

df2 <- summarySE(CD13, measurevar = "CD31AREA", groupvars = c("Condition", "Age"))

df2 <- df2 %>% rename(`CD31 Area` = CD31AREA)

df3 <- summarySE(CD13, measurevar = "Ratio", groupvars = c("Condition", "Age"))

ggplot(data = df1, aes(x = Age, y = `CD13 Area`, fill = Condition)) + 
  geom_bar(stat = "identity", position = position_dodge(), colour = "black") +
  geom_errorbar(aes(ymin = `CD13 Area` - se, ymax = `CD13 Area` + se), 
                colour = "black", width = 0.1, position = position_dodge(0.9)) +
  ylab("CD13 Area") +
  scale_fill_manual(values = cbbPalette1) + 
  max.theme

ggplot(data = df2, aes(x = Age, y = `CD31 Area`, fill = Condition)) + 
  geom_bar(stat = "identity", position = position_dodge(), colour = "black") +
  geom_errorbar(aes(ymin = `CD31 Area` - se, ymax = `CD31 Area` + se), 
                colour = "black", width = 0.1, position = position_dodge(0.9)) +
  ylab("CD31 Area") +
  scale_fill_manual(values = cbbPalette1) + 
  max.theme

ggplot(data = df3, aes(x = Age, y = Ratio, fill = Condition)) + 
  geom_bar(stat = "identity", position = position_dodge(), colour = "black") +
  geom_errorbar(aes(ymin = Ratio - se, ymax = Ratio + se), 
                colour = "black", width = 0.1, position = position_dodge(0.9)) +
  ylab("Ratio") +
  scale_fill_manual(values = cbbPalette1) + 
  max.theme

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
