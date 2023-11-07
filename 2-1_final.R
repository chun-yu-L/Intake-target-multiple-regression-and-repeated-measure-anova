# -*- coding: utf-8 -*-

# Data from: https://datadryad.org/stash/dataset/doi:10.5061/dryad.fg35f75
# original paper：https://doi.org/10.1093/beheco/ary142

library(tidyverse)
exp2_1 <- read.table("./Exp2_target.csv", 
                     header=TRUE, stringsAsFactors=TRUE, sep=",", na.strings="NA", dec=".", 
                     strip.white=TRUE) %>% as_tibble()

str(exp2_1)
exp2_1$beetle <- as.factor(exp2_1$beetle)
exp2_1$age <- as.factor(exp2_1$age)
exp2_1$diet.pair <- as.factor(exp2_1$diet.pair)
str(exp2_1)

#########基礎整理######
head(exp2_1)
str(exp2_1)
exp2_1$beetle <- as.factor(exp2_1$beetle)
exp2_1$age <- as.factor(exp2_1$age)
exp2_1$diet.pair <- as.factor(exp2_1$diet.pair)
levels(exp2_1$age)
levels(exp2_1$beetle)
levels(exp2_1$diet.pair)
levels(exp2_1$sex)
levels(exp2_1$choice)
table(exp2_1$beetle)  #這個是指 beetle "1" 有1個、"2" 有1個....."168" 有1個，他應該是用來確認有沒有輸入錯誤
dim(exp2_1)
###################################



############################### Multiple regression ###########################
################################# eaten ratio

pratio <- mutate(protein.perc = protein.eaten/total.eaten, exp2_1)
str(select(pratio, beetle, age, diet.pair,choice, sex,
           fat.eaten, protein.eaten, total.eaten, protein.perc))
pratio_target <- lm(protein.perc ~ age*diet.pair*sex, data = pratio)

summary(pratio_target)
anova(pratio_target)
###########檢測VIF
pratio_target_vif <- lm(protein.perc ~ age+diet.pair+sex, data = pratio)
library(car)
vif(pratio_target_vif)
####VIF不要檢測interaction，用+號的就好。因為把兩個沒相關的乘起來也會變相關↓
pratio_target_vif <- lm(protein.perc ~ age+diet.pair+sex, data = pratio)
vif(pratio_target_vif)

###########看圖檢測multiple regression的前提假設
plot(pratio_target)
par(mfrow=c(2,2)) ##跑這個之後再跑plot會把4張圖拼成一張(類似autoplot)
plot(pratio_target)
par(mfrow=c(1,1)) ##改回一次一張
library(ggfortify)
autoplot(pratio_target, which = 1:6, ncol = 3, label.size = 3) + theme_bw()
autoplot(pratio_target)
library(car)
qqPlot(pratio_target) ##normality Q-Q plot

###########統計檢測multiple regression的前提假設
##變異數
leveneTest(pratio_target)
ncvTest(pratio_target)
##常態分佈
shapiro.test(resid(pratio_target))
library(nortest)
pearson.test(pratio_target$residuals)
## model fit
summary(lm(abs(resid(pratio_target))~fitted(pratio_target)))
plot(fitted(pratio_target),
       residuals(pratio_target), xlab = "Fitted
Values", ylab = "Residuals")
lines(smooth.spline(fitted(pratio_target),
                      residuals(pratio_target)), col="red")
plot(pratio_target,1)


###########Results
summary(pratio_target)
anova(pratio_target)


############################# eaten weight
str(select(exp2_1, beetle, age, diet.pair, sex, fat.eaten, protein.eaten, total.eaten))

############# Fat target
fat_target <- lm(fat.eaten ~ age*diet.pair*sex, data = exp2_1)

## multicollinearity
library(car)
fat_target_vif <- lm(fat.eaten ~ age+diet.pair+sex, data = exp2_1)
vif(fat_target_vif)

######assumption check
## figure check
library(ggfortify)
autoplot(fat_target, which = 1:6, ncol = 3, label.size = 3) + theme_bw()
autoplot(fat_target)
## normality
shapiro.test(resid(fat_target))
library(nortest)
pearson.test(fat_target$residuals)
qqPlot(fat_target)
## homoscedasticity
leveneTest(fat_target)
ncvTest(fat_target)
##model fit
summary(lm(abs(resid(fat_target))~fitted(fat_target)))
plot(fitted(fat_target),
     residuals(fat_target), xlab = "Fitted
Values", ylab = "Residuals")
lines(smooth.spline(fitted(fat_target),
                    residuals(fat_target)), col="red")
plot(fat_target,1)

##results
summary(fat_target)
anova(fat_target)

fat_target_2 <- lm(fat.eaten ~ age + diet.pair + sex, data = exp2_1)

## w/o interactions assumption checking
autoplot(fat_target_2)
## model comparison
summary(fat_target)$adj.r.squared
summary(fat_target_2)$adj.r.squared
AIC(fat_target, fat_target_2)
BIC(fat_target, fat_target_2)
library(lmtest)
lrtest(fat_target, fat_target_2)
anova(fat_target, fat_target_2, test = "LRT")

## w/o interaction results
summary(fat_target_2)
anova(fat_target_2)



############ Protein target
protein_target <- lm(protein.eaten ~ age*diet.pair*sex, data = exp2_1)

##multicollinearity
protein_target_vif <- lm(protein.eaten ~ age+diet.pair+sex, data = exp2_1)
vif(protein_target_vif)
#### assumption check
## figure check
qqPlot(protein_target)
autoplot(protein_target, which = 1:6, ncol = 3, label.size = 3) + theme_bw()
autoplot(protein_target)
plot(protein_target)
##normality
shapiro.test(resid(protein_target))
pearson.test(protein_target$residuals)
## homoscedasticity
leveneTest(protein_target)
ncvTest(protein_target)
##model fit
summary(lm(abs(resid(protein_target))~fitted(protein_target)))
plot(fitted(protein_target),
     residuals(protein_target), xlab = "Fitted
Values", ylab = "Residuals")
lines(smooth.spline(fitted(protein_target),
                    residuals(protein_target)), col="red")
plot(protein_target,1)

## results
summary(protein_target)
anova(protein_target)

protein_target_2 <- lm(protein.eaten ~ age + diet.pair + sex, data = exp2_1)

## w/o interactions assumption checking
autoplot(protein_target_2)
## model comparison
summary(protein_target)$adj.r.squared
summary(protein_target_2)$adj.r.squared
AIC(protein_target, protein_target_2)
BIC(protein_target, protein_target_2)
library(lmtest)
lrtest(protein_target, protein_target_2)
anova(protein_target, protein_target_2, test = "LRT")

## w/o interaction results
summary(protein_target_2)
anova(protein_target_2)


################################## Data visualization ####################
library(tidyverse)

exp2_1 <- read.table("./Exp2_target.csv", 
                     header=TRUE, stringsAsFactors=TRUE, sep=",", na.strings="NA", dec=".", 
                     strip.white=TRUE) %>%  as_tibble()

exp2_1$beetle <- as.factor(exp2_1$beetle)
exp2_1$age <- as.factor(exp2_1$age)
exp2_1$diet.pair <- as.factor(exp2_1$diet.pair)

c <- exp2_1 %>%
  rename(
    f_2 = fat.2,
    f_4 = fat.4,
    f_6 = fat.6,
    f_8 = fat.8,
    f_10 = fat.10,
    p_2 = prot.2,
    p_4 = prot.4,
    p_6 = prot.6,
    p_8 = prot.8,
    p_10 = prot.10,
    total.fat = fat.eaten,
    total.protein = protein.eaten
  ) %>%
  pivot_longer(c(f_2, f_4, f_6, f_8, f_10,
                 p_2, p_4, p_6, p_8, p_10),
               names_to = c(".value", "day"),
               names_sep = "_"
  ) %>%
  group_by(beetle) %>%
  mutate(cump = cumsum(p),
         cumf = cumsum(f),
         pfratio = p/f)

names(c)
select(c, beetle, day, f, p, cumf, cump, total.fat, total.protein, total.eaten)

c$day <- as.numeric(c$day)
c$pfratio[!is.finite(c$pfratio)] <- 0 #把無限大跟不存在變成0
c$pfratio

#print(c, n = 20) #n = 顯示多少row

mean2f <- c %>%
  group_by(day, diet.pair, age) %>%
  #  group_by(day, diet.pair, sex, age) %>%
  summarise(cumf = cumsum(mean(cumf)), 
            cump = cumsum(mean(cump)))
mean2f

  c %>%
  ggplot() +
  labs(x = "cumulative protein eaten", y = "cumulative fat eaten")+
  geom_point(aes(x = cump, y = cumf, color = diet.pair, shape = diet.pair))+
  scale_shape(solid = FALSE)+
#  scale_colour_brewer(palette = "Set1") +
  #  geom_smooth(method = "lm", size = 1, se = F)+
  #  facet_grid(sex~age, margins = T)+
  facet_grid(.~age)+
  #facet_wrap(~age+sex)+
  geom_line(data = mean2f, aes(x = cump, y = cumf, group = diet.pair), size = 1)+
  geom_point(data = mean2f, aes(x = cump, y = cumf, color = diet.pair), size = 2)

############### 0 & 21 graph respectively
mean2f0 <- filter(mean2f, age == 21)
c %>%
  filter(age == 21) %>%
  ggplot() +
  geom_point(aes(x = cump, y = cumf, color = diet.pair, shape = diet.pair))+
  scale_shape(solid = FALSE)+
  scale_colour_brewer(palette = "Set1") +
  #  geom_smooth(method = "lm", size = 1, se = F)+
  #  facet_grid(sex~age, margins = T)+
  #  facet_grid(.~age)+
  geom_line(data = mean2f0, aes(x = cump, y = cumf, group = diet.pair), size = 1)+
  geom_point(data = mean2f0, aes(x = cump, y = cumf, color = diet.pair), size = 2)
############## 0 & 21 graph respectively end


############################### Repeat measure ANOVA ###########################
exp2_1.3 <- select(c, beetle, age, diet.pair, sex, day, total.eaten, p, f) %>%
  mutate(fday = as.factor(day))
exp2_1.3

#### unbalaned data要怎麼check？
#??beetlegroup <- groupedData(p ~ age:fday|beetle, exp2_1.3)
#??isBalanced(beetlegroup)

###################### repeat measure anova fat eaten#######
library(afex)
fateaten.aov_car <- aov_car(f ~ age*sex*diet.pair + Error(beetle/fday), exp2_1.3)
anova(fateaten.aov_car)
summary(fateaten.aov_car)

#post-hoc
library(emmeans)
pairs(lsmeans(fateaten.aov_car, spec = "age"))
pairs(lsmeans(fateaten.aov_car, spec = "fday"))
lsmeans(fateaten.aov_car, pairwise ~ age:fday, adjust = "fdr")
lsmeans(fateaten.aov_car, pairwise ~ diet.pair:fday, adjust = "fdr")
lsmeans(fateaten.aov_car, pairwise ~ age:diet.pair:fday, adjust = "fdr")

#figure
library(Rmisc)
fat.se.three <- summarySE(exp2_1.3, measurevar = "f", groupvars = c("day", "age", "diet.pair"))
repeatfatfiga <- fat.se.three %>%
  ggplot(aes(x = day, y = f, color = age))+
  geom_point()+
  geom_errorbar(aes(ymin = f - se, ymax = f + se), width = 0.3)+
  scale_x_continuous(breaks = seq(0, 10, by = 2))+
  labs(y = "fat eaten (g)")+
  #geom_smooth(method = "lm", se = F, formula = y ~ poly(x, 2))+facet_grid(.~diet.pair)
  geom_smooth(method = "loess", se = F)+facet_grid(.~diet.pair)
#geom_line()+facet_grid(.~diet.pair)
#geom_smooth(method = "lm", se = F, formula = y ~ poly(x, 4))

fat.se.dayage <- summarySE(exp2_1.3, measurevar = "f", groupvars = c("day", "age"))
repeatfatfigb <- fat.se.dayage %>%
  ggplot(aes(x = day, y = f, color = age))+
  geom_point()+
  geom_errorbar(aes(ymin = f - se, ymax = f + se), width = 0.3)+
  scale_x_continuous(breaks = seq(0, 10, by = 2))+
  labs(y = "fat eaten (g)")+
  #geom_smooth(method = "lm", se = F, formula = y ~ poly(x, 2))
  geom_smooth(method = "loess", se = F)
#geom_line()

fat.se.dietage <- summarySE(exp2_1.3, measurevar = "f", groupvars = c("day", "diet.pair"))
repeatfatfigc <- fat.se.dietage %>%
  ggplot(aes(x = day, y = f, color = diet.pair))+
  geom_point()+
  geom_errorbar(aes(ymin = f - se, ymax = f + se), width = 0.3)+
  scale_x_continuous(breaks = seq(0, 10, by = 2))+
  labs(y = "fat eaten (g)")+
  #geom_smooth(method = "lm", se = F, formula = y ~ poly(x, 2))
  geom_smooth(method = "loess", se = F)
#geom_line()

############## repeat measure anova protein eaten#######
proteaten.aov_car <- aov_car(p ~ age*sex*diet.pair + Error(beetle/fday), exp2_1.3)
anova(proteaten.aov_car)
summary(proteaten.aov_car)

#post-hoc
pairs(lsmeans(proteaten.aov_car, spec = "fday"))
lsmeans(proteaten.aov_car, pairwise ~ age:fday, adjust = "fdr")
lsmeans(proteaten.aov_car, pairwise ~ diet.pair:fday, adjust = "fdr")
lsmeans(proteaten.aov_car, pairwise ~ age:diet.pair:fday, adjust = "fdr")

#figure
prot.se.three <- summarySE(exp2_1.3, measurevar = "p", groupvars = c("day", "age", "diet.pair"))
repeatprotfigd <- prot.se.three %>%
  ggplot(aes(x = day, y = p, color = age))+
  geom_point()+
  geom_errorbar(aes(ymin = p - se, ymax = p + se), width = 0.3)+
  scale_x_continuous(breaks = seq(0, 10, by = 2))+
  labs(y = "protein eaten (g)")+
  #geom_smooth(method = "lm", se = F, formula = y ~ poly(x, 2))+facet_grid(.~diet.pair)
  geom_smooth(method = "loess", se = F)+facet_grid(.~diet.pair)
#geom_line()+facet_grid(.~diet.pair)
  #geom_smooth(method = "lm", se = F, formula = y ~ poly(x, 4))

prot.se.dayage <- summarySE(exp2_1.3, measurevar = "p", groupvars = c("day", "age"))
repeatprotfige <- prot.se.dayage %>%
  ggplot(aes(x = day, y = p, color = age))+
  geom_point()+
  geom_errorbar(aes(ymin = p - se, ymax = p + se), width = 0.3)+
  scale_x_continuous(breaks = seq(0, 10, by = 2))+
  labs(y = "protein eaten (g)")+
  #geom_smooth(method = "lm", se = F, formula = y ~ poly(x, 2))
  geom_smooth(method = "loess", se = F)#geom_line()

prot.se.dietage <- summarySE(exp2_1.3, measurevar = "p", groupvars = c("day", "diet.pair"))
repeatprotfigf <- prot.se.dietage %>%
  ggplot(aes(x = day, y = p, color = diet.pair))+
  geom_point()+
  geom_errorbar(aes(ymin = p - se, ymax = p + se), width = 0.3)+
  scale_x_continuous(breaks = seq(0, 10, by = 2))+
  labs(y = "protein eaten (g)")+
  #geom_smooth(method = "lm", se = F, formula = y ~ poly(x, 2))
  geom_smooth(method = "loess", se = F)
#geom_line()
##隨著時間，protein吃的有差(day跟其他的interaction)，但sex跟diet跟age本身沒差

####combine plot####
library(ggpubr)
ggarrange(repeatfatfiga, repeatfatfigb, repeatfatfigc,
          repeatprotfigd, repeatprotfige, repeatprotfigf)
#          labels = c("A", "B", "C", "D", "E", "F"))
ggarrange(repeatfatfiga, repeatprotfigd, 
          #nrow = 2,
          common.legend = TRUE, legend = "top")

#######################repeat measure anova p/f ratio######
c
exp2_1.1 <- select(c, beetle, age, diet.pair, sex, day, p, f, pfratio) %>%
  mutate(fday = as.factor(day))
exp2_1.1

pfratio.aov_car <- aov_car(pfratio ~ age*sex*diet.pair + Error(beetle/fday), exp2_1.1)
anova(pfratio.aov_car)
summary(pfratio.aov_car)

#post-hoc
library(emmeans)
pairs(lsmeans(pfratio.aov_car, spec = "age"))
pairs(lsmeans(pfratio.aov_car, spec = "fday"))
lsmeans(pfratio.aov_car, pairwise ~ age:fday, adjust = "fdr")
lsmeans(pfratio.aov_car, pairwise ~ diet.pair:fday, adjust = "fdr")
lsmeans(pfratio.aov_car, pairwise ~ age:diet.pair:fday, adjust = "fdr")

#figure

library(Rmisc)
pfratio.se.three <- summarySE(exp2_1.1, measurevar = "pfratio", groupvars = c("day", "age", "diet.pair"))
pfratiofigg <- pfratio.se.three %>%
  ggplot(aes(x = day, y = pfratio, color = age))+
  geom_point()+
  geom_errorbar(aes(ymin = pfratio - se, ymax = pfratio + se), width = 0.3)+
  scale_x_continuous(breaks = seq(0, 10, by = 2))+
  labs(y = "p/f ratio")+
  geom_smooth(method = "loess", se = F)+
  facet_grid(.~diet.pair)

pfratio.se.three %>%
  ggplot(aes(x = day, y = pfratio, color = age))+
  geom_point()+
  geom_errorbar(aes(ymin = pfratio - se, ymax = pfratio + se), width = 0.3)+
  scale_x_continuous(breaks = seq(0, 10, by = 2))+
  labs(y = "p/f ratio")+
  geom_smooth(method = "loess", se = F)+
  facet_grid(.~diet.pair)#+
  geom_smooth(method = "lm", se = F, color = "black", aes(group = age), size = 0.5)

pfratio.se.dayage <- summarySE(exp2_1.1, measurevar = "pfratio", groupvars = c("day", "age"))
pfratio.se.dayage %>%
  ggplot(aes(x = day, y = pfratio, color = age))+
  geom_point()+
  geom_errorbar(aes(ymin = pfratio - se, ymax = pfratio + se), width = 0.3)+
  scale_x_continuous(breaks = seq(0, 10, by = 2))+
  labs(y = "p/f ratio")+
  geom_smooth(method = "loess", se = F)#+
  facet_grid(.~diet.pair)



####### repeat measure anova protein eaten/ total eaten######
exp2_1.2 <- select(c, beetle, age, diet.pair, sex, day, total.eaten, p, f) %>%
  mutate(fday = as.factor(day), 
         proteinperc = p/total.eaten)
exp2_1.2

protperc.aov_car <- aov_car(proteinperc ~ age*sex*diet.pair + Error(beetle/fday), exp2_1.2)
anova(protperc.aov_car)
summary(protperc.aov_car)

#post-hoc
lsmeans(protperc.aov_car, pairwise ~ age:fday, adjust = "fdr")
pairs(lsmeans(protperc.aov_car, spec = "age"))
pairs(lsmeans(protperc.aov_car, spec = "fday"))

#figure
protperc.se.dietage <- summarySE(exp2_1.2, measurevar = "proteinperc", groupvars = c("day", "age", "diet.pair"))
protpercfigh <- protperc.se.dietage %>%
  ggplot(aes(x = day, y =  proteinperc, color = age))+
  geom_point()+
  geom_errorbar(aes(ymin =  proteinperc - se, ymax =  proteinperc + se), width = 0.3)+
  scale_x_continuous(breaks = seq(0, 10, by = 2))+
  labs(y = "percentage protein")+
  facet_grid(.~diet.pair)+
  geom_smooth(method = "loess", se = F)+
  geom_smooth(method = "lm", se = F, color = "black", aes(group = age), size = 0.5, formula = y ~ poly(x, 2))
#  geom_smooth(method = "lm", se = F, formula = y ~ poly(x, 2))

library(ggpubr)
ggarrange(repeatfatfiga, repeatprotfigd, protpercfigh, pfratiofigg, 
          common.legend = T, 
          legend = "right")




