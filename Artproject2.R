library(tidyverse)
library(binom)
library(exactci)
library(RColorBrewer)
library(ggpubr)
library(PropCIs)
library(epitools)
library(msm)
library(sandwich)
library(MASS)

#Artproject2

artdata <- read_csv("C:\\Users\\Asus\\Desktop\\Artproject2.csv", col_names = TRUE)
head(artdata)
artdata$Gender <- factor(artdata$Gender, levels = c("M","F"))
artdata$Skill <- factor(artdata$Skill)
artdata1 <- artdata %>% mutate(score = rowSums(.[4:8], na.rm = T))
head(artdata1)
View(artdata1)
summary(artdata1)


artdata1 %>% group_by(Skill) %>% summarise(mean.age = mean(Age))
artdata1 %>% group_by(Skill) %>% summarise(mean.score = mean(score))
artdata1 %>% group_by(Gender) %>% summarise(mean.age = mean(Age))
artdata1 %>% group_by(Gender) %>% summarise(mean.score = mean(score))


table(artdata1$Skill, artdata1$Gender) #Trained men are small
table(artdata1$Skill, artdata1$Picture1) #Totally Amazing! 
table(artdata1$Skill, artdata1$Picture2)
table(artdata1$Skill, artdata1$Picture3)
table(artdata1$Skill, artdata1$Picture4)
table(artdata1$Skill, artdata1$Picture5)



prop.test(c(20,28), c(57,34), conf.level = .95, correct = F)
prop.test(c(55,12), c(58,34), conf.level = .95, correct = F)
prop.test(c(44,9), c(56,34), conf.level = .95, correct = F)
prop.test(c(45,21), c(58,34), conf.level = .95, correct = F) #No statistical Difference
prop.test(c(33,10), c(57,34), conf.level = .95, correct = F)
33/57 #for the last column (Correct answer percentage for Trained group)

riskscoreci(20,57,28,34, conf.level = .95) #(61% percent at least better than trained group, up to 3 times better)
riskscoreci(55,58,12,34, conf.level = .95)
riskscoreci(44,56,9,34, conf.level = .95)
riskscoreci(45,58,21,34, conf.level = .95) #No significance
riskscoreci(33,57,10,34, conf.level = .95)



table1_matrix <- matrix(c(37,20,6,28), byrow = T, ncol = 2)
chisq.test(table1_matrix)
table2_matrix <- matrix(c(3,55,22,12), byrow = T, ncol = 2)
chisq.test(table2_matrix)
table3_matrix <- matrix(c(12,44,25,9), byrow = T, ncol = 2)
chisq.test(table3_matrix)
table4_matrix <- matrix(c(13,45,13,21), byrow = T, ncol = 2) #No significance
chisq.test(table4_matrix)
table5_matrix <- matrix(c(24,33,24,10), byrow = T, ncol = 2)
chisq.test(table5_matrix)
#Except Picture 4 all Pictures are associated with Skill


artdata2 <- artdata1 %>% group_by(Skill) %>% summarise(mean = mean(score, na.rm = T))
artdata2
ggplot(artdata2, aes(x = Skill, y = mean)) + geom_col(fill = "white", color  = "black") + 
  ggtitle("Mean of Scores For Both Groups ") + ylab("Mean of Scores")

artdata1 %>% group_by(Skill, Gender) %>% summarise(mean = mean(score, na.rm = T)) #Untrained men did better than women
#Interesting part is that before Training, men do better but after training women do better
((3.45-2.06)/(2.06)) * 100 #67% increase of score for women
((2.8-2.61)/(2.61)) * 100 #7.2% increase of score for men (Although the trained men sample is small)
artdata1 %>% group_by(Skill) %>% summarise(mean = mean(score, na.rm = T))
(3.40/5) * 100 #percentage of correctness for trained in average
(2.35/5) * 100 #percentage of correctenss for untrained in average

artdata3 <- artdata1 %>% group_by(Skill, Gender) %>% summarise(mean = mean(score, na.rm = T))
artdata3
artdata3$Skill <- factor(artdata3$Skill, levels = c("Untrained", "Trained"))
ggplot(artdata3, aes(Skill, y = mean, color = Gender, group =Gender)) + geom_point() + 
  geom_line() + 
  scale_colour_manual(values = c("lightblue4", "red3")) + 
  ggtitle("Mean of Scores For Trained and Untrained for each Gender") + xlab("Skill") + ylab("Mean of Scores")


artdata1 %>% group_by(Skill, score) %>% summarise(counts = n())
ggplot(artdata1, aes(x = score, fill = Skill)) +
  geom_histogram(position = "dodge") + scale_fill_brewer(palette = "Set1", direction = -1) + xlab("Score") +
  ylab("Count") + ggtitle("Number of Subjects For Each Score")


ggplot(artdata1, aes(x = score)) + geom_histogram(aes(color = Skill), fill = "white", 
                                                  position = "identity") +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  xlab("Score") +
  ylab("Count") + ggtitle("Number of Subjects For Each Score")

ggplot(artdata1, aes(x = score)) + geom_density(aes(y = ..count.., color = Skill ), fill = "lightgray") +
  geom_vline(aes(xintercept = mean(score)), 
             linetype = "dashed", size = 0.6,
             color = "#868686FF") + 
  xlab("Score") +
  ylab("Density") + ggtitle("Density plot of Scores")


ggplot(artdata1, aes(x = score, y = Age, color = Skill)) + geom_boxplot() +
  xlab("Score") + ggtitle("Boxplot")


art3 <- artdata1 %>% group_by(score) %>% summarise(counts = n())
art3
art4 <- art3 %>%
  arrange(desc(score)) %>%
  mutate(prop = round(counts*100/sum(counts), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)

ggplot(art4, aes(x = "", y = prop, fill = as.factor(score))) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = lab.ypos, label = prop), color = "black", size = 4)+
  coord_polar("y", start = 0)+ 
  scale_fill_brewer(palette = "YlGnBu") +
  theme_void() + guides(fill=guide_legend(title="Scores"))
#14% answers all the questions correctly



artdata4 <- within(artdata1, Skill <- relevel(Skill, ref = 2))
summary(m1 <- glm(score ~ Skill + Age + Gender , family="poisson", data=artdata4))
summary(m2 <- glm.nb(score ~ Skill + Age + Gender, data=artdata4))

cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)
r.est


with(m1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE))) # So good model
m3 <- update(m1, .~. - Skill)
anova(m3, m1, test = "Chisq")


s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4)), 
                 coef(m1), cov.m1)
rexp.est <- exp(r.est[, -3])
rexp.est[, "Robust SE"] <- s
rexp.est


#rate ratio
#This is the estimated rate ratio comparing Trained to Untrained, given the other variables are held constant in the model. 
#Trained compared to Untrained, while holding the other variable constant in the model, 
#are expected to have a rate 1.69 times greater for score.

exp(1.8)




artdata11 <- artdata1 %>% dplyr::select(Picture1:Picture5)
head(artdata11)
alpha(artdata11)
