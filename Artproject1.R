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



#Art project1
artdata <- read_csv("C:\\Users\\Asus\\Desktop\\Art Project.csv", col_names = TRUE)
head(artdata)
artdata$Gender <- factor(artdata$Gender, levels = c("M","F"))
artdata$Skill <- factor(artdata$Skill)
artdata1 <- artdata %>% mutate(score = rowSums(.[4:8], na.rm = T))
head(artdata1)
summary(artdata1)




artdata1 %>% group_by(Skill) %>% summarise(mean.age = mean(Age, na.rm = T))
artdata1 %>% group_by(Skill) %>% summarise(mean.score = mean(score))
artdata1 %>% group_by(Gender) %>% summarise(mean.age = mean(Age, na.rm =T))
artdata1 %>% group_by(Gender) %>% summarise(mean.score = mean(score))


table(artdata1$Skill, artdata1$Gender)
table(artdata1$Skill, artdata1$Picture1)
table(artdata1$Skill, artdata1$Picture2)
table(artdata1$Skill, artdata1$Picture3)
table(artdata1$Skill, artdata1$Picture4)
table(artdata1$Skill, artdata1$Picture5)

prop.test(c(28,7), c(30,30), conf.level = .95, correct = F)
prop.test(c(30,7), c(30,29), conf.level = .95, correct = F)
prop.test(c(28,10), c(30,28), conf.level = .95, correct = F)
prop.test(c(27,10), c(30,30), conf.level = .95, correct = F)
prop.test(c(20,8), c(27,30), conf.level = .95, correct = F)
20/27 #for the last column

riskscoreci(28,30,7,30, conf.level = .95)
riskscoreci(30,30,7,29, conf.level = .95)
riskscoreci(28,30,10,28, conf.level = .95)
riskscoreci(27,30,10,30, conf.level = .95)
riskscoreci(20,27,8,30, conf.level = .95)
#Probability of choosing correct answer of Picture5 for the 
#ones who takes the class is between 1.5 and 5.38 times the
#probability who didn't take the class (at least 50% better)

table1_matrix <- matrix(c(2,28,23,7), byrow = T, ncol = 2)
chisq.test(table1_matrix)
table2_matrix <- matrix(c(0,30,22,7), byrow = T, ncol = 2)
chisq.test(table2_matrix)
table3_matrix <- matrix(c(2,28,18,10), byrow = T, ncol = 2)
chisq.test(table3_matrix)
table4_matrix <- matrix(c(3,27,20,10), byrow = T, ncol = 2)
chisq.test(table4_matrix)
table5_matrix <- matrix(c(7,20,22,8), byrow = T, ncol = 2)
chisq.test(table5_matrix)
#Each Picture is associated with the Skill


artdata2 <- artdata1 %>% group_by(Skill) %>% summarise(mean = mean(score, na.rm = T))
artdata2
ggplot(artdata2, aes(x = Skill, y = mean)) + geom_col()
artdata1 %>% group_by(Skill, Gender) %>% summarise(mean = mean(score, na.rm = T))
#Because there are just 2 Male trained
artdata1 %>% filter(Gender == "M", Skill == "Trained")
4.43/5 #percentage of correctness for trained
1.4/5 #percentage of correctenss for untrained


ggplot(artdata2, aes(x = Skill, y = mean)) + geom_col(fill = "white", color = "red")

artdata1 %>% group_by(Skill, score) %>% summarise(counts = n())
ggplot(artdata1, aes(x = score, fill = Skill)) +
  geom_histogram(position = "dodge") + scale_fill_brewer(palette = "Set1", direction = -1)

ggplot(artdata1, aes(x = score)) + geom_density(aes(y = ..count.., color = Skill ), fill = "lightgray") +
  geom_vline(aes(xintercept = mean(score)), 
             linetype = "dashed", size = 0.6,
             color = "#868686FF")

ggplot(artdata1, aes(x = score)) + geom_histogram(aes(color = Skill), fill = "white", 
                                                  position = "identity") +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))


artdata1 %>% group_by(Skill) %>% summarise(mean(Age, na.rm = T))
ggplot(artdata1, aes(x = score, y = Age, color = Skill)) + geom_boxplot()



art3 <- artdata1 %>% group_by(score) %>% summarise(counts = n())
art3
art4 <- art3 %>%
arrange(desc(score)) %>%
mutate(prop = round(counts*100/sum(counts), 1),
lab.ypos = cumsum(prop) - 0.5*prop)

ggplot(art4, aes(x = "", y = prop, fill = score)) +
geom_bar(width = 1, stat = "identity", color = "white") +
geom_text(aes(y = lab.ypos, label = prop), color = "white")+
coord_polar("y", start = 0)+ 
theme_void()
#31% of all participants did their best, also, 10% did the worst

#not really good because of small sample of Gender
ggplot(data=subset(artdata1, !is.na(Gender)), aes(factor(score))) + geom_bar(aes(fill = Gender), position = "dodge")
subset(artdata1, !is.na(Gender)) %>% group_by(Gender) %>% summarise(mean(score, na.rm = T))



artdata3 <- within(artdata1, Skill <- relevel(Skill, ref = 2))
summary(m1 <- glm(score ~ Skill + Age + Gender, family="poisson", data=artdata3))
summary(m2 <- glm.nb(score ~ Skill + Age + Gender, data=artdata1))

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
#rate of score is 3 times more in Trained group in comparison to Untrained group

head(artdata10)
artdata10 <- artdata1 %>% dplyr::select(Picture1:Picture5)
alpha(artdata10)
