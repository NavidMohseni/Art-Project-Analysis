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


artdata <- read_csv("C:\\Users\\Asus\\Desktop\\Art data 3 - Copy.csv", col_names = TRUE)
head(artdata)
artdata$Sex <- factor(artdata$Sex, levels = c("M","F"))
artdata$Ed <- factor(artdata$Ed)
glimpse(artdata)
artdata1 <- artdata %>% mutate(score = rowSums(.[4:7], na.rm = T))
head(artdata1)
View(artdata1)
summary(artdata1)


artdata1 %>% group_by(Ed) %>% summarise(mean.age = mean(Age))
artdata1 %>% group_by(Ed) %>% summarise(mean.score = mean(score))
artdata1 %>% group_by(Sex) %>% summarise(mean.age = mean(Age))
artdata1 %>% group_by(Sex) %>% summarise(mean.score = mean(score))


table(artdata1$Ed, artdata1$Sex) #Trained men are small
table(artdata1$Ed, artdata1$Pic1)  
table(artdata1$Ed, artdata1$Pic2)
table(artdata1$Ed, artdata1$Pic3)
table(artdata1$Ed, artdata1$Pic4)

prop.test(c(55,6), c(73,18), conf.level = .95, correct = F)
prop.test(c(51,4), c(73,19), conf.level = .95, correct = F)
prop.test(c(58,6), c(73,19), conf.level = .95, correct = F)
prop.test(c(61,7), c(73,19), conf.level = .95, correct = F) 
61/73 #Modern Answer Percentage for the Last Picture



riskscoreci(55,73,6,18, conf.level = .95) #(30% percent at least better(modern) than untrained group, up to 4 times better)
riskscoreci(51,73,4,19, conf.level = .95)
riskscoreci(58,73,6,19, conf.level = .95)
riskscoreci(61,73,7,19, conf.level = .95)



table1_matrix <- matrix(c(18,55,13,6), byrow = T, ncol = 2)
chisq.test(table1_matrix)
table2_matrix <- matrix(c(22,51,15,4), byrow = T, ncol = 2)
chisq.test(table2_matrix)
table3_matrix <- matrix(c(15,58,13,6), byrow = T, ncol = 2)
chisq.test(table3_matrix)
table4_matrix <- matrix(c(12,61,12,7), byrow = T, ncol = 2) 
chisq.test(table4_matrix)



artdata2 <- artdata1 %>% group_by(Ed) %>% summarise(mean = mean(score, na.rm = T))
artdata2
ggplot(artdata2, aes(x = Ed, y = mean)) + geom_col(fill = "white", color  = "black") + 
  ggtitle("Mean of Scores For Both Groups ") + ylab("Mean of Scores") + xlab("Ed")



artdata1 %>% group_by(Ed, Sex) %>% summarise(mean = mean(score, na.rm = T)) #Trained women like more modern pictures than men
#After Training both Sex are interested in modern art

artdata1 %>% group_by(Ed) %>% summarise(mean = mean(score, na.rm = T))
(3.04/4) * 100 #percentage of modern selection for trained in average
(1.21/4) * 100 #percentage of modern selection for untrained in average



artdata3 <- artdata1 %>% group_by(Ed, Sex) %>% summarise(mean = mean(score, na.rm = T))
artdata3
artdata3$Ed <- factor(artdata3$Ed, levels = c("Untrained", "Trained"))
ggplot(artdata3, aes(Ed, y = mean, color = Sex, group =Sex)) + geom_point() + 
  geom_line() + 
  scale_colour_manual(values = c("lightblue4", "red3")) + 
  ggtitle("Mean of Scores For Trained and Untrained for each Sex") + xlab("Ed") + ylab("Mean of Scores")



artdata1 %>% group_by(Ed, score) %>% summarise(counts = n())
ggplot(artdata1, aes(x = score, fill = Ed)) +
  geom_histogram(position = "dodge") + scale_fill_brewer(palette = "Set1", direction = -1) + xlab("Score") +
  ylab("Count") + ggtitle("Number of Subjects For Each Score")


ggplot(artdata1, aes(x = score)) + geom_histogram(aes(color = Ed), fill = "white", 
                                                  position = "identity") +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  xlab("Score") +
  ylab("Count") + ggtitle("Number of Subjects For Each Score")

ggplot(artdata1, aes(x = score)) + geom_density(aes(y = ..count.., color = Ed ), fill = "lightgray") +
  geom_vline(aes(xintercept = mean(score)), 
             linetype = "dashed", size = 0.6,
             color = "#868686FF") + 
  xlab("Score") +
  ylab("Density") + ggtitle("Density plot of Scores")


ggplot(artdata1, aes(x = score, y = Age, color = Ed)) + geom_boxplot() +
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
#16.3% Prefer traditional art totally



artdata4 <- within(artdata1, Ed <- relevel(Ed, ref = 2))
summary(m1 <- glm(score ~ Ed + Age + Sex , family="poisson", data=artdata4))
summary(m2 <- glm.nb(score ~ Ed + Age + Sex, data=artdata4))

cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)
r.est


with(m1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE))) # So good model
m3 <- update(m1, .~. - Ed)
anova(m3, m1, test = "Chisq")


s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4)), 
                 coef(m1), cov.m1)
rexp.est <- exp(r.est[, -3])
rexp.est[, "Robust SE"] <- s
rexp.est


#rate ratio
#Trained compared to Untrained, while holding the other variable constant in the model, 
#are expected to have a rate 1.69 times greater for score.



artdata3 <- gather(artdata, key = "Picture_Numb", value = "Art", Pic1, Pic2, Pic3, Pic4)
head(artdata3)
view(artdata3)
glimpse(artdata3)
artdata3$Picture_Numb <- as.factor(artdata3$Picture_Numb)
artdata3$Art <- as.factor(artdata3$Art)

glimpse(artdata3)
summary(artdata3)
table(artdata3$Ed, artdata3$Art)
table(artdata3$Ed, artdata3$Art, artdata3$Sex)
chisq.test(table(artdata3$Ed, artdata3$Art))
artdata3 %>% group_by(Art) %>% summarise(mean.age = mean(Age))
ggplot(artdata3, aes(x = Art, fill = Ed)) + geom_bar(position = "fill")
ggplot(artdata3, aes(x = Ed, fill = Art)) + geom_bar(position = "fill") + xlab("Skill") #Recommended
ggplot(artdata3, aes(x = Ed, fill = Art)) + geom_bar() + xlab("Skill")
ggplot(artdata3, aes(x = Ed, fill = Art)) + geom_bar() + xlab("Skill") + facet_grid(. ~ Sex)



