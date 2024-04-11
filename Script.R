library(readxl)
library(tidyverse)
library(ggpubr)
library(stargazer)
library(knitr)

##### Importing Data ##### 
personal <- read_excel("data/Personal.xlsx", sheet = "Personal")
metadata <- read_excel("data/Personal.xlsx", sheet = "Metadaten")

##### Organizing Data ####
personal$Position = as.factor(personal$Position) 
personal$Position = factor(personal$Position, levels = c("RL", "FB", "PK", "PA", "Head of Group 9.3", "Knowledge Manager", "Assistent"))


#levels(personal$Position)

##### Figs ####
#By Section
personal %>% group_by(Section) %>% ggplot(aes(Section, fill = Sex)) + geom_bar() + labs(y=NULL, caption = "Figure 1 - Persons per Sections") + scale_y_continuous(n.breaks = 10) 
personal %>% group_by(Section) %>% ggplot(aes(Section, fill = Sex)) + geom_bar(position = position_fill())

#By Position (Without 9.3)
fig_Section = personal %>% filter("9.3" != Section) %>% group_by(Position) %>% ggplot(aes(Position, fill = Sex)) +
  geom_bar() + facet_wrap(.~ Section) + labs(y = NULL, x = NULL)

personal %>% filter("9.3" != Section) %>% group_by(Position) %>% ggplot(aes(Position, fill = Sex)) + 
  geom_bar() + labs(y = NULL, x = NULL) + scale_x_discrete(labels = metadata$Long)



ggarrange(fig_gesamt, fig_Section)

#### Analyse ####
table(personal$Sex == "M")
round(personal %>% filter(Position == "RL") %>% summarise(mean(personal$Sex == "M")), digits = 3) 
mean(personal$Sex[personal$Position == "PK"] == "M")

#Chi^2 Test 
subset = personal %>% filter(Section != "9.3")
chisq.sex.position = chisq.test(subset$Sex, subset$Position)
rm(subset)
chisq.sex.position$observed
chisq.sex.position$expected
kable(chisq.sex.position$observed, col.names = c("Sex", metadata$Long), caption = "Fig.3 - Observed Values")
l = c("Female", "Male")
chisq.sex.position


