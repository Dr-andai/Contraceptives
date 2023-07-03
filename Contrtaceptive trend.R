#Analysis on Trends of Contraceptive

 
#Set working directory
setwd('C:/Users/Admin/Documents/PROJECTS/CONTRACEPTION/Contraceptives_R')

#Load Libraries
library(readxl)
library(tidyverse)
library(janitor)
library(ggplot2)
library(ggthemes)
library(hrbrthemes)
library(dplyr)

trends <- read_excel("C:/Users/Admin/Documents/PROJECTS/CONTRACEPTION/Contraception.xlsx", sheet=2)
head(trends)
# remove sterilization vasectomy 
trends_long <- trends[-7,]

trends_long <- trends_long%>%
  pivot_longer(
    cols = "2018":"2022",
    names_to = "year",
    values_to = "clients"
  )
head(trends_long)
names(trends_long)


trends_long$year <- as.numeric(str_match(trends_long$year,"[0-9]+"))


myColours2 = c("#040c04","#FF0000","#5cac94",
               "#24a4d4", "#ca5cdd","#4d3ec0")
pd <- position_dodge(0.2)

plot2 <- trends_long%>%
  rename("Method"="Contraceptive Method") %>%
  ggplot(aes(x=year, y=clients, color = Method))+
  labs (title = "New Contraceptive Users by Method",
        y = "Contraceptive users",
        x = "",
        color = "Method")+
  scale_y_continuous(trans = 'log2')+
  geom_line(linewidth = 1.5, position = pd)+
  geom_point(size = 3, shape=21, fill="grey",position = pd)+
  theme_few()+
  scale_color_manual(values = myColours2)
 
plot2



# SECTION TWO
trends_re <- read_excel("C:/Users/Admin/Documents/PROJECTS/CONTRACEPTION/Contraception.xlsx", sheet=3)

trendsre_long <- trends_re%>%
  pivot_longer(
    cols = "2018":"2022",
    names_to = "year",
    values_to = "clients"
  )

names(trendsre_long)
trendsre_long$year <- as.numeric(str_match(trendsre_long$year,"[0-9]+"))

plot3 <- trendsre_long%>%
  rename("Method"="Contraceptive Method") %>%
  ggplot(aes(x=year, y=clients, color = Method))+
  labs (title = "Re-Uptake of Contraceptive Methods by Users",
        y = "Contraceptive Users",
        x = "",
        color = "Method")+
  scale_y_continuous(trans = 'log2')+
  geom_line(linewidth = 1.5, position = pd)+
  geom_point(size = 3, shape=21, fill="grey",position = pd)+
  theme_few()+
  scale_color_manual(values = myColours2)

plot3
warnings()


hrbrthemes::import_roboto_condensed()
















