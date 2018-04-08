getwd()
setwd("G:/Data Wrangling")

library(ggplot2)
titanic <- readRDS("titanic.rds")

str(titanic)

# First Bar plot 
ggplot(titanic, aes(x = Pclass, fill = Sex)) +
  geom_bar(position = "dodge")

# Adding Facet Grid Layer to visualise between Survivors and Casualties
ggplot(titanic, aes(x = Pclass, fill = Sex)) +
  geom_bar(position = "dodge") +
  facet_grid(. ~ Survived)

# Creating a customer Jitter
posn.jd <- position_jitterdodge(0.5, 0 , 0.6)

# Changed to points instead of Bars
ggplot(titanic, aes(x = Pclass, y = Age, color = Sex)) +
  geom_point(size = 3, alpha = .5, position = posn.jd) +
  facet_grid(. ~ Survived)
