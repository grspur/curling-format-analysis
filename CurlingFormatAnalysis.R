#Brier and Scotties data analysis of 2018 16-team format vs. 2015-17 pre-qualifying format

library(tidyverse)
tidyverse_update()
library(lubridate)
library(stringr)
library(janitor)
devtools::install_github("sfirke/janitor") # advanced github version with 2&3-level tabyls

quartzFonts(ANXTC = c("Avenir Next Condensed Regular", "Avenir Next Condensed Demi Bold", "Avenir Next Condensed Italic", "Avenir Next Condensed Demi Bold Italic"))

Brier <- read_csv("Data files/BrierScoringData.txt")
Scotties <- read_csv("Data files/ScottiesData.txt")

BrierRR <- Brier %>% filter(Stage == "Round robin")
ScottiesRR <- Scotties %>% filter(Stage == "Round robin")

#Change these for each different plot sent to ggdraw()
myTitle <- "Settling the score"
mySubtitle <- "The number of ends in Scotties round-robin games by points scored\n"
myXLabel <- "End score (0 indicates blanked end)"
myYLabel <- "Count"
myCaption <- "Data source: Curling Canada, Wikipedia  |  Postmedia Sports"

#Brier margin of victory chart
BrierRR %>% filter(Margin >= 0) %>% 
  ggplot() +
  geom_histogram(aes(Margin), bins = 12, colour = "white", fill = "steelblue3") +
  facet_wrap(~ Year) +
  scale_x_continuous(breaks = c(1:12), minor_breaks = NULL) +
  scale_y_continuous(breaks = c(0,4,8,12,16,20)) +
  theme_light() +
  theme(
    text = element_text(family = "ANXTC"), 
    plot.title = element_text(size = 16, face = "bold"), 
    plot.subtitle = element_text(size = 13, face = "plain"), 
    plot.caption = element_text(size = 12, face = "plain"), 
    axis.title.x = element_text(size = 13, face = "plain", margin = margin(t = 10, b = 10)), 
    axis.title.y = element_text(size = 13, face = "plain", margin = margin(r = 10)), 
    axis.text.x = element_text(size = 12, face = "plain"), 
    axis.text.y = element_text(size = 12, face = "plain"), 
    strip.background = element_rect(fill = "red2"),
    strip.text = element_text(size = 12, face = "bold")
    ) +
  labs(title = myTitle, subtitle = mySubtitle, x = myXLabel, y = myYLabel, caption = myCaption) +
  annotate("rect", xmin = 2, xmax = 11, ymin = 20.5, ymax = 23, fill = "lavenderblush", colour = "grey90") +
  annotate("text", x = 6.5, y = 21.75, label = c("Games decided by 1-3 points: 69.7%", "Games decided by 1-3 points: 63.6%", "Games decided by 1-3 points: 65.2%", "Games decided by 1-3 points: 41.1%"), family = "ANXTC", size = 3.675)

#Brier number of ends played
BrierEnds <- BrierRR %>% tabyl(Year, Ends)
BrierEndsRR <- BrierEnds %>% 
  mutate(`7` = `7` / 2) %>% 
  mutate(`8` = `8` / 2) %>% 
  mutate(`9` = `9` / 2) %>% 
  mutate(`10` = `10` / 2) %>% 
  mutate(`11` = `11` / 2)
BrierEndsRR <- gather(BrierEndsRR, key = Ends.played, value = Count, -Year)
  
  ggplot(BrierEndsRR) +
  geom_col(aes(as.numeric(Ends.played), Count), colour = "white", fill = "steelblue3") +
  facet_wrap(~ Year) +
  scale_x_continuous(minor_breaks = NULL) +
  #scale_y_continuous(breaks = c(0,4,8,12,16,20))
  theme_light() +
  theme(
    text = element_text(family = "ANXTC"), 
    plot.title = element_text(size = 16, face = "bold"), 
    plot.subtitle = element_text(size = 13, face = "plain"), 
    plot.caption = element_text(size = 12, face = "plain"), 
    axis.title.x = element_text(size = 13, face = "plain", margin = margin(t = 10, b = 10)), 
    axis.title.y = element_text(size = 13, face = "plain", margin = margin(r = 10)), 
    axis.text.x = element_text(size = 12, face = "plain"), 
    axis.text.y = element_text(size = 12, face = "plain"), 
    strip.background = element_rect(fill = "red2"),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  labs(title = myTitle, subtitle = mySubtitle, x = myXLabel, y = myYLabel, caption = myCaption) +
  annotate("rect", xmin = 7, xmax = 11, ymin = 30.5, ymax = 33.5, fill = "lavenderblush", colour = "grey90") +
  annotate("text", x = 9, y = 32, label = c("Games with less than 10 ends: 50.0%", "Games with less than 10 ends: 53.0%", "Games with less than 10 ends: 54.5%", "Games with less than 10 ends: 75.0%"), family = "ANXTC", size = 3.75)

#Brier score by ends
BrierEndsScoreSum <- read_csv("Data files/BrierEndsScore.txt")
  
ggplot(BrierEndsScoreSum) +
  geom_col(aes(End.score, Count), colour = "white", fill = "steelblue3") +
  facet_wrap(~ Year) +
  scale_x_continuous(breaks = c(0:5), minor_breaks = NULL) +
  #scale_y_continuous(breaks = c(0,4,8,12,16,20))
  theme_light() +
  theme(
    text = element_text(family = "ANXTC"), 
    plot.title = element_text(size = 16, face = "bold"), 
    plot.subtitle = element_text(size = 13, face = "plain"), 
    plot.caption = element_text(size = 12, face = "plain"), 
    axis.title.x = element_text(size = 13, face = "plain", margin = margin(t = 10, b = 10)), 
    axis.title.y = element_text(size = 13, face = "plain", margin = margin(r = 10)), 
    axis.text.x = element_text(size = 12, face = "plain"), 
    axis.text.y = element_text(size = 12, face = "plain"), 
    strip.background = element_rect(fill = "red2"),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  labs(title = myTitle, subtitle = mySubtitle, x = myXLabel, y = myYLabel, caption = myCaption) +
  annotate("rect", xmin = 0.5, xmax = 4.5, ymin = 335, ymax = 370, fill = "lavenderblush", colour = "grey90") +
  annotate("text", x = 2.5, y = 352.5, label = c("Ends with 3 or more points: 6.5%", "Ends with 3 or more points: 7.1%", "Ends with 3 or more points: 6.9%", "Ends with 3 or more points: 11.2%"), family = "ANXTC", size = 3.75)

###SCOTTIES###

#Scotties margin of victory chart
ScottiesRR %>% filter(Margin >= 0) %>% 
  ggplot() +
  geom_histogram(aes(Margin), bins = 13, colour = "white", fill = "red2") +
  facet_wrap(~ Year) +
  scale_x_continuous(breaks = c(1:13), minor_breaks = NULL) +
  scale_y_continuous(breaks = c(0,4,8,12,16,20)) +
  theme_light() +
  theme(
    text = element_text(family = "ANXTC"), 
    plot.title = element_text(size = 16, face = "bold"), 
    plot.subtitle = element_text(size = 13, face = "plain"), 
    plot.caption = element_text(size = 12, face = "plain"), 
    axis.title.x = element_text(size = 13, face = "plain", margin = margin(t = 10, b = 10)), 
    axis.title.y = element_text(size = 13, face = "plain", margin = margin(r = 10)), 
    axis.text.x = element_text(size = 12, face = "plain"), 
    axis.text.y = element_text(size = 12, face = "plain"), 
    strip.background = element_rect(fill = "steelblue3"),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  labs(title = myTitle, subtitle = mySubtitle, x = myXLabel, y = myYLabel, caption = myCaption) +
  annotate("rect", xmin = 2, xmax = 12, ymin = 22.5, ymax = 25, fill = "lightsteelblue1", colour = "grey90") +
  annotate("text", x = 7, y = 23.75, label = c("Games decided by 1-3 points: 63.6%", "Games decided by 1-3 points: 68.2%", "Games decided by 1-3 points: 54.5%", "Games decided by 1-3 points: 51.8%"), family = "ANXTC", size = 3.675)

#Scotties number of ends played
ScottiesEnds <- ScottiesRR %>% tabyl(Year, Ends)
ScottiesEndsRR <- ScottiesEnds %>% 
  mutate(`6` = `6` / 2) %>% 
  mutate(`7` = `7` / 2) %>% 
  mutate(`8` = `8` / 2) %>% 
  mutate(`9` = `9` / 2) %>% 
  mutate(`10` = `10` / 2) %>% 
  mutate(`11` = `11` / 2)
ScottiesEndsRR <- gather(ScottiesEndsRR, key = Ends.played, value = Count, -Year)

ggplot(ScottiesEndsRR) +
  geom_col(aes(as.numeric(Ends.played), Count), colour = "white", fill = "red2") +
  facet_wrap(~ Year) +
  scale_x_continuous(breaks = c(6:11), minor_breaks = NULL) +
  #scale_y_continuous(breaks = c(0,4,8,12,16,20))
  theme_light() +
  theme(
    text = element_text(family = "ANXTC"), 
    plot.title = element_text(size = 16, face = "bold"), 
    plot.subtitle = element_text(size = 13, face = "plain"), 
    plot.caption = element_text(size = 12, face = "plain"), 
    axis.title.x = element_text(size = 13, face = "plain", margin = margin(t = 10, b = 10)), 
    axis.title.y = element_text(size = 13, face = "plain", margin = margin(r = 10)), 
    axis.text.x = element_text(size = 12, face = "plain"), 
    axis.text.y = element_text(size = 12, face = "plain"), 
    strip.background = element_rect(fill = "steelblue3"),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  labs(title = myTitle, subtitle = mySubtitle, x = myXLabel, y = myYLabel, caption = myCaption) +
  annotate("rect", xmin = 6, xmax = 11, ymin = 31, ymax = 34, fill = "lightsteelblue1", colour = "grey90") +
  annotate("text", x = 8.5, y = 32.5, label = c("Games with less than 10 ends: 51.5%", "Games with less than 10 ends: 51.5%", "Games with less than 10 ends: 56.1%", "Games with less than 10 ends: 64.3%"), family = "ANXTC", size = 3.75)

#Scotties score by ends
ScottiesEndsScoreSum <- read_csv("Data files/ScottiesEndScore.txt")

ggplot(ScottiesEndsScoreSum) +
  geom_col(aes(End.score, Count), colour = "white", fill = "red2") +
  facet_wrap(~ Year) +
  scale_x_continuous(breaks = c(0:6), minor_breaks = NULL) +
  #scale_y_continuous(breaks = c(0,4,8,12,16,20))
  theme_light() +
  theme(
    text = element_text(family = "ANXTC"), 
    plot.title = element_text(size = 16, face = "bold"), 
    plot.subtitle = element_text(size = 13, face = "plain"), 
    plot.caption = element_text(size = 12, face = "plain"), 
    axis.title.x = element_text(size = 13, face = "plain", margin = margin(t = 10, b = 10)), 
    axis.title.y = element_text(size = 13, face = "plain", margin = margin(r = 10)), 
    axis.text.x = element_text(size = 12, face = "plain"), 
    axis.text.y = element_text(size = 12, face = "plain"), 
    strip.background = element_rect(fill = "steelblue3"),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  labs(title = myTitle, subtitle = mySubtitle, x = myXLabel, y = myYLabel, caption = myCaption) +
  annotate("rect", xmin = 0.5, xmax = 5.5, ymin = 350, ymax = 385, fill = "lightsteelblue1", colour = "grey90") +
  annotate("text", x = 3, y = 367.5, label = c("Ends with 3 or more points: 7.4%", "Ends with 3 or more points: 8.7%", "Ends with 3 or more points: 10.0%", "Ends with 3 or more points: 13.0%"), family = "ANXTC", size = 3.75)
