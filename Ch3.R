library(ggplot2)
library(devtools)
install.packages("ggrepel")
library(ggrepel)
library(tidyverse)
install.packages("Lahman")

#Let's get into evaluating the Lahman data now

github_pull("beanumber/baseball_R/tree/master/data")
Batting <- Lahman::Batting
halloffame <- Lahman::HallOfFame

setwd("C:\\Users\\james\\R_Working_Directory\\Analyzing_Baseball_Data_With_R")

Batting <- Batting %>%
  group_by(playerID) %>%
  mutate(From = min(yearID)) %>%
  mutate(To = max(yearID))

batting_summary <- Batting %>%
  group_by(playerID) %>%
  summarise(From = mean(From), To = mean(To))

hof_lahman <- halloffame %>%
  left_join(select(batting_summary, playerID, From, To), by = "playerID")

hof_lahman <- hof_lahman %>%
  filter(inducted == "Y" & category == "Player" & !is.na(From))


hof <- read.csv("hofbatting.csv")

#First, we define a player's mid-career as the average of the first and last seasons of baseball, we then use mutate() and cut() functions to create a new factor variable Era

hof <- hof %>%
  mutate(MidCareer = (From + To) / 2,
         Era = cut(MidCareer,
                   breaks = c(1800, 1900, 1919, 1941, 1960, 1976, 1993, 2050),
                   labels = c("19th Century", "Dead Ball", "Lively Ball", "Integration", "Expansion", "Free Agency", "Long Ball")))

hof_eras <- summarise(group_by(hof, Era), N=n())
hof_eras

ggplot(hof, aes(x=Era)) + 
  geom_bar() +
  xlab("Baseball Era") +
  ylab("Frequency") +
  ggtitle("Era of the Nonpitching Hall of Famers")
ggsave("chapter3_BarGraph.png")

ggplot(hof_eras, aes(Era, N)) +
  geom_point() +
  xlab("Baseball Era") +
  ylab("Frequency") +
  ggtitle("Era of the Nonpitching Hall of Famers") +
  coord_flip()

#Saving a pdf of the graphs
pdf("Chapter3_Multiple_Graphs.pdf")
ggplot(hof, aes(Era)) + geom_bar() 
ggplot(hof_eras, aes(Era, N)) + geom_point() + coord_flip()
dev.off()


#3.4 Numeric Variable: One-Dimensional Scatterplot and Histogram

ggplot(hof, aes(x=OPS, y=1)) + 
  geom_jitter(height = 0.6) + ylim(-1,3) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_fixed(ratio = 0.03)

lmOPS_hof = lm(OPS ~ SLG, data= hof)
summary(lmOPS_hof)

#let's try a qplot
library(ggrepel)
p <- qplot(HR, OPS, data = hof, color = Era, size = AB)
# Label speed outliers using geom_text_repel
r <- p + geom_text_repel(
  data = subset(hof, OPS > quantile(OPS, 0.75) + 1.5 * IQR(OPS) | OPS < quantile(OPS, 0.25) - 1.5 * IQR(OPS) | HR > quantile(HR, 0.75) + 1.5 * IQR(HR) | HR < quantile(HR, 0.25) - 1.5 * IQR(HR) ),
  aes(x = HR, y = OPS, label = Player),
  color = "#000000", 
  size = 4,
  nudge_x = 0.2, nudge_y = 0.4,
  segment.color = "red",
)

print(r)

p <- qplot(MidCareer, OBP, data = hof, color = Era, size = HR)
# Label speed outliers using geom_text_repel
r <- p + geom_text_repel(
  data = subset(hof, OBP > quantile(OBP, 0.75) + 1.5 * IQR(OBP) | OBP < quantile(OBP, 0.25) - 1.5 * IQR(OBP)),
  aes(x = MidCareer, y = OBP, label = Player),
  color = "#000000", 
  size = 4,
  nudge_x = 0.2, nudge_y = 0.4,
  segment.color = "red",
)

print(r)

p <- qplot(MidCareer, SLG, data = hof, color = Era, size = HR)
# Label speed outliers using geom_text_repel
r <- p + geom_text_repel(
  data = subset(hof, SLG > quantile(SLG, 0.75) + 1.5 * IQR(SLG) | SLG < quantile(SLG, 0.25) - 1.5 * IQR(SLG)),
  aes(x = MidCareer, y = SLG, label = Player),
  color = "#000000", 
  size = 4,
  nudge_x = 0.2, nudge_y = 0.4,
  segment.color = "red",
)

print(r)

p <- qplot(MidCareer, OPS, data = hof, color = Era, size = HR)
# Label speed outliers using geom_text_repel
r <- p + geom_text_repel(
  data = subset(hof, OPS > quantile(OPS, 0.75) + 1.5 * IQR(OPS) | OPS < quantile(OPS, 0.25) - 1.5 * IQR(OPS)),
  aes(x = MidCareer, y = OPS, label = Player),
  color = "#000000", 
  size = 4,
  nudge_x = 0.2, nudge_y = 0.4,
  segment.color = "red",
)

# Add a horizontal line at the y-value corresponding to the OPS outlier threshold
r <- r + geom_hline(yintercept = quantile(hof$OPS, 0.75) + 1.5 * IQR(hof$OPS), color = "red", linetype = "dotted")
# Add a horizontal line at the y-value corresponding to the OPS outlier threshold
r <- r + geom_hline(yintercept = quantile(hof$OPS, 0.25) - 1.5 * IQR(hof$OPS), color = "blue", linetype = "dotted")

print(r)

p <- qplot(MidCareer, HR, data = hof, color = Era, size = OBP)
# Label speed outliers using geom_text_repel
r <- p + geom_text_repel(
  data = subset(hof, HR > quantile(HR, 0.75) + 1.5 * IQR(HR) | HR< quantile(HR, 0.25) - 1.5 * IQR(HR)),
  aes(x = MidCareer, y = HR, label = Player),
  color = "#000000", 
  size = 4,
  nudge_x = 0.2, nudge_y = 0.4,
  segment.color = "red",
)

# Add a horizontal line at the y-value corresponding to the OPS outlier threshold
r <- r + geom_hline(yintercept = quantile(hof$HR, 0.75) + 1.5 * IQR(hof$HR), color = "red", linetype = "dotted")
# Add a horizontal line at the y-value corresponding to the OPS outlier threshold
r <- r + geom_hline(yintercept = quantile(hof$HR, 0.25) - 1.5 * IQR(hof$HR), color = "blue", linetype = "dotted")

print(r)


#Now let's make a histogram

ggplot(hof, aes(x=OPS)) +
  geom_histogram()

#Let's  specify bin size 


ggplot(hof, aes(x=OPS)) +
  geom_histogram(breaks = seq(0.4, 1.2, by = 0.1),
                 color = "blue", fill = "white")


#Chapter 3.5 Two Numeric Values, Scatter Plot with Smoothing

ggplot(hof, aes(MidCareer, OPS)) + 
  geom_point() +
  geom_smooth()


(p <- ggplot(hof, aes(OBP,SLG)) + geom_point())
lm_fit <- lm(SLG ~OBP, data = hof)

(p <- p + 
    xlim(0.25, 0.50) + ylim(0.28, 0.75) +
    xlab("On Base Percentage") + 
    ylab("Slugging Percentage"))

#Alternatively we could change the limits and the labels by appealing directly to the scale_x_continuous() and scale_y_continuous() functions

p <- p +
  scale_x_continuous("On Base Percentage", 
                     limits = c(0.25, 0.50)) +
  scale_y_continuous("Slugging  Percentage", 
                     limits = c(0.28, 0.75))
p

q = p + geom_abline(intercept = coef(lm_fit)[1], slope = coef(lm_fit)[2], color="red")
q


#Suppose we wanted to draw the function y=0.7-x on the graph:

(p <- p + geom_abline(slope = -1,
                      intercept = seq(0.7, 1, by=0.1),
                      color="black",
                      linetype = "dotted"))

#In our final iteration, we want to add labels to the lines showing the constant values of OPS
#First Let's create a dataframe which contains the coordiates
ops_labels <- tibble(
  OBP = rep(0.3, 4),
  SLG = seq(0.4, 0.7, by = 0.1),
  label = paste("OPS = ", OBP + SLG)
)

p + geom_text(data = ops_labels, hjust = "right", 
              aes(label = label))
