# Figure 1
exp1 = read.delim("ColoredRooms.txt", header = T)

exp.avg = aggregate(Learning ~ Color, FUN = mean, data = exp1)
exp.blue = subset(exp1, Color == "blue")
blue.se = sqrt(sum(((exp.blue$Learning - mean(exp.blue$Learning)) ^ 2)) / 
                 (sd(exp.blue$Learning) / nrow(exp.blue)))
exp.red = subset(exp1, Color == "red")
red.se = sqrt(sum(((exp.red$Learning - mean(exp.red$Learning)) ^ 2)) / 
                (sd(exp.red$Learning) / nrow(exp.red)))
exp.yellow = subset(exp1, Color == "yellow")
yellow.se = sqrt(sum(((exp.yellow$Learning - mean(exp.yellow$Learning)) ^ 2)) / 
                   (sd(exp.yellow$Learning) / nrow(exp.yellow)))

exp.avg$SE = c(blue.se, red.se, yellow.se)

library(ggplot2)

ggplot(data = exp.avg, mapping = aes(x = Color, y = Learning, group = Color, linetype = Color)) +
  geom_point() +
  geom_errorbar(mapping = aes(ymin = Learning - SE, ymax = Learning + SE), width = 0.2)  +
  scale_y_continuous(limits = c(-10, 10)) +
  labs(title = "Vocabulary Learning by Classroom Color", x = "Classroom Color", 
       y = "Learning Performance", caption = "Error bars = SE") +
  theme_bw()

ggsave(filename = "ch9.figure1.png", height = 900, width = 1200, units = "px",
       dpi = 200)

# Figure 2
library(ggplot2)

semphon.1 = data.frame(SemRel = c(rep("Irrelevant", 2), 
                                  rep("Relevant",2  )), 
                       PhonRel = rep(c("Irrelevant", "Relevant"),2), 
                       RT = c(445, 410, 385, 350),
                       InteractType = rep("Additive (No Interaction)", 4))

semphon.2 = data.frame(SemRel = c(rep("Irrelevant", 2), 
                                  rep("Relevant",2  )), 
                       PhonRel = rep(c("Irrelevant", "Relevant"),2), 
                       RT = c(445, 385, 365, 350),
                       InteractType = rep("Reduced/Enhanced", 4))

semphon.3 = data.frame(SemRel = c(rep("Irrelevant", 2), 
                                  rep("Relevant",2  )), 
                       PhonRel = rep(c("Irrelevant", "Relevant"),2), 
                       RT = c(445, 350, 360, 410),
                       InteractType = rep("Opposite", 4))

semphon = rbind(semphon.1, semphon.2, semphon.3)

ggplot(data = semphon, aes(x = SemRel, y = RT, 
                             group = PhonRel, linetype = PhonRel)) +
  geom_line()+
  geom_point()+
  scale_y_continuous(limits = c(325, 475)) +
  facet_grid(~ InteractType) +
  labs(title = "Interaction Types", x = "Semantic Level", 
       y = "Reaction Time (ms)", 
       linetype = "Phonological Level") + 
  theme_bw()

ggsave(filename = "ch9.figure2.png", height = 900, width = 1800, units = "px",
       dpi = 200)

# Figure 3
exp2 = read.delim("ColoredRooms2.txt", header = T)
colorgender.aov = aov(Learning ~ Gender * Color, data = exp2) 

png(filename = "ch9.figure3.png", height = 900, width = 1200, units = "px",
    res = 200)

library(effects)			
colorgender.ef = allEffects(colorgender.aov)	
plot(colorgender.ef) 

dev.off()