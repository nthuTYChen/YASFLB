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