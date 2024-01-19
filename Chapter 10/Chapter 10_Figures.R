# Figure 1
ddat = read.delim("dorami_part.txt")

library(ggplot2)

ggplot(data = ddat, 
       mapping = aes(x = Freq, y = RT, group = Education, color = Education)) +
  geom_line(stat = "summary", fun = mean, linewidth = 1) +
  scale_color_manual(values = c("black", "grey")) +
  labs(title = "Frequency x Education", x = "Frequency", 
       y = "Reaction Time (ms)") +
  theme_bw()

ggsave(filename = "ch10.figure1.tiff", height = 1350, width = 1800, units = "px",
       compression = c("lzw"), dpi = 300)

# Figure 2
ggplot(ddat, aes(y=RT, x=Education, fill=Freq, group=Freq)) +
  geom_bar(stat = "summary", fun = mean, position = "dodge") +
  scale_fill_manual(values=c("black", "darkgrey")) +
  labs(title = "Frequency x Education", x = "Frequency", 
       y = "Reaction Time (ms)") +
  theme_bw()

ggsave(filename = "ch10.figure2.tiff", height = 1350, width = 1800, units = "px",
       compression = c("lzw"), dpi = 300)

# Figure 3
ggplot(data = ddat, 
       mapping = aes(x = SynCat, y = RT, group = Education, color = Education)) +
  geom_line(stat = "summary", fun = mean, linewidth = 1) +
  scale_color_manual(values = c("black", "grey")) +
  coord_cartesian(ylim = c(650, 825)) +
  labs(title = "Syntactic Category x Education", x = "Syntactic Category", 
       y = "Reaction Time (ms)") +
  theme_bw()

ggsave(filename = "ch10.figure3.tiff", height = 1350, width = 1800, units = "px",
       compression = c("lzw"), dpi = 300)

# Figure 4
library(lsmeans)
ddat.all = read.delim("doramiR.txt")
ddat.clean = na.omit(ddat.all) 
ddat.clean$Participant = as.factor(ddat.clean$Participant)
ddat.part.aov = aov(RT ~ Education * SynCat * Freq +
                      +	Error(Participant / (SynCat * Freq)), data = ddat.clean)
ddat.lsmeans = as.data.frame(
  lsmeans(ddat.part.aov, c("Education", "SynCat", "Freq"))
)
library(ggplot2)
ggplot(data = ddat.lsmeans, 
       mapping = aes(x = Education, y = lsmean, group = Freq, 
                     color = Freq)) +
  facet_grid(. ~ SynCat) +
  geom_line(stat = "identity", linewidth = 1) + geom_point(stat = "identity") +
  geom_errorbar(mapping = aes(ymin = lower.CL, ymax = upper.CL), 
                width = .1, linewidth = 1, linetype = 1) +
  scale_color_manual(values = c("black", "darkgrey")) +
  coord_cartesian(ylim = c(550, 900)) +
  labs(x = "Education", y = "least square means",
       title = "Education x Frequency x SynCat", color="Frequency") +
  theme_bw()

ggsave(filename = "ch10.figure4.tiff", height = 1350, width = 1800, units = "px",
       compression = c("lzw"), dpi = 300)