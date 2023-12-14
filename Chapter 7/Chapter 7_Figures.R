# Figure 1

png(filename = "ch7.figure1.png", width = 1200, height = 900, units = "px", res = 200)

plot(function(x) {df(x, df1=5, df2=5)}, xlim=c(0,3), ylim=c(0,1), ylab = "", xlab = "") 
plot(function(x) {df(x, df1=5, df2=10)}, xlim=c(0,3), add=T, lty=2) 
plot(function(x) {df(x, df1=10, df2=5)}, xlim=c(0,3), add=T, lwd=2) 
plot(function(x) {df(x, df1=10, df2=10)},
       xlim=c(0,3), add=T, lty=2, lwd=2) 
legend("topright", lty=c(1,2,1,2), lwd = c(1,1,2,2), 
       legend=c("df1 = 5, df2 = 5", "df1 = 5, df2 = 10", "df1 = 10, df2 = 5",
                    "df1 = 10, df2 = 10")) 

dev.off()

# Figure 2
library(ggplot2)

nv = read.table("NounsVerbs.txt", header = T) 
ggplot(subset(nv, Study == 3), aes(x = WordType, y = Measure)) +
  geom_point() +
  geom_line(aes(group = Participant)) +
  labs(title = "Study 3 from NounsVerbs", x = "Word Type", y = "Measure") +
  theme_bw()

ggsave(filename = "ch7.figure2.png", width = 1200, height = 900, units = "px",
       dpi = 200)

# Figure 3
library(ggplot2)

n = 30
means = c()
ses = c()
ci.upper = c()
ci.lower = c()
for(i in 1:50) {
  set.seed(i * 10)
  sample.new = rnorm(n)
  means[i] = mean(sample.new)
  ses[i] = sd(sample.new) / sqrt(n)
  ci.upper[i] = means[i] + 1.96 * ses[i]
  ci.lower[i] = means[i] - 1.96 * ses[i]
}

ci.test = data.frame(sample.mean = means, sample.se = se, 
                     ci.upper = ci.upper, ci.lower = ci.lower, x = 1:50)

means.avg = mean(means)

ci.test$includeMu = ifelse(ci.test$ci.lower > 0 | ci.test$ci.upper < 0, "No", "Yes")

ggplot(data = ci.test, mapping = aes(x = x, y = sample.mean, color = includeMu)) +
  geom_errorbar(ymin = ci.lower, ymax = ci.upper) +
  geom_hline(yintercept = 0, color = "red") +
  scale_x_continuous(labels = NULL, breaks = NULL) +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_color_manual(values = c("#252525", "#cccccc")) +
  coord_flip() + labs(title = "95% CI of 50 Samples from Normal Distribution",
                      subtitle = "H0: mu = 0, sd = 1", x = "", y = "95% CI", caption = "red line = mu") +
  guides(color = guide_legend(title = "Include mu")) +
  theme_bw()

ggsave(filename = "ch7.figure3.png", width = 1200, height = 900, units = "px",
       dpi = 200)