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

# Figure 4
png(filename = "ch7.figure4.png", width = 1200, height = 900, units = "px", res = 200)

error.bar = function(x, y, upper, lower = upper, length = 0.1,...) { 
  if(length(x) != length(y) | length(y) !=length(lower) | 
     length(lower) != length(upper)) 
    stop("vectors must be same length") 
  arrows(x, y + upper, x, y - lower, 
         angle = 90, code = 3, length = length, ...) 
} 
sister.plot = barplot(22, ylim = c(0,30), 
                      ylab = "Mean VOT (ms)", main = "Sister vs. Martians") 
conf95 = qt(0.05/2, df = 15) * (3/sqrt(16))
error.bar(sister.plot, 22, conf95) 
abline(h = 20, lty = 2)

dev.off()

# Figure 5
sister.mean = data.frame(Speaker = "My Sister", VOT = 22)
library(ggplot2) 
ggplot(data = sister.mean, mapping = aes(x = Speaker, y = VOT)) + 
  geom_bar(fill = "white", color = "black", stat = "identity") + 	
  geom_errorbar(mapping = aes(ymin = VOT-conf95, 
                              ymax = VOT+conf95), width = 0.2) +
  theme_bw()

ggsave(filename = "ch7.figure5.png", width = 1200, height = 900, units = "px",
       dpi = 200)

# Figure 6
png(filename = "ch7.figure6.png", width = 1200, height = 900, units = "px", res = 200)

bg = read.delim("BoysGirls.txt")
study1 = subset(bg, Study == 1)
t.res1 = t.test(Measure ~ Gender, data = study1, var.equal = T) 
conf95.upper = t.res1$conf.int[2]
conf95.lower = t.res1$conf.int[1]
conf95.2 = (conf95.upper - conf95.lower) / 2
boys1 = subset(study1, Gender == "Boy") 
girls1 = subset(study1, Gender == "Girl")
bg.plot = barplot(abs(mean(boys1$Measure) - mean(girls1$Measure)),
                  names.arg=c("Boys vs. Girls"), ylab = "Difference", 
                  ylim = c(0, 3.5)) 	
error.bar(bg.plot, abs(mean(boys1$Measure) - mean(girls1$Measure)), 
          conf95.2) 

dev.off()

# Figure 7
bg.df = data.frame(xLab = "Boys vs. Girls", 
                   Diff = abs(mean(boys1$Measure) - mean(girls1$Measure)),
                   CI = conf95.2)

ggplot(data = bg.df, mapping = aes(x = xLab, y = Diff)) +
  geom_bar(color = "black", fill = "white", stat = "identity") +
  geom_errorbar(mapping = aes(ymin = Diff - CI, ymax = Diff + CI),
                width = 0.2) +
  scale_y_continuous(limits = c(0, 3.5)) +
  labs(x = NULL, y = "Difference") +
  theme_bw()

ggsave(filename = "ch7.figure7.png", width = 1200, height = 900, units = "px",
       dpi = 200)

# Figure 8
png(filename = "ch7.figure8.png", width = 1200, height = 900, units = "px", res = 200)

nv = read.delim("NounsVerbs.txt")
study3 = subset(nv, Study == 3)
t.res3 = t.test(Measure ~ WordType, data = study3, paired = T) 	
conf95.upper = t.res3$conf.int[2]
conf95.lower = t.res3$conf.int[1]
conf95.3 = (conf95.upper - conf95.lower) / 2
nouns3 = subset(study3, WordType == "Noun")
verbs3 = subset(study3, WordType == "Verb")
Diff.mean = mean(abs(N - V))
nv.plot = barplot(Diff.mean, names.arg=c("Nouns vs. Verbs"), 
                  ylab = "Mean of Paired Difference ", ylim=c(0, 3.5))
error.bar(nv.plot, Diff.mean, conf95.3)

dev.off()