#Cabarrus (25), Catawba (35), Lincoln (109), Rowan (159), Iredell (97)
#, Stanly (167), Gaston (71), Mecklenburg (119), Union (179)
p_25 <- read.delim(file = "patches_37025.txt", header = FALSE, sep = "\t")
p_35 <- read.delim(file = "patches_37035.txt", header = FALSE, sep = "\t")
p_71 <- read.delim(file = "patches_37071.txt", header = FALSE, sep = "\t")
p_97 <- read.delim(file = "patches_37097.txt", header = FALSE, sep = "\t")
p_109 <- read.delim(file = "patches_37109.txt", header = FALSE, sep = "\t")
p_119 <- read.delim(file = "patches_37119.txt", header = FALSE, sep = "\t")
p_159 <- read.delim(file = "patches_37159.txt", header = FALSE, sep = "\t")
p_167 <- read.delim(file = "patches_37167.txt", header = FALSE, sep = "\t")
p_179 <- read.delim(file = "patches_37179.txt", header = FALSE, sep = "\t")

t.test(p_25, p_35, var.equal = FALSE)

#add a column to each data table to tell which county it is
library(dplyr)
p_25 <- mutate(p_25, county="Cabarrus")
p_35 <- mutate(p_35, county="Catawaba")
p_71 <- mutate(p_71, county="Gaston")
p_97 <- mutate(p_97, county="Iredell")
p_109 <- mutate(p_109, county="Lincoln")
p_119 <- mutate(p_119, county="Mecklenburg")
p_159 <- mutate(p_159, county="Rowan")
p_167 <- mutate(p_167, county="Stanly")
p_179 <- mutate(p_179, county="Union")

patch_new <- bind_cols(p_25, p_35, p_71)

#combine all of the data together
patch <- bind_rows(p_25, p_35, p_71, p_97, p_109, p_119, p_159, p_167, p_179)

#trying to normalize the data with log
patch$V1 <- sapply( patch$V1, log)
patch$old <- patch$V1

library(tidyr)

install.packages("gmodels", dependencies = TRUE)
install.packages("car", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
install.packages("qqplotr", dependencies = TRUE)
install.packages("dplyr", dependencies = TRUE)
install.packages("emmeans", dependencies = TRUE)
install.packages("magrittr")
install.packages("multcompView")
library(magrittr)

library(gmodels)
library(car)
library(ggplot2)
library(qqplotr)
library(dplyr)
library(emmeans)

#descriptive stats
patch$county <- as.factor(patch$county)
a_sum <- patch %>% select( V1, county) %>% group_by( county) %>% summarise( n = n(),
                                                                            mean = mean(V1, na.rm = TRUE),
                                                                            sd = sd(V1, na.rm = TRUE),
                                                                            stderr = sd/sqrt(n),
                                                                            LCL = mean - qt(1 - (0.05/2), n-1) * stderr,
                                                                            UCL = mean + qt(1 - (0.05/2), n-1) * stderr,
                                                                            median = median(V1, na.rm = TRUE),
                                                                            min = min(V1, na.rm = TRUE),
                                                                            max = max(V1, na.rm = TRUE),
                                                                            IQR = IQR(V1, na.rm = TRUE))
#Shapiro-Wilk test for normailty
#significantly small p-values
patch %>%
  group_by(county) %>%
  summarise(`W stat` = shapiro.test(V1)$statistic,
            `p-val` = shapiro.test(V1)$p.value)

#QQ plots
# all are skewed (positive?)
#all take similar shape
ggplot(data = patch, mapping = aes(sample = V1, color = county, fill = county)) +
  stat_qq_band(alpha = 0.5, conf=0.95, qtype = 1, bandType = "boot", B=5000) +
  stat_qq_line(identity = TRUE) +
  stat_qq_point(col="black") +
  facet_wrap(~ county, scales = "free") +
  labs(x = "Theoretical Quantiles", y = "sample Quantiles") + theme_bw()

#histogram
ggplot(patch, aes(x=V1, color = county, fill = county) )+ 
  geom_histogram(binwidth=1)

library(RColorBrewer)
ggplot(patch, aes(x=V1)) +
  ggtitle("Density Plot by County") +
  geom_density(data = patch, mapping = aes(color = county)) +
  scale_color_brewer(palette = "YlGnBu") + theme_grey()

display.brewer.all()
  
ggplot(patch, aes(x=old)) +
  geom_density(data = patch, mapping = aes(color = county)) +
  xlim(0, 250) + ylim(0, 0.0025)

# Change line colors by groups
c_25 <- patch[patch$county == "Cabarrus",]
ggplot(c_25, aes(x=V1, color=county)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, binwidth = 1, fill = "white")+
  geom_density(alpha=0.6)+
  labs(title="Patch histogram plot",x="V1", y = "Density")+
  theme_classic()

## density plots seperate
ggplot(patch, mapping = aes(x = V1, color = county, fill = county)) +
  geom_density(alpha=0.6)+
  facet_grid(~ county) +
  scale_color_brewer(palette = "YlGnBu") + scale_fill_brewer(palette = "YlGnBu") + theme_dark()
## histograms seperate
ggplot(patch, mapping = aes(x = V1, color = county, fill = county)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, binwidth = 1)+
  facet_grid(~ county) +
  scale_color_brewer(palette = "YlGnBu") + scale_fill_brewer(palette = "YlGnBu") + theme_dark()
## Histograms + density plots -- seperate by county
ggplot(patch, mapping = aes(x = V1, color = county, fill = county)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, binwidth = 1, fill = "white")+
  geom_density(alpha=0.6)+
  facet_grid(~ county) +
  scale_color_brewer(palette = "YlGnBu") + scale_fill_brewer(palette = "YlGnBu") + theme_dark()

ggplot(patch, aes(x=V1, color=county, fill=county)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, binwidth = 1)
#Perform Levene's Test of Equality of Variances

#close to 0 --> rejected
#with this rejected it means that the signifance level will be underestimated
#which means you could falsely reject the null
#The Levene’s test uses an F-test to test the null hypothesis that the variance 
# is equal across groups.  A p value less than .05 indicates a violation of the assumption. 
# If a violation occurs, it is likely that conducting the non-parametric equivalent of the 
# analysis is more appropriate.
lev1<-leveneTest(V1 ~ county, data=patch, center="mean")
lev2<-leveneTest(V1 ~ county, data=patch, center="median")
print(lev1)
print(lev2)

#Produce boxplots and visually check for outliers
ggplot(patch, aes(x = county, y = V1, fill = county)) +
  stat_boxplot(geom ="errorbar", width = 0.5) +
  geom_boxplot() + 
  scale_fill_brewer(palette = "YlGnBu") +
  stat_summary(fun.y=mean, geom="point", shape=10, size=3.5, color="black") + 
  ggtitle("Boxplots of patch sizes for each county") + 
  theme_dark() + theme(legend.position="none")

ggplot(patch, aes(x = county, y = old, fill = county)) +
  stat_boxplot(geom ="errorbar", width = 0.5) +
  geom_boxplot() + 
  stat_summary(fun.y=mean, geom="point", shape=10, size=3.5, color="black") + 
  ggtitle("Boxplots of patch sizes for each county
          - not normalized") + 
  theme_bw() + theme(legend.position="none") + ylim(0, 125) 

#Perform an ANOVA to check for patch size differences between counties
#got a significance value of near 0
#F value of 8.51
#ran type two because there is no interaction
m1<-lm(V1 ~ county, data=patch, contrasts = c("contr.sum", "contr.poly"))
Anova(m1, type=2)
#because the assumption of equality of variance was rejected also tested with a
# Kruskal-wallis ANOVA 
# non-parametric method
kruskal.test( V1 ~ county, data=patch)

#Compute expected marginal means post-hoc tests
posthocs<-emmeans(m1, pairwise ~ county, adjust="tukey")

#Display post-hoc letter groupings
comparisons <- multcomp::cld(posthocs$emmeans, details=TRUE, sort=TRUE, alpha=0.05, Letters = letters, adjust="tukey")
comparisons
write.csv(comparisons, file = "comp.csv")
Export(comparisons, comp, .csv)


#Plot estimated marginal means
emm <- summary(posthocs)$emmeans
ggplot(emm, aes(county)) +
  geom_line(aes(y = emmean, group = 1)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  geom_point(aes(y = emmean), size = 2, color = "blue") +
  labs(x = "County", y = "Estimated marginal mean", 
       title = "Estimated marginal means with 
       95% confidence intervals") +
  theme_bw()

#Plot contrasts
plot(posthocs$contrasts) +
  geom_vline(xintercept = 0) + 
  theme_bw() +
  labs(y = "Contrast", 
       x = "Estimated marginal mean difference", 
       title = "Estimated marginal mean differences 
       with 95% confidence intervals")

