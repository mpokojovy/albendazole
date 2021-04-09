##########################################################################################
# R program by (C) Michael Pokojovy and Paul Goldstein (2021)                            #
#                                                                                        #
# Original article:                                                                      #
# Albendazole (Anthelmintic Benzimidazole)-Induced Morphological Changes                 #
# in Female Ascaris lumbricoides var. suum and Loss of Synaptonemal Complexes in Meiosis #
##########################################################################################

set.seed(1)

df = data.frame(group = c(rep("TRT", 213), rep("CTRL", 380), rep("CTRL", 380)),
                males.present = c(rep("YES", 213), rep("NO", 380), rep("YES", 380)),
                girdle.present = c(sample(c("YES", "NO"), size = 213, replace = TRUE, prob = c(187/213, 26/213)),
                                   sample(c("YES", "NO"), size = 380, replace = TRUE, prob = c(372/380, 8/380)),
                                   sample(c("YES", "NO"), size = 380, replace = TRUE, prob = c(8/380, 372/380))))


# Logistic regression

require(rms)
lrm.model <- lrm(girdle.present ~ group + males.present, data = df)

print(lrm.model)

library(pROC)
test_prob = predict(lrm.model, newdata = df)
test_roc = roc(df$girdle.present ~ test_prob, plot = TRUE, print.auc = TRUE)

# Anova

df = data.frame(group = c(rep("TRT", 213), rep("CTRL", 380), rep("CTRL", 380)),
                males.present = c(rep("YES", 213), rep("NO", 380), rep("YES", 380)),
                girdle.present = c(sample(c(1, 0), size = 213, replace = TRUE, prob = c(187/213, 26/213)),
                                   sample(c(1, 0), size = 380, replace = TRUE, prob = c(372/380, 8/380)),
                                   sample(c(1, 0), size = 380, replace = TRUE, prob = c(8/380, 372/380))))

aov.fit = aov(girdle.present ~ group + males.present, data = df)

print(summary(aov.fit))

print(TukeyHSD(aov.fit))

# GLM

glm.model = glm(girdle.present ~ group + males.present, data = df, family = binomial())
plot(glm.model)