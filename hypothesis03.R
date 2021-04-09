##########################################################################################
# R program by (C) Michael Pokojovy and Paul Goldstein (2021)                            #
#                                                                                        #
# Original article:                                                                      #
# Albendazole (Anthelmintic Benzimidazole)-Induced Morphological Changes                 #
# in Female Ascaris lumbricoides var. suum and Loss of Synaptonemal Complexes in Meiosis #
##########################################################################################

# Two-sample test for proportions

set.seed(1)

df = data.frame(group          = c(rep("CTRL", 167), rep("CTRL", 167), rep("TRT", 213), rep("TRT", 213)),
                girdle.present = c(rep("NO",   167), rep("YES",  167), rep("NO",  213), rep("YES", 213)), 
                infertile.eggs = c(sample(c("YES", "NO"), size = 167, replace = TRUE, prob = c(  2/167, 165/167)),
                                   sample(c("YES", "NO"), size = 167, replace = TRUE, prob = c(165/167,   2/167)),
                                   sample(c("YES", "NO"), size = 213, replace = TRUE, prob = c(187/213,  26/213)),
                                   sample(c("YES", "NO"), size = 213, replace = TRUE, prob = c( 26/213, 187/213))))

p.CTRL.girdle    = length(which((df$infertile.eggs == "YES") & (df$group == "CTRL") & (df$girdle.present == "YES")))/length(which((df$group == "CTRL") & (df$girdle.present == "YES")))
p.TRT.girdle     = length(which((df$infertile.eggs == "YES") & (df$group == "TRT")  & (df$girdle.present == "YES")))/length(which((df$group == "TRT")  & (df$girdle.present == "YES")))
p.CTRL.no.girdle = length(which((df$infertile.eggs == "YES") & (df$group == "CTRL") & (df$girdle.present == "NO")))/length(which((df$group == "CTRL") & (df$girdle.present == "NO")))
p.TRT.no.girdle  = length(which((df$infertile.eggs == "YES") & (df$group == "TRT")  & (df$girdle.present == "NO")))/length(which((df$group == "TRT")  & (df$girdle.present == "NO")))

plot(c(0, 1), c(p.CTRL.no.girdle, p.CTRL.girdle), 
     xlab = "Genital girdle present", ylab = "Proportion of infertile eggs", 
     xlim = c(0, 1), ylim = c(0, 1.25), xaxt = "n",
     main = "Control group", col = "blue", lwd = 2)
axis(1, at = c(0, 1), labels = c("NO", "YES"))

lines(c(0, 1), c(p.CTRL.no.girdle, p.CTRL.girdle), col = "blue", lwd = 2)
points(c(0, 1), c(p.TRT.no.girdle,  p.TRT.girdle), col = "red", lwd = 2)
lines(c(0, 1), c(p.TRT.no.girdle,  p.TRT.girdle), col = "red", lwd = 2)

legend("topright", legend = c("Control", "Treatment"), pch = 1, lwd = 2, col = c("blue", "red"))
        
# Logistic regression

require(rms)
lrm.model <- lrm(infertile.eggs ~ girdle.present + group + girdle.present*group, data = df)

print(lrm.model)
print(anova(lrm.model))

library(pROC)
test_prob = predict(lrm.model, newdata = df)
test_roc = roc(df$infertile.eggs ~ test_prob, plot = TRUE, print.auc = TRUE)

# GLM

glm.model = glm(infertile.eggs ~ girdle.present + group + girdle.present*group, data = df, family = binomial())
plot(glm.model)