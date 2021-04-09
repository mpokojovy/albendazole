##########################################################################################
# R program by (C) Michael Pokojovy and Paul Goldstein (2021)                            #
#                                                                                        #
# Original article:                                                                      #
# Albendazole (Anthelmintic Benzimidazole)-Induced Morphological Changes                 #
# in Female Ascaris lumbricoides var. suum and Loss of Synaptonemal Complexes in Meiosis #
##########################################################################################

# Two-sample test for proportions

prop.test(x = c(38, 380), n = c(380, 380), alternative = "less")

# ANOVA

set.seed(1)

df = data.frame(group = c(rep("CTRL", 380), rep("TRT", 380)),
                eggs.infertile = c(sample(c(1, 0), size = 380, replace = TRUE, prob = c( 38/380, 342/380)),
                                   sample(c(1, 0), size = 380, replace = TRUE, prob = c(380/380,   0/380))))

aov.fit = aov(eggs.infertile ~ group, data = df)

print(summary(aov.fit))
print(TukeyHSD(aov.fit))

plot(aov.fit)