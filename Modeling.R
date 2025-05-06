df$Groups <- as.factor(df$Groups)
dfsame$Groups <- as.factor(dfsame$Groups)


summary(df)
summary(dfsame)

library(ggplot2)
library(dplyr)
library(lme4)
library(stats)

lmm_model <- lmer(OpticalProperty ~ Groups + (1 | subID), data = df,REML = FALSE)
summary(lmm_model)

lmm_model2 <- lmer(OpticalProperty ~ Groups + (Groups | subID), data = df,REML = FALSE)
summary(lmm_model2)

anova(lmm_model, lmm_model2)



lmm_model_same <- lmer(OpticalProperty ~ Groups + (1 | subID), data = dfsame,REML = FALSE)
summary(lmm_model_same)


lmm_model2_same <- lmer(OpticalProperty ~ Groups + (Groups | subID), data = dfsame,REML = FALSE)
summary(lmm_model2_same)

anova(lmm_model_same, lmm_model2_same)



