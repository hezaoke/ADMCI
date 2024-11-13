# This file documents a meta-analysis study of the effect sizes the language 
#  essessments of mild AD vs MCI and other related contrasts across studies in 
#  different languages.

# The document are all accessible in the shared google drive folder with Tom 
#  and Chacen.

install.packages("metafor")
library(metafor)

setwd("/Users/kehezao/Documents/Alzheimer_disease/MetaAnalysis_MAD and MCI")
datafile = "Testing Sheet for Effect Size Calculations.csv"
dat = read.csv(datafile)
View(dat)

names(dat)

# narrow down to a particular Comparison
# TODO: we have to recategorize the labels so we can do the analysis

# TODO: add descriptive statistics such as mean, sd of effect sizes; number of
# languages, etc.

# Calculate effect sizes with Hedge's g
dat_es <- escalc(
  measure = "SMDH",  # "SMDH" specifies Hedge's g
  m1i = Group.Mean.1, sd1i = SD.1, n1i = Group.Size.1,
  m2i = Group.Mean.2, sd2i = SD.2, n2i = Group.Size.2,
  data = dat
)

View(dat_es)

# for (i in 1:length(dat_es$yi)){
#   print(i)
#   print(dat_es$yi[i])
#   print(dat$Effect.Size..New.[i])
# }

unique(dat_es$Language)

# somehow there is slight difference in the results; 
# resolve this later by looking at the way metafor calculate Hedge's g.

meta_model = rma(yi,vi, data=dat_es, method="REML") 
#REML: restricted/residual maximum likelihood

summary(meta_model)
forest(meta_model)
funnel(meta_model)

meta_model_mod1 = rma(yi,vi, data=dat_es, mods=~Language, method="REML") 
summary(meta_model_mod1)
confint(meta_model_mod1)

meta_model_mod2 = rma(yi,vi, data=dat_es, mods=~Language+year, method="REML") 
summary(meta_model_mod2)




