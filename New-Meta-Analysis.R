# This file documents a meta-analysis study of the effect sizes the language 
#  essessments of mild AD vs MCI and other related contrasts across studies in 
#  different languages.

# The document are all accessible in the shared google drive folder with Tom 
#  and Chacen.

install.packages("metafor")
library(metafor)
library(dplyr)


#Step 1 Calculate SE for Each Effect Sized and Convert Cohen's D into Hedge's G
your_data$SE <- sqrt(
  (your_data$Group_Size_1 + your_data$Group_Size_2) / 
    (your_data$Group_Size_1 * your_data$Group_Size_2) +
  (your_data$Effect_Size_New^2) / 
    (2 * (your_data$Group_Size_1 + your_data$Group_Size_2))
your_data$Effect_Size_New <- your_data$Effect_Size_New * 
  (1 - 3 / (4 * (your_data$Group_Size_1 + your_data$Group_Size_2) - 9))
#Step 2 Split Data by Comparison Type
comparisons <- c("NE_vs_MCI", "NE_vs_AD", "MCI_vs_AD", "MCI_vs_mAD", "NE_vs_mAD", "AD_vs_mAD")
dat_list <- lapply(comparisons, function(comp) {
  subset(your_data, Comparison == comp)
})
names(dat_list) <- comparisons
#Step 3 Run Meta-Analysis for Each Comparison
library(metafor)

meta_list <- lapply(dat_list, function(df) {
  rma(
    yi = Effect_Size_New,  # Your Hedges’ g or Cohen’s d
    sei = SE,              # Calculated SE
    data = df,
    method = "REML"        # Random-effects model
  )
})
names(meta_list) <- paste0("meta_", gsub("_vs_", "_", comparisons))
#Step 4 Generate Multi-Panel Forest Plot
par(mfrow = c(3, 2), mar = c(4, 4, 2, 2))  # Adjust margins

for (i in seq_along(meta_list)) {
  forest(
    meta_list[[i]],
    main = names(meta_list)[i],
    xlab = "SMD (95% CI)",
    slab = dat_list[[i]]$Study  # Show study labels
  )
  # Add heterogeneity stats
  text(-4, -1, pos = 4, cex = 0.8,
       paste("I² =", round(meta_list[[i]]$I2, 1), "%",
            "τ² =", round(meta_list[[i]]$tau2, 2)))
}
  
  
datafile = "C:/Users/Camel/Downloads/Final Testing Sheet - ALL.csv"
dat = read.csv(datafile)
View(dat)

names(dat)
comp_type <- "Control vs MCI"  # Change this to filter by a different comparison type

filtered_data <- dat %>% filter(ComparisonType == comp_type) # Filter the dataset based on the specified comparison type

# narrow down to a particular Comparison
# TODO: we have to recategorize the labels so we can do the analysis

# TODO: add descriptive statistics such as mean, sd of effect sizes; number of
# languages, etc.

# Calculate effect sizes with Hedge's g
dat_es <- escalc(
  measure = "SMDH",  # "SMDH" specifies Hedge's g
  m1i = Group.Mean.1, sd1i = SD.1, n1i = Group.Size.1,
  m2i = Group.Mean.2, sd2i = SD.2, n2i = Group.Size.2,
  data = filtered_data
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
