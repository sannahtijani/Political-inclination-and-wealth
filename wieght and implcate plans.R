# Load required libraries
library(mitools)
library(survey)
library(mice)
library(dplyr)

# Assuming dfs contains the names of your imputed datasets
dfs <- c("fA.rnd.1")

# Read replicate weights
us16wr <- read_dta("us16wr.dta")
us16wr <- us16wr %>% replace(is.na(.), 0)

# Create separate data frames for each imputed dataset
for (i in 1:length(dfs)) {
  tmp <- get(dfs[i])
  mi.idx <- tmp$inum.x
  for (j in 1:5) {
    assign(paste0(dfs[i], "_", j), tmp[mi.idx == j,])
  }
}

# Create an imputation list
imputed_data <- imputationList(list(fA.rnd.1_1, fA.rnd.1_2, fA.rnd.1_3, fA.rnd.1_4, fA.rnd.1_5))

# Create survey design for each imputed dataset
us.svyrw <- svrepdesign(
  data = imputed_data,
  id = ~hid,
  weights = ~hpopwgt,
  repweights = us16wr[, -1],  # Exclude first column with id
  scale = 1,
  rscales = rep(1 / 100),  # If using full set
  type = "other",
  combined.weights = TRUE
)

# Modify imputed data within the survey design object
us.svyrw$designs <- lapply(us.svyrw$designs, function(design) {
  design$variables$V085044a <- ifelse(design$variables$V085044a == 1, 0,
                                      ifelse(design$variables$V085044a == 3, 1, NA))
  return(design)
})

# Fit the probit model on each imputed dataset
model_1 <- lapply(us.svyrw$designs, function(design) {
  svyglm(V085044a ~ 1 + haf, design, family = binomial(link = 'probit'))
})

# Combine results from multiple imputed datasets
combined_results <- MIcombine(model_1)

# Display summary of combined results
summary(combined_results)


######modify below

model_2 <- lapply(us.svyrw, function(design) {
  svyglm(V085044a ~ 1 + ha + income + partisanship + age + gender + employment_dummy + retirement , design = design, family = binomial(link = 'probit'))
})

model_3 <- lapply(us.svyrw, function(design) {
  svyglm(V085044a ~ 1 + hanr, design = design, family = binomial(link = 'probit'))
})

model_4 <- lapply(us.svyrw, function(design) {
  svyglm(V085044a ~ 1 + hanr + income + partisanship + age + gender + employment_dummy + retirement , design = design, family = binomial(link = 'probit'))
})

model_5 <- lapply(us.svyrw, function(design) {
  svyglm(V085044a ~ 1 + haf, design = design, family = binomial(link = 'probit'))
})

model_6 <- lapply(us.svyrw, function(design) {
  svyglm(V085044a ~ 1 + haf + income + partisanship + age + gender + employment_dummy + retirement , design = design, family = binomial(link = 'probit'))
})

#models <- lapply(designs, function(design) {
  #svyglm(response ~ predictor1 + predictor2, design = design, family = binomial(link = 'probit'))
#})


#combine the results from the multiple imputation
combined_results <- MIcombine(model_2)
combined_results <- MIcombine(model_3)
combined_results <- MIcombine(model_4)
combined_results <- MIcombine(model_5)
combined_results <- MIcombine(model_6)

