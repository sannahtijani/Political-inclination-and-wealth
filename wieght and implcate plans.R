#for weight and implicate

# create have one data frame for each imputed dataset, 
#and each data frame should include a column for the weights

# create imputed list
imputed_data <- imputationList(list(df1, df2, df3, df4, df5))
#svyrepdsgn

#create survey design for each imputed dataset
us.svyrw <- svrepdesign(data=us.mi, 
                        id = ~hid,
                        weights=~hpopwgt, 
                        repweights=us16wr[, -1], #exclude first col with id
                        scale = 1 ,
                        rscales = rep( 1 / 100 ) , #if using full set
                        type = "other",
                        combined.weights=TRUE)


designs <- lapply(imputed_data$imputations, function(df) {
  svydesign(ids = ~1, data = df, weights = ~weight_column)
})

#fit the probit model
models <- lapply(designs, function(design) {
  svyglm(response ~ predictor1 + predictor2, design = design, family = binomial(link = 'probit'))
})


#combine the results from the multiple imputation
combined_results <- MIcombine(models)


