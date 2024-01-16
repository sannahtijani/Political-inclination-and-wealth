#for weight and implicate
#for weight and implicate
library(mitools)
library(survey)
library(mice)
library(tidyverse)

#for weight and implicate
library(mitools)
library(survey)
library(mice)
library(tidyverse)

# create have one data frame for each imputed dataset, 
#and each data frame should include a column for the weights
#seprate the dataframes into smaller one by implicate 
dfs <- c("merged08_data1")
us16wr <- read_dta("us16wr.dta")
us16wr <- us16wr %>% replace(is.na(.), 0)


for(i in 1:length(dfs)){
  tmp <- get(dfs[i])
  mi.idx<-tmp$inum.x
  for(j in 1:5) {
    #assign(filename, what)
    assign(paste0(dfs[i], "_", j), tmp[mi.idx==j,])
  }
}
# I have a question because we have both inum.x and inum.x, so should we do it for both or used an additional method
# create imputed list
imputed_data <- imputationList(list(merged08_data1_1, merged08_data1_2, merged08_data1_3, merged08_data1_4, merged08_data1_5))
#svyrepdsgn


#create survey design for each imputed dataset
us.svyrw <- svrepdesign(data=imputed_data, 
                        id = ~hid,
                        weights=~hpopwgt, 
                        repweights=us16wr[, -1], #exclude first col with id
                        scale = 1 ,
                        rscales = rep( 1 / 100 ) , #if using full set
                        type = "other",
                        combined.weights=TRUE)


#fit the probit model
models <- lapply(designs, function(design) {
  svyglm(response ~ predictor1 + predictor2, design = design, family = binomial(link = 'probit'))
})


#combine the results from the multiple imputation
combined_results <- MIcombine(models)


