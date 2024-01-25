library(dplyr)
library(tidyverse)
library(Hmisc)
library(tidyr)
library(haven)
library(acid)
library(StatMatch)
library(mitools)
library(survey)
library(mice)
#Load data
data_household95 <- read.LIS('us95h')
data_individual95 <- read.LIS('us95p')
us95wr <- read.LIS('us95r')
weight_us95 <- us95wr %>% replace(is.na(.), 0)
anes96 <- read_dta(paste(USR_DIR, "/stijan/nes96.dta", sep=""))
print(head(anes96))
#data transformation
data_lws_95 <- merge(data_household95, data_individual95, by.x = "hid", by.y = "hid", all.x = TRUE)
selected_vars <- c("V960366", "V960552","V960604", "V960606", "V960610", "V960615","V960702", "V960701","V960365" )
anes_data96 <- anes96[, selected_vars]
data_lws_95 <- rename(data_lws_95, education = educlev)
data_lws_95 <- data_lws_95 %>%
  mutate(
    education = case_when(
      education %in% c("[110]less than primary", "[111]never attended", "[120]primary") ~ 1,
      education %in% c("[130] lower secondary" ,"[100]low, less than upper secondary", "[200]medium, upper secondary and post-secondary non-tertiary", "[210]upper secondary") ~ 2,
      education %in% c("[220]post-secondary non-tertiary", "[300]high, tertiary", "[311]short-cycle tertiary") ~ 3,
      education %in% c("[310]BA, MA or equivalent, short-cycle tertiary", "[312]bachelor or equivalent") ~ 4,
      education %in% c("[313]master or equivalent", "[320]doctorate or equivalent") ~ 5,
      TRUE ~ NA_integer_
    )
  )
print(unique(data_lws_95$education))
anes_data96 <- rename(anes_data96, education = V960610)
print(unique(anes_data96$education))
anes_data96<- anes_data96 %>%
  mutate(
    education = case_when(
      education == 1 ~ 1,
      education %in% c(2,3,4) ~ 2,
      education == 5 ~ 3,
      education == 6 ~ 4,
      education == 7 ~ 5,
      TRUE ~ NA_real_
    )
  )
print(unique(anes_data96$education))
#employment
data_lws_95 <- rename(data_lws_95, employment = lfs)
print(unique(data_lws_95$employment))
data_lws_95 <- data_lws_95 %>%
  mutate(
    employment = case_when(
      employment == "[100]employed" ~ 1,
      employment == "[200]unemployed" ~ 2,
      employment %in% c("[300]not in labour force","[330]disabled", "[340]homemaker" ) ~ 3,
      employment == "[310]retired" ~ 4,
      employment == "[320]in education" ~ 5,
      TRUE ~ NA_integer_
    )
  )
print(unique(data_lws_95$employment))
anes_data96 <- rename(anes_data96, employment = V960615)
anes_data96 <- anes_data96 %>%
  mutate(
    employment = case_when(
      employment %in% c(10,15,16,17,18) ~ 1,
      employment %in% c(20,40) ~ 2,
      employment %in% c(70,71,75) ~ 3,
      employment %in% c(50, 51) ~ 4,
      employment %in% c(80, 81) ~ 5,
      TRUE ~ NA_integer_
    )
  )
print(unique(anes_data96$employment))
data_lws_95 <- rename(data_lws_95, income = hitotal)
data_lws_95 <- data_lws_95 %>%
  mutate(
    income = case_when(
      income < 14768 ~ 1,
      income >= 14768 & income <= 22273 ~ 2,
      income >= 22274 & income <= 36653 ~ 3,
      income >= 36654 & income <= 56369 ~ 4,
      income > 56359 ~ 5,
      TRUE ~ NA_real_
    )
  )
anes_data96$V960701 <- as.factor(anes_data96$V960701)
names(anes_data96)[names(anes_data96) == "V960701"] <- "income"
anes_data96 <- anes_data96%>%
  mutate(
    income = as.numeric(case_when(
      income %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ 1,
      income %in% c("11", "12", "13") ~ 2,
      income %in% c("14", "15", "16") ~ 3,
      income %in% c("17", "18", "19", "20") ~ 4,
      income %in% c("21", "22", "23", "24") ~ 5,
      TRUE ~ NA_real_
    ))
  ) 
anes96_full <- anes_data96[complete.cases(anes_data96$employment), ]
anes96_full <- anes96_full[complete.cases(anes96_full$income), ]
anes96_full <- anes96_full[complete.cases(anes96_full$education), ]
lws95 <- data_lws_95[complete.cases(data_lws_95$employment), ]
lws95 <- lws95[complete.cases(lws95$income), ]
lws95 <- lws95[complete.cases(lws95$education), ]
# If employment is 2, income is 1, and education is 1 then education should become 2
anes96_full$education <- ifelse(anes96_full$employment == 2 & anes96_full$income == 1 & anes96_full$education == 1, 2, anes96_full$education)
lws95$education <- ifelse(lws95$employment == 2 & lws95$income == 1 & lws95$education == 1, 2, lws95$education)
# If employment is 2, income is 2, and education is 1 then education should become 2
anes96_full$education <- ifelse(anes96_full$employment == 2 & anes96_full$income == 2 & anes96_full$education == 1, 2, anes96_full$education)
lws95$education <- ifelse(lws95$employment == 2 & lws95$income == 2 & lws95$education == 1, 2, lws95$education)
# If employment is 3, income is 3, and education is 1 then education should become 2
anes96_full$education <- ifelse(anes96_full$employment == 3 & anes96_full$income == 3 & anes96_full$education == 1, 2, anes96_full$education)
lws95$education <- ifelse(lws95$employment == 3 & lws95$income == 3 & lws95$education == 1, 2, lws95$education)
# If employment is 4, income is 4, and education is 1 then education should become 2
anes96_full$education <- ifelse(anes96_full$employment == 4 & anes96_full$income == 4 & anes96_full$education == 1, 2, anes96_full$education)
lws95$education <- ifelse(lws95$employment == 4 & lws95$income == 4 & lws95$education == 1, 2, lws95$education)
# If employment is 1, income is 5, and education is 1 then education should become 2
anes96_full$education <- ifelse(anes96_full$employment == 1 & anes96_full$income == 5 & anes96_full$education == 1, 2, anes96_full$education)
lws95$education <- ifelse(lws95$employment == 1 & lws95$income == 5 & lws95$education == 1, 2, lws95$education)
# If employment is 3, income is 5, and education is 1 then education should become 2
anes96_full$education <- ifelse(anes96_full$employment == 3 & anes96_full$income == 5 & anes96_full$education == 1, 2, anes96_full$education)
lws95$education <- ifelse(lws95$employment == 3 & lws95$income == 5 & lws95$education == 1, 2, lws95$education)
# If employment is 3, income is 2, and education is 3 then education should become 2
anes96_full$education <- ifelse(anes96_full$employment == 3 & anes96_full$income == 2 & anes96_full$education == 3, 2, anes96_full$education)
lws95$education <- ifelse(lws95$employment == 3 & lws95$income == 2 & lws95$education == 3, 2, lws95$education)
# If employment is 5, income is 2, and education is 3 then education should become 2
anes96_full$education <- ifelse(anes96_full$employment == 5 & anes96_full$income == 2 & anes96_full$education == 3, 2, anes96_full$education)
lws95$education <- ifelse(lws95$employment == 5 & lws95$income == 2 & lws95$education == 3, 2, lws95$education)
# If employment is 2, income is 3, and education is 3 then education should become 4
anes96_full$education <- ifelse(anes96_full$employment == 2 & anes96_full$income == 3 & anes96_full$education == 3, 4, anes96_full$education)
lws95$education <- ifelse(lws95$employment == 2 & lws95$income == 3 & lws95$education == 3, 4, lws95$education)
# If employment is 5, income is 4, and education is 3 then education should become 4
anes96_full$education <- ifelse(anes96_full$employment == 5 & anes96_full$income == 4 & anes96_full$education == 3, 4, anes96_full$education)
lws95$education <- ifelse(lws95$employment == 5 & lws95$income == 4 & lws95$education == 3, 4, lws95$education)
# If employment is 2, income is 5, and education is 3 then education should become 2
anes96_full$education <- ifelse(anes96_full$employment == 2 & anes96_full$income == 5 & anes96_full$education == 3, 2, anes96_full$education)
lws95$education <- ifelse(lws95$employment == 2 & lws95$income == 5 & lws95$education == 3, 2, lws95$education)
# If employment is 3, income is 1, and education is 4 then education should become 3
anes96_full$education <- ifelse(anes96_full$employment == 3 & anes96_full$income == 1 & anes96_full$education == 4, 3, anes96_full$education)
lws95$education <- ifelse(lws95$employment == 3 & lws95$income == 1 & lws95$education == 4, 3, lws95$education)
# If employment is 3, income is 2, and education is 4 then education should become 5
anes96_full$education <- ifelse(anes96_full$employment == 3 & anes96_full$income == 2 & anes96_full$education == 4, 5, anes96_full$education)
lws95$education <- ifelse(lws95$employment == 3 & lws95$income == 2 & lws95$education == 4, 5, lws95$education)
# If employment is 5, income is 2, and education is 4 then education should become 2
anes96_full$education <- ifelse(anes96_full$employment == 5 & anes96_full$income == 2 & anes96_full$education == 4, 2, anes96_full$education)
lws95$education <- ifelse(lws95$employment == 5 & lws95$income == 2 & lws95$education == 4, 2, lws95$education)
# If employment is 3, income is 3, and education is 4 then education should become 3
anes96_full$education <- ifelse(anes96_full$employment == 3 & anes96_full$income == 3 & anes96_full$education == 4, 3, anes96_full$education)
lws95$education <- ifelse(lws95$employment == 3 & lws95$income == 3 & lws95$education == 4, 3, lws95$education)
# If employment is 5, income is 3, and education is 4 then education should become 3
anes96_full$education <- ifelse(anes96_full$employment == 5 & anes96_full$income == 3 & anes96_full$education == 4, 3, anes96_full$education)
lws95$education <- ifelse(lws95$employment == 5 & lws95$income == 3 & lws95$education == 4, 3, lws95$education)
# If employment is 5, income is 5, and education is 4 then education should become 3
anes96_full$education <- ifelse(anes96_full$employment == 5 & anes96_full$income == 5 & anes96_full$education == 4, 3, anes96_full$education)
lws95$education <- ifelse(lws95$employment == 5 & lws95$income == 5 & lws95$education == 4, 3, lws95$education)
# If employment is 3, income is 1, and education is 5 then education should become 3
anes96_full$education <- ifelse(anes96_full$employment == 3 & anes96_full$income == 1 & anes96_full$education == 5, 3, anes96_full$education)
lws95$education <- ifelse(lws95$employment == 3 & lws95$income == 1 & lws95$education == 5, 3, lws95$education)
# If employment is 5, income is 1, and education is 5 then education should become 4
anes96_full$education <- ifelse(anes96_full$employment == 5 & anes96_full$income == 1 & anes96_full$education == 5, 4, anes96_full$education)
lws95$education <- ifelse(lws95$employment == 5 & lws95$income == 1 & lws95$education == 5, 4, lws95$education)
# If employment is 4, income is 2, and education is 5 then education should become 4
anes96_full$education <- ifelse(anes96_full$employment == 4 & anes96_full$income == 2 & anes96_full$education == 5, 4, anes96_full$education)
lws95$education <- ifelse(lws95$employment == 4 & lws95$income == 2 & lws95$education == 5, 4, lws95$education)
# If employment is 5, income is 2, and education is 5 then education should become 2
anes96_full$education <- ifelse(anes96_full$employment == 5 & anes96_full$income == 2 & anes96_full$education == 5, 2, anes96_full$education)
lws95$education <- ifelse(lws95$employment == 5 & lws95$income == 2 & lws95$education == 5, 2, lws95$education)
# If employment is 2, income is 3, and education is 5 then education should become 4
anes96_full$education <- ifelse(anes96_full$employment == 2 & anes96_full$income == 3 & anes96_full$education == 5, 4, anes96_full$education)
lws95$education <- ifelse(lws95$employment == 2 & lws95$income == 3 & lws95$education == 5, 4, lws95$education)
# If employment is 3, income is 3, and education is 5 then education should become 4
anes96_full$education <- ifelse(anes96_full$employment == 3 & anes96_full$income == 3 & anes96_full$education == 5, 4, anes96_full$education)
lws95$education <- ifelse(lws95$employment == 3 & lws95$income == 3 & lws95$education == 5, 4, lws95$education)
# If employment is 3, income is 4, and education is 5 then education should become 4
anes96_full$education <- ifelse(anes96_full$employment == 3 & anes96_full$income == 4 & anes96_full$education == 5, 4, anes96_full$education)
lws95$education <- ifelse(lws95$employment == 3 & lws95$income == 4 & lws95$education == 5, 4, lws95$education)
# If employment is 2, income is 5, and education is 5 then education should become 4
anes96_full$education <- ifelse(anes96_full$employment == 2 & anes96_full$income == 5 & anes96_full$education == 5, 4, anes96_full$education)
lws95$education <- ifelse(lws95$employment == 2 & lws95$income == 5 & lws95$education == 5, 4, lws95$education)
# If employment is 5, income is 5, and education is 5 then education should become 3
anes96_full$education <- ifelse(anes96_full$employment == 5 & anes96_full$income == 5 & anes96_full$education == 5, 3, anes96_full$education)
lws95$education <- ifelse(lws95$employment == 5 & lws95$income == 5 & lws95$education == 5, 3, lws95$education)
# If employment is 5, income is 3, and education is 3 then education should become 2
anes96_full$education <- ifelse(anes96_full$employment == 5 & anes96_full$income == 3 & anes96_full$education == 3, 2, anes96_full$education)
lws95$education <- ifelse(lws95$employment == 5 & lws95$income == 3 & lws95$education == 3, 2, lws95$education)
# If employment is 3, income is 3, and education is 4 then education should become 2
anes96_full$education <- ifelse(anes96_full$employment == 3 & anes96_full$income == 3 & anes96_full$education == 4, 2, anes96_full$education)
lws95$education <- ifelse(lws95$employment == 3 & lws95$income == 3 & lws95$education == 4, 2, lws95$education)
group.v <- c("employment", "income","education")
rnd.1 <- RANDwNND.hotdeck(data.rec = lws95, data.don = anes96_full,
                          match.vars = NULL, don.class = group.v)
fused_us96 <- create.fused(data.rec = lws95, data.don = anes96_full,
                           mtc.ids = rnd.1$mtc.ids, z.vars = c("V960552"))
unique(fused_us96$V960552)
fused_us96$net_wealth <- (fused_us96$haf + fused_us96$han) - (fused_us96$hlr + fused_us96$hln)
fused_us96$quintiles <- cut(fused_us96$net_wealth, breaks = quantile(fused_us96$net_wealth, probs = seq(0, 1, by = 0.2)), include.lowest = TRUE, labels = FALSE)
dfs <- c("fused_us96")
for (i in 1:length(dfs)) {
  tmp <- get(dfs[i])
  mi.idx <- tmp$inum.x
  for (j in 1:5) {
    assign(paste0(dfs[i], "_", j), tmp[mi.idx == j,])
  }
}
# Get the dimensions of each data frame
for (j in 1:5) {
  df_name <- paste0("fused_us96_", j)
  print(paste("The dimensions of", df_name, "are: ", dim(get(df_name))))
}
imputed_data <- imputationList(list(fused_us96_1, fused_us96_2, fused_us96_3, fused_us96_4, fused_us96_5))
us.svyrw <- svrepdesign(
  data = imputed_data,
  id = ~hid,
  weights = ~hpopwgt,
  repweights = weight_us95[, -1],  # Exclude first column with id
  scale = 1,
  rscales = rep(1 / 100),  # If using full set
  type = "other",
  combined.weights = TRUE
)
us.svyrw$designs <- lapply(us.svyrw$designs, function(design) {
  design$variables$V960552 <- ifelse(design$variables$V960552 == 1, 1,
                                     ifelse(design$variables$V960552 == 2, 0, NA))
  return(design)
})
# Fit the probit model on each imputed dataset
model_1 <- lapply(us.svyrw$designs, function(design) {
  svyglm(V960552 ~ 1 + net_wealth, design, family = binomial(link = 'probit'))
})
# Combine results from multiple imputed datasets
combined_results <- MIcombine(model_1)
# Display summary of combined results
coef(combined_results)

