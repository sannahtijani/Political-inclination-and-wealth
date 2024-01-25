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
data_household01 <- read.LIS('us01h')
data_individual01 <- read.LIS('us01p')
us01wr <- read.LIS('us01r')
weight_us01 <- us01wr %>% replace(is.na(.), 0)
anes00 <- read_dta(paste(USR_DIR, "/stijan/anes2000TS.dta", sep=""))
print(head(anes00))

data_lws_01 <- merge(data_household01, data_individual01, by.x = "hid", by.y = "hid", all.x = TRUE)

selected_vars <- c("V000793", "V001249","V000908", "V000913", "V000918", "V000909","V000995")
anes_data00 <- anes00[, selected_vars]

#data cleaning
data_lws_01 <- rename(data_lws_01, education = educlev)
data_lws_01 <- data_lws_01 %>%
  mutate(
    education = case_when(
      education %in% c("[110]less than primary", "[111]never attended", "[120]primary", "[130] lower secondary","[100]low, less than upper secondary") ~ 1,
      education %in% c( "[200]medium, upper secondary and post-secondary non-tertiary", "[210]upper secondary") ~ 2,
      education %in% c("[300]high, tertiary", "[311]short-cycle tertiary", "[220]post-secondary non-tertiary" ) ~ 3,
      education %in% c("[310]BA, MA or equivalent, short-cycle tertiary", "[312]bachelor or equivalent") ~ 4,
      education %in% c("[313]master or equivalent", "[320]doctorate or equivalent") ~ 5,
      TRUE ~ NA_integer_
    )
  )

print(unique(data_lws_01$education))

anes_data00 <- rename(anes_data00, education = V000913)
print(unique(anes_data00$education))
anes_data00<- anes_data00 %>%
  mutate(
    education = case_when(
      education %in% c(1,2) ~ 1,
      education == 3 ~ 2,
      education %in% c(4, 5) ~ 3,
      education == 6 ~ 4,
      education == 7 ~ 5,
      TRUE ~ NA_real_
    )
  )
print(unique(anes_data00$education))

#employment
data_lws_01 <- rename(data_lws_01, employment = lfs)
print(unique(data_lws_01$employment))
data_lws_01 <- data_lws_01 %>%
  mutate(
    employment = case_when(
      employment == "[100]employed" ~ 1,
      employment == "[200]unemployed" ~ 2,
      employment %in% c("[300]not in labour force", "[340]homemaker", "[330]disabled", "[320]in education") ~ 3,
      employment == "[310]retired" ~ 4,
      TRUE ~ NA_integer_
    )
  )

print(unique(data_lws_01$employment))

anes_data00 <- rename(anes_data00, employment = V000918)
print(unique(anes_data00$employment))
anes_data00 <- anes_data00 %>%
  mutate(
    employment = case_when(
      employment == 1 ~ 1,
      employment %in% c(2,3) ~ 2,
      employment %in% c(5,6,7) ~ 3,
      employment == 4 ~ 4,
      TRUE ~ NA_integer_
    )
  )

print(unique(anes_data00$employment))

data_lws_01 <- rename(data_lws_01, income = hitotal)
data_lws_01 <- data_lws_01 %>%
  mutate(
    income = case_when(
      income < 17955 ~ 1,
      income >= 17955 & income <= 33006 ~ 2,
      income >= 33007 & income <= 52272 ~ 3,
      income >= 52273 & income <= 81960 ~ 4,
      income > 81960 ~ 5,
      TRUE ~ NA_real_
    )
  )

anes_data00$V000995 <- as.factor(anes_data00$V000995)
names(anes_data00)[names(anes_data00) == "V000995"] <- "income"
anes_data00 <- anes_data00%>%
  mutate(
    income = as.numeric(case_when(
      income %in% c("1", "2", "3") ~ 1,
      income %in% c("4", "5") ~ 2,
      income == 6 ~ 3,
      income %in% c("7", "8", "9") ~ 4,
      income %in% c("10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22") ~ 5,
      TRUE ~ NA_real_
    ))
  ) 

anes00_full <- anes_data00[complete.cases(anes_data00$employment), ]
anes00_full <- anes00_full[complete.cases(anes00_full$income), ]
anes00_full <- anes00_full[complete.cases(anes00_full$education), ]
lws01 <- data_lws_01[complete.cases(data_lws_01$employment), ]
lws01 <- lws01[complete.cases(lws01$income), ]
lws01 <- lws01[complete.cases(lws01$education), ]

# If employment is 2, income is 3, and education is 1 then education should become 2
anes00_full$education <- ifelse(anes00_full$employment == 2 & anes00_full$income == 3 & anes00_full$education == 1, 2, anes00_full$education)
lws01$education <- ifelse(lws01$employment == 2 & lws01$income == 3 & lws01$education == 1, 2, lws01$education)

# If employment is 4, income is 3, and education is 1 then education should become 2
anes00_full$education <- ifelse(anes00_full$employment == 4 & anes00_full$income == 3 & anes00_full$education == 1, 2, anes00_full$education)
lws01$education <- ifelse(lws01$employment == 4 & lws01$income == 3 & lws01$education == 1, 2, lws01$education)

# If employment is 3, income is 4, and education is 1 then education should become 2
anes00_full$education <- ifelse(anes00_full$employment == 3 & anes00_full$income == 4 & anes00_full$education == 1, 2, anes00_full$education)
lws01$education <- ifelse(lws01$employment == 3 & lws01$income == 4 & lws01$education == 1, 2, lws01$education)

# If employment is 4, income is 4, and education is 1 then education should become 2
anes00_full$education <- ifelse(anes00_full$employment == 4 & anes00_full$income == 4 & anes00_full$education == 1, 2, anes00_full$education)
lws01$education <- ifelse(lws01$employment == 4 & lws01$income == 4 & lws01$education == 1, 2, lws01$education)

# If employment is 4, income is 5, and education is 1 or 2 then education should become 3
anes00_full$education <- ifelse(anes00_full$employment == 4 & anes00_full$income == 5 & (anes00_full$education == 1 | anes00_full$education == 2), 3, anes00_full$education)
lws01$education <- ifelse(lws01$employment == 4 & lws01$income == 5 & (lws01$education == 1 | lws01$education == 2), 3, lws01$education)

# If employment is 2, income is 4, and education is 2 then education should become 1
anes00_full$education <- ifelse(anes00_full$employment == 2 & anes00_full$income == 4 & anes00_full$education == 2, 1, anes00_full$education)
lws01$education <- ifelse(lws01$employment == 2 & lws01$income == 4 & lws01$education == 2, 1, lws01$education)

# If employment is 2, income is 5, and education is 2 then education should become 1
anes00_full$education <- ifelse(anes00_full$employment == 2 & anes00_full$income == 5 & anes00_full$education == 2, 1, anes00_full$education)
lws01$education <- ifelse(lws01$employment == 2 & lws01$income == 5 & lws01$education == 2, 1, lws01$education)

# If employment is 3, income is 5, and education is 2 then income should become 4
anes00_full$income <- ifelse(anes00_full$employment == 3 & anes00_full$income == 5 & anes00_full$education == 2, 4, anes00_full$income)
lws01$income <- ifelse(lws01$employment == 3 & lws01$income == 5 & lws01$education == 2, 4, lws01$income)

# If employment is 3, income is 5, and education is 1 then education should become 2 and income should become 4
anes00_full$education <- ifelse(anes00_full$employment == 3 & anes00_full$income == 5 & anes00_full$education == 1, 2, anes00_full$education)
anes00_full$income <- ifelse(anes00_full$employment == 3 & anes00_full$income == 5 & anes00_full$education == 1, 4, anes00_full$income)
lws01$education <- ifelse(lws01$employment == 3 & lws01$income == 5 & lws01$education == 1, 2, lws01$education)
lws01$income <- ifelse(lws01$employment == 3 & lws01$income == 5 & lws01$education == 1, 4, lws01$income)

# If employment is 2, income is 5, and education is 3 then education should become 4
anes00_full$education <- ifelse(anes00_full$employment == 2 & anes00_full$income == 5 & anes00_full$education == 3, 4, anes00_full$education)
lws01$education <- ifelse(lws01$employment == 2 & lws01$income == 5 & lws01$education == 3, 4, lws01$education)

# If employment is 3, income is 5, and education is 3 then income should become 4
anes00_full$income <- ifelse(anes00_full$employment == 3 & anes00_full$income == 5 & anes00_full$education == 3, 4, anes00_full$income)
lws01$income <- ifelse(lws01$employment == 3 & lws01$income == 5 & lws01$education == 3, 4, lws01$income)

# If employment is 2, income is 3, and education is 4 then education should become 3
anes00_full$education <- ifelse(anes00_full$employment == 2 & anes00_full$income == 3 & anes00_full$education == 4, 3, anes00_full$education)
lws01$education <- ifelse(lws01$employment == 2 & lws01$income == 3 & lws01$education == 4, 3, lws01$education)

# If employment is 2, income is 4, and education is 4 then education should become 3
anes00_full$education <- ifelse(anes00_full$employment == 2 & anes00_full$income == 4 & anes00_full$education == 4, 3, anes00_full$education)
lws01$education <- ifelse(lws01$employment == 2 & lws01$income == 4 & lws01$education == 4, 3, lws01$education)

# If employment is 3, income is 5, and education is 4 then income should become 4
anes00_full$income <- ifelse(anes00_full$employment == 3 & anes00_full$income == 5 & anes00_full$education == 4, 4, anes00_full$income)
lws01$income <- ifelse(lws01$employment == 3 & lws01$income == 5 & lws01$education == 4, 4, lws01$income)

# If employment is 4, income is 1, and education is 5 then education should become 4
anes00_full$education <- ifelse(anes00_full$employment == 4 & anes00_full$income == 1 & anes00_full$education == 5, 4, anes00_full$education)
lws01$education <- ifelse(lws01$employment == 4 & lws01$income == 1 & lws01$education == 5, 4, lws01$education)

# If employment is 2, income is 3, and education is 5 then education should become 3
anes00_full$education <- ifelse(anes00_full$employment == 2 & anes00_full$income == 3 & anes00_full$education == 5, 3, anes00_full$education)
lws01$education <- ifelse(lws01$employment == 2 & lws01$income == 3 & lws01$education == 5, 3, lws01$education)

# If employment is 3, income is 4, and education is 5 then education should become 4
anes00_full$education <- ifelse(anes00_full$employment == 3 & anes00_full$income == 4 & anes00_full$education == 5, 4, anes00_full$education)
lws01$education <- ifelse(lws01$employment == 3 & lws01$income == 4 & lws01$education == 5, 4, lws01$education)

# If employment is 2, income is 5, and education is 5 then education should become 4
anes00_full$education <- ifelse(anes00_full$employment == 2 & anes00_full$income == 5 & anes00_full$education == 5, 4, anes00_full$education)
lws01$education <- ifelse(lws01$employment == 2 & lws01$income == 5 & lws01$education == 5, 4, lws01$education)

# If employment is 2, income is 4, and education is 1 then income should become 3 and education should become 2
anes00_full$income <- ifelse(anes00_full$employment == 2 & anes00_full$income == 4 & anes00_full$education == 1, 3, anes00_full$income)
anes00_full$education <- ifelse(anes00_full$employment == 2 & anes00_full$income == 4 & anes00_full$education == 1, 2, anes00_full$education)
lws01$income <- ifelse(lws01$employment == 2 & lws01$income == 4 & lws01$education == 1, 3, lws01$income)
lws01$education <- ifelse(lws01$employment == 2 & lws01$income == 4 & lws01$education == 1, 2, lws01$education)

# If employment is 2, income is 5, and education is 1 then education should become 4
anes00_full$education <- ifelse(anes00_full$employment == 2 & anes00_full$income == 5 & anes00_full$education == 1, 4, anes00_full$education)
lws01$education <- ifelse(lws01$employment == 2 & lws01$income == 5 & lws01$education == 1, 4, lws01$education)

# If employment is 3, income is 5, and education is 2 then income should become 4
anes00_full$income <- ifelse(anes00_full$employment == 3 & anes00_full$income == 5 & anes00_full$education == 2, 4, anes00_full$income)
lws01$income <- ifelse(lws01$employment == 3 & lws01$income == 5 & lws01$education == 2, 4, lws01$income)

# If employment is 2, income is 4, and education is 3 then education should become 5
anes00_full$education <- ifelse(anes00_full$employment == 2 & anes00_full$income == 4 & anes00_full$education == 3, 5, anes00_full$education)
lws01$education <- ifelse(lws01$employment == 2 & lws01$income == 4 & lws01$education == 3, 5, lws01$education)

# If employment is 2, income is 3, and education is 1 then education should become 2
anes00_full$education <- ifelse(anes00_full$employment == 2 & anes00_full$income == 3 & anes00_full$education == 1, 2, anes00_full$education)
lws01$education <- ifelse(lws01$employment == 2 & lws01$income == 3 & lws01$education == 1, 2, lws01$education)

# If employment is 2, income is 4, and education is 5 then income should become 3 and education should become 3
anes00_full$income <- ifelse(anes00_full$employment == 2 & anes00_full$income == 4 & anes00_full$education == 5, 3, anes00_full$income)
anes00_full$education <- ifelse(anes00_full$employment == 2 & anes00_full$income == 4 & anes00_full$education == 5, 3, anes00_full$education)
lws01$income <- ifelse(lws01$employment == 2 & lws01$income == 4 & lws01$education == 5, 3, lws01$income)
lws01$education <- ifelse(lws01$employment == 2 & lws01$income == 4 & lws01$education == 5, 3, lws01$education)

# If employment is 2, income is 3, and education is 5 then education should become 3
anes00_full$education <- ifelse(anes00_full$employment == 2 & anes00_full$income == 3 & anes00_full$education == 5, 3, anes00_full$education)
lws01$education <- ifelse(lws01$employment == 2 & lws01$income == 3 & lws01$education == 5, 3, lws01$education)


group.v <- c("employment", "income","education")
rnd.1 <- RANDwNND.hotdeck(data.rec = lws01, data.don = anes00_full,
                          match.vars = NULL, don.class = group.v)
fused_us00 <- create.fused(data.rec = lws01, data.don = anes00_full,
                           mtc.ids = rnd.1$mtc.ids, z.vars = c("V001249"))
unique(fused_us00$V001249)

fused_us00$net_wealth <- (fused_us00$haf + fused_us00$han) - (fused_us00$hlr + fused_us00$hln)
fused_us00$quintiles <- cut(fused_us00$net_wealth, breaks = quantile(fused_us00$net_wealth, probs = seq(0, 1, by = 0.2)), include.lowest = TRUE, labels = FALSE)

dfs <- c("fused_us00")

for (i in 1:length(dfs)) {
  tmp <- get(dfs[i])
  mi.idx <- tmp$inum.x
  for (j in 1:5) {
    assign(paste0(dfs[i], "_", j), tmp[mi.idx == j,])
  }
}

# Get the dimensions of each data frame
for (j in 1:5) {
  df_name <- paste0("fused_us00_", j)
  print(paste("The dimensions of", df_name, "are: ", dim(get(df_name))))
}

imputed_data <- imputationList(list(fused_us00_1, fused_us00_2, fused_us00_3, fused_us00_4, fused_us00_5))

us.svyrw <- svrepdesign(
  data = imputed_data,
  id = ~hid,
  weights = ~hpopwgt,
  repweights = weight_us01[, -1],  # Exclude first column with id
  scale = 1,
  rscales = rep(1 / 100),  # If using full set
  type = "other",
  combined.weights = TRUE
)

us.svyrw$designs <- lapply(us.svyrw$designs, function(design) {
  design$variables$V001249 <- ifelse(design$variables$V001249 == 1, 1,
                                     ifelse(design$variables$V001249 == 3, 0, NA))
  return(design)
})

# Fit the probit model on each imputed dataset
model_1 <- lapply(us.svyrw$designs, function(design) {
  svyglm(V001249 ~ 1 + net_wealth, design, family = binomial(link = 'probit'))
})

# Combine results from multiple imputed datasets
combined_results <- MIcombine(model_1)

# Display summary of combined results
coef(combined_results)

