library(dplyr)
library(doBy)
library(tidyverse)
library(Hmisc)
library(tidyr)
library(haven)
library(acid)
library(StatMatch)
getwd()
setwd("C:/Users/ducie/Documents/WU/distribution field/Project")

########################US16##############################################

#Load data
data_us16_wi <- read_dta("us16wp (4).dta")
data_us16_wh <- read_dta("us16wh (1).dta")


zip_file_path <- "C:/Users/ducie/Documents/WU/distribution field/Project/Election data/anes_timeseries_2016_dta.zip"

extracted_dir <- "C:/Users/ducie/Documents/WU/distribution field/Project/Election data/"

unzip(zip_file_path, exdir = extracted_dir)

list.files(extracted_dir)

anes_data <- read_dta(file.path(extracted_dir, "anes_timeseries_2016.dta"))


#create data frame with variables of interest from anes 

selected_vars <- c("V161109", "V161268", "V161316", "V161324", "V161334", "V162024a", 
                   "V162034a", "V162292a", "V161270", "V161342", "V161267", 
                   "V161277", "V161361x", "V160001")
anes_data_selected <- anes_data %>% 
  select(all_of(selected_vars))


#transforming the education variable for data fusion
data_us16_wi <- data_us16_wi %>%
  mutate(
    education = case_when(
      educlev %in% c(111, 110) ~ 1,
      educlev == 120 ~ 2,
      educlev == 130 ~ 3,
      educlev == 210 ~ 4,
      educlev == 220 ~ 5,
      educlev == 311 ~ 6,
      educlev == 312 ~ 7,
      educlev == 313 ~ 8,
      educlev == 320 ~ 9,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-educlev)  

anes_data_selected <- anes_data_selected %>%
  mutate(
    education = case_when(
      V161270 %in% c(1, 2) ~ 1,
      V161270 == 3 ~ 2,
      V161270 %in% c(4, 5) ~ 3,
      V161270 %in% c(6, 7, 8, 9, 90) ~ 4,
      V161270 == 10 ~ 5,
      V161270 %in% c(11, 12) ~ 6,
      V161270 == 13 ~ 7,
      V161270 %in% c(14, 15) ~ 8,
      V161270 == 16 ~ 9,
      TRUE ~ NA_real_
    )
  ) %>%
  select(-V161270)

#modify the employment status variable for data fusion
anes_data_selected <- anes_data_selected %>%
  mutate(
    employment = case_when(
      V161277 == 1 ~ 1,
      V161277 == 4 ~ 2,
      V161277 == 5 ~ 3,
      V161277 == 8 ~ 4,
      V161277 == 6 ~ 5,
      V161277 == 7 ~ 6,
      V161277 %in% c(2, -9) ~ NA_integer_,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-V161277)

data_us16_wi <- data_us16_wi %>%
  mutate(
    employment = case_when(
      lfs == 100 ~ 1,
      lfs == 200 ~ 2,
      lfs == 310 ~ 3,
      lfs == 320 ~ 4,
      lfs == 330 ~ 5,
      lfs == 340 ~ 6,
      lfs == 300 ~ NA_integer_,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-lfs)

#modify the income variable for data fusion
anes_data_selected$V161361x <- as.factor(anes_data_selected$V161361x)

names(anes_data_selected)[names(anes_data_selected) == "V161361x"] <- "income"

anes_data_selected <- anes_data_selected %>%
  mutate(
    income = case_when(
      income %in% c("1", "2", "3", "4", "5", "6", "7","8") ~ "1",
      income %in% c("9", "10", "11", "12", "13") ~ "2",
      income %in% c("14", "15", "16", "17", "18", "19") ~ "3",
      income %in% c("20", "21", "22", "23", "24") ~ "4",
      income %in% c("25", "26", "27", "28", "29") ~ "5",
      TRUE ~ "NA"
    )
  )

data_us16_wi <- data_us16_wi %>%
  mutate(
    income = case_when(
      pitotal < 24002 ~ 1,
      pitotal >= 24003 & pitotal <= 45600 ~ 2,
      pitotal >= 45601 & pitotal <= 74869 ~ 3,
      pitotal >= 74870 & pitotal <= 121018 ~ 4,
      pitotal > 121019 ~ 5,
      TRUE ~ NA_real_
    )
  )

data_us16_wi$income <- data_us16_wi$pitotal


### matching process ###
#Random distance hot deck

anes_data_selected1 <- anes_data_selected[complete.cases(anes_data_selected$employment), ]
unique(anes_data_selected1$employment)
data_us16_wi1 <- data_us16_wi[complete.cases(data_us16_wi$employment), ]
unique(data_us16_wi1$employment)

group.v <- c("employment")
rnd.1 <- RANDwNND.hotdeck(data.rec = data_us16_wi1, data.don = anes_data_selected1,
                          match.vars = NULL, don.class = group.v)

fA.rnd.1 <- create.fused(data.rec = data_us16_wi1, data.don = anes_data_selected1,
                         mtc.ids = rnd.1$mtc.ids, z.vars = c("V162034a"))
unique(fA.rnd.1$V162034a)


# Merge household and personal data based on 'hid' and 'pid'
merged_data <- merge(data_us16_wh, fA.rnd.1, by.x = "hid", by.y = "hid", all.x = TRUE)


########################          US96         ##############################################

#Load data will need to adapt in Lissy
data_us16_wi <- read_dta("us16wp (4).dta")
data_us16_wh <- read_dta("us16wh (1).dta")

zip_file_path_anes96 <- "C:/Users/ducie/Documents/WU/distribution field/Project/Election data/anes1996dta.zip"
extracted_dir <- "C:/Users/ducie/Documents/WU/distribution field/Project/Election data/"
unzip(zip_file_path_anes96, exdir = extracted_dir)
list.files(extracted_dir)
anes_96 <- read_dta(file.path(extracted_dir, "nes96.dta"))


#create data frame with variables of interest from anes 

selected_vars <- c("V960366", "V960552","V960604", "V960606", "V960610", "V960615","V960702", "V960701" )
anes96_data_selected <- anes_96 %>% 
  select(all_of(selected_vars))


#transforming the education variable for data fusion
data_us16_wi <- data_us16_wi %>%
  mutate(
    education = case_when(
      educlev %in% c(111, 110,120) ~ 1,
      educlev == 130 ~ 2,
      educlev %in% c(100, 200, 210) ~ 3,
      educlev %in% c(220,300, 311) ~ 4,
      educlev %in% c(310, 312) ~ 5,
      educlev %in% c(313,320) ~ 6,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-educlev)  

anes96_data_selected <- anes96_data_selected %>%
  mutate(
    education = case_when(
      V960610 == 1 ~ 1,
      V960610 == 2 ~ 2,
      V960610 %in% c(3, 4) ~ 3,
      V960610 == 5 ~ 4,
      V960610 == 6 ~ 5,
      V960610 == 7 ~ 6,
      TRUE ~ NA_real_
    )
  ) %>%
  select(-V960610)

#modify the employment status variable for data fusion
anes96_data_selected <- anes96_data_selected %>%
  mutate(
    employment = case_when(
      V960615 %in% c(10,15,16,17,18) ~ 1,
      V960615 == 40 ~ 2,
      V960615 %in% c(50, 51) ~ 3,
      V960615 %in% c(80, 81) ~ 4,
      V960615 %in% c(70,71,75) ~ 5,
      V960615 == 20 ~ NA_integer_,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-V960615)

data_us16_wi <- data_us16_wi %>%
  mutate(
    employment = case_when(
      lfs == 100 ~ 1,
      lfs == 200 ~ 2,
      lfs == 310 ~ 3,
      lfs == 320 ~ 4,
      lfs == 340 ~ 5,
      lfs %in% c(300, 330) ~ NA_integer_,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-lfs)

#modify the income variable for data fusion
anes96_data_selected$V960701 <- as.factor(anes96_data_selected$V960701)

anes96_data_selected <- anes96_data_selected %>%
  mutate(
    income = as.numeric(case_when(
      V960701 %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ 1,
      V960701 %in% c("11", "12", "13") ~ 2,
      V960701 %in% c("14", "15", "16") ~ 3,
      V960701 %in% c("17", "18", "19", "20") ~ 4,
      V960701 %in% c("21", "22", "23", "24") ~ 5,
      TRUE ~ NA_real_
    ))
  ) %>%
  select(-V960701)


data_us16_wi <- data_us16_wi %>%
  mutate(
    income = case_when(
      pitotal < 14768 ~ 1,
      pitotal >= 14768 & pitotal <= 22273 ~ 2,
      pitotal >= 22274 & pitotal <= 36653 ~ 3,
      pitotal >= 36654 & pitotal <= 56369 ~ 4,
      pitotal > 56359 ~ 5,
      TRUE ~ NA_real_
    )
  )



### matching process ###
#Random distance hot deck

anes96_data_selected1 <- anes96_data_selected[complete.cases(anes96_data_selected$employment), ]
unique(anes96_data_selected1$employment)
data_us16_wi1 <- data_us16_wi[complete.cases(data_us16_wi$employment), ]
unique(data_us16_wi1$employment)

group.v <- c("employment")
rnd.1 <- RANDwNND.hotdeck(data.rec = data_us16_wi1, data.don = anes96_data_selected1,
                          match.vars = NULL, don.class = group.v)

fA.rnd.1 <- create.fused(data.rec = data_us16_wi1, data.don = anes96_data_selected1,
                         mtc.ids = rnd.1$mtc.ids, z.vars = c("V960366","V960552"))
unique(fA.rnd.1$V162034a)


# Merge household and personal data based on 'hid' and 'pid'
merged_data <- merge(data_us16_wh, fA.rnd.1, by.x = "hid", by.y = "hid", all.x = TRUE)
