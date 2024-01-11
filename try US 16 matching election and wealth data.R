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

########################          US16         ##############################################

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
      educlev %in% c(100, 200, 210) ~ 4,
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
unique(fA.rnd.1$V960552)


# Merge household and personal data based on 'hid' and 'pid'
merged_data <- merge(data_us16_wh, fA.rnd.1, by.x = "hid", by.y = "hid", all.x = TRUE)

########################          US00       ##############################################

#Load data will need to adapt in Lissy
data_us16_wi <- read_dta("us16wp (4).dta")
data_us16_wh <- read_dta("us16wh (1).dta")

zip_file_path_anes00 <- "C:/Users/ducie/Documents/WU/distribution field/Project/Election data/anes2000TSdta.zip"
extracted_dir <- "C:/Users/ducie/Documents/WU/distribution field/Project/Election data/"
unzip(zip_file_path_anes00, exdir = extracted_dir)
list.files(extracted_dir)
anes_00 <- read_dta(file.path(extracted_dir, "anes2000TS.dta"))


#create data frame with variables of interest from anes 

selected_vars <- c("V000793", "V001249","V000908", "V000912", "V000918", "V000909","V000995")
anes00_data_selected <- anes_00 %>% 
  select(all_of(selected_vars))


#transforming the education variable for data fusion
data_us16_wi <- data_us16_wi %>%
  mutate(
    education = case_when(
      educlev %in% c(111, 110,120) ~ 1,
      educlev %in% c(130,100) ~ 2,
      educlev %in% c(200, 210) ~ 3,
      educlev == 220 ~ 4,
      educlev %in% c(311, 300) ~ 5,
      educlev %in% c(312,310) ~ 6,
      educlev %in% c(313, 320) ~ 7,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-educlev)  

anes00_data_selected <- anes00_data_selected %>%
  mutate(
    education = case_when(
      V000912 == 1 ~ 1,
      V000912 == 2 ~ 2,
      V000912 == 3 ~ 3,
      V000912 == 4 ~ 4,
      V000912 == 5 ~ 5,
      V000912 == 6 ~ 6,
      V000912 == 7 ~ 7,
      TRUE ~ NA_real_
    )
  ) %>%
  select(-V000912)

#modify the employment status variable for data fusion
anes00_data_selected <- anes00_data_selected %>%
  mutate(
    employment = case_when(
      V000918 %in% c(1, 2) ~ 1,
      V000918 == 3 ~ 2,
      V000918 == 4 ~ 3,
      V000918 == 5 ~ 4,
      V000918 == 6 ~ 5,
      V000918 == 7 ~ 6,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-V000918)

data_us16_wi <- data_us16_wi %>%
  mutate(
    employment = case_when(
      lfs == 100 ~ 1,
      lfs == 200 ~ 2,
      lfs == 310 ~ 3,
      lfs == 330 ~ 4,
      lfs == 340 ~ 5,
      lfs == 320 ~ 6,
      lfs == 300 ~ NA_integer_,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-lfs)

#modify the income variable for data fusion
anes00_data_selected$V000995 <- as.factor(anes00_data_selected$V000995)

anes00_data_selected <- anes00_data_selected %>%
  mutate(
    income = as.numeric(case_when(
      V000995 %in% c("1", "2", "3") ~ 1,
      V000995 %in% c("4", "5") ~ 2,
      V000995 == 6 ~ 3,
      V000995 %in% c("7", "8", "9") ~ 4,
      V000995 %in% c( "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22") ~ 5,
      TRUE ~ NA_real_
    ))
  ) %>%
  select(-V000995)


data_us16_wi <- data_us16_wi %>%
  mutate(
    income = case_when(
      pitotal < 17955 ~ 1,
      pitotal >= 17955 & pitotal <= 33006 ~ 2,
      pitotal >= 33007 & pitotal <= 52272 ~ 3,
      pitotal >= 52273 & pitotal <= 81960 ~ 4,
      pitotal > 81960 ~ 5,
      TRUE ~ NA_real_
    )
  )



### matching process ###
#Random distance hot deck

anes00_data_selected1 <- anes00_data_selected[complete.cases(anes00_data_selected$income), ]
unique(anes00_data_selected1$income)
data_us16_wi1 <- data_us16_wi[complete.cases(data_us16_wi$income), ]
unique(data_us16_wi1$income)



group.v <- c("income")
rnd.1 <- RANDwNND.hotdeck(data.rec = data_us16_wi1, data.don = anes00_data_selected1,
                          match.vars = NULL, don.class = group.v)

fA.rnd.1 <- create.fused(data.rec = data_us16_wi1, data.don = anes00_data_selected1,
                         mtc.ids = rnd.1$mtc.ids, z.vars = c("V000793","V001249"))
unique(fA.rnd.1$001249)


# Merge household and personal data based on 'hid' and 'pid'
merged_data <- merge(data_us16_wh, fA.rnd.1, by.x = "hid", by.y = "hid", all.x = TRUE)

########################          US04       ##############################################

#Load data will need to adapt in Lissy
data_us16_wi <- read_dta("us16wp (4).dta")
data_us16_wh <- read_dta("us16wh (1).dta")

zip_file_path_anes04 <- "C:/Users/ducie/Documents/WU/distribution field/Project/Election data/anes2004TSdta.zip"
extracted_dir <- "C:/Users/ducie/Documents/WU/distribution field/Project/Election data/"
unzip(zip_file_path_anes04, exdir = extracted_dir)
list.files(extracted_dir)
anes_04 <- read_dta(file.path(extracted_dir, "anes2004TS.dta"))


#create data frame with variables of interest from anes 

selected_vars <- c("V041109a", "V043085a","V043250", "V043251", "V043254", "V043260b","V045026", "V043294")
anes04_data_selected <- anes_04 %>% 
  select(all_of(selected_vars))


#transforming the education variable for data fusion
data_us16_wi <- data_us16_wi %>%
  mutate(
    education = case_when(
      educlev %in% c(111, 110,120) ~ 1,
      educlev %in% c(130,100) ~ 2,
      educlev %in% c(200, 210) ~ 3,
      educlev == 220 ~ 4,
      educlev %in% c(311, 300) ~ 5,
      educlev %in% c(312,310) ~ 6,
      educlev %in% c(313, 320) ~ 7,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-educlev)  

anes04_data_selected <- anes04_data_selected %>%
  mutate(
    education = case_when(
      V043254 == 1 ~ 1,
      V043254 == 2 ~ 2,
      V043254 == 3 ~ 3,
      V043254 == 4 ~ 4,
      V043254 == 5 ~ 5,
      V043254 == 6 ~ 6,
      V043254 == 7 ~ 7,
      TRUE ~ NA_real_
    )
  ) %>%
  select(-V043254)

#modify the employment status variable for data fusion
anes04_data_selected <- anes04_data_selected %>%
  mutate(
    employment = case_when(
      V043260b == 1 ~ 1,
      V043260b == 4 ~ 2,
      V043260b == 5 ~ 3,
      V043260b == 6 ~ 4,
      V043260b == 7 ~ 5,
      V043260b == 8 ~ 6,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-V043260b)

data_us16_wi <- data_us16_wi %>%
  mutate(
    employment = case_when(
      lfs == 100 ~ 1,
      lfs == 200 ~ 2,
      lfs == 310 ~ 3,
      lfs == 330 ~ 4,
      lfs == 340 ~ 5,
      lfs == 320 ~ 6,
      lfs == 300 ~ NA_integer_,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-lfs)

#modify the income variable for data fusion
anes04_data_selected$V043294 <- as.factor(anes04_data_selected$V043294)

anes04_data_selected <- anes04_data_selected %>%
  mutate(
    income = as.numeric(case_when(
      V043294 %in% c("1", "2", "3","4", "5", "6", "7", "8", "9") ~ 1,
      V043294 %in% c("10", "11", "12", "13") ~ 2,
      V043294 %in% c("14", "15", "16", "17") ~ 3,
      V043294 %in% c("18", "19", "20") ~ 4,
      V043294 %in% c( "21", "22", "23") ~ 5,
      TRUE ~ NA_real_
    ))
  ) %>%
  select(-V043294)


data_us16_wi <- data_us16_wi %>%
  mutate(
    income = case_when(
      pitotal < 18486 ~ 1,
      pitotal >= 18486 & pitotal <= 34674 ~ 2,
      pitotal >= 34675 & pitotal <= 55230 ~ 3,
      pitotal >= 52231 & pitotal <= 88001 ~ 4,
      pitotal > 88001 ~ 5,
      TRUE ~ NA_real_
    )
  )



### matching process ###
#Random distance hot deck

anes04_data_selected1 <- anes04_data_selected[complete.cases(anes04_data_selected$income), ]
unique(anes04_data_selected1$income)
data_us16_wi1 <- data_us16_wi[complete.cases(data_us16_wi$income), ]
unique(data_us16_wi1$income)



group.v <- c("income")
rnd.1 <- RANDwNND.hotdeck(data.rec = data_us16_wi1, data.don = anes04_data_selected1,
                          match.vars = NULL, don.class = group.v)

fA.rnd.1 <- create.fused(data.rec = data_us16_wi1, data.don = anes04_data_selected1,
                         mtc.ids = rnd.1$mtc.ids, z.vars = c("V043085a","V045026"))
unique(fA.rnd.1$V045026)


# Merge household and personal data based on 'hid' and 'pid'
merged_data <- merge(data_us16_wh, fA.rnd.1, by.x = "hid", by.y = "hid", all.x = TRUE)

########################          US08       ##############################################

#Load data will need to adapt in Lissy
data_us16_wi <- read_dta("us16wp (4).dta")
data_us16_wh <- read_dta("us16wh (1).dta")

zip_file_path_anes08 <- "C:/Users/ducie/Documents/WU/distribution field/Project/Election data/anes_timeseries_2008_dta.zip"
extracted_dir <- "C:/Users/ducie/Documents/WU/distribution field/Project/Election data/"
unzip(zip_file_path_anes08, exdir = extracted_dir)
list.files(extracted_dir)
anes_08 <- read_dta(file.path(extracted_dir, "anes_timeseries_2008.dta"))


#create data frame with variables of interest from anes 

selected_vars <- c("V083249", "V083222a","V083218x", "V085044a")
anes08_data_selected <- anes_08 %>% 
  select(all_of(selected_vars))


#transforming the education variable for data fusion
data_us16_wi <- data_us16_wi %>%
  mutate(
    education = case_when(
      educlev %in% c(111, 110,120) ~ 1,
      educlev %in% c(130,100) ~ 2,
      educlev %in% c(200, 210) ~ 3,
      educlev == 220 ~ 4,
      educlev %in% c(311, 300) ~ 5,
      educlev %in% c(312,310) ~ 6,
      educlev %in% c(313, 320) ~ 7,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-educlev)  

anes08_data_selected <- anes08_data_selected %>%
  mutate(
    education = case_when(
      V083218x == 1 ~ 1,
      V083218x == 2 ~ 2,
      V083218x == 3 ~ 3,
      V083218x == 4 ~ 4,
      V083218x == 5 ~ 5,
      V083218x == 6 ~ 6,
      V083218x == 7 ~ 7,
      TRUE ~ NA_real_
    )
  ) %>%
  select(-V083218x)

#modify the employment status variable for data fusion
anes08_data_selected <- anes08_data_selected %>%
  mutate(
    employment = case_when(
      V083222a == 1 ~ 1,
      V083222a == 4 ~ 2,
      V083222a == 5 ~ 3,
      V083222a == 6 ~ 4,
      V083222a == 7 ~ 5,
      V083222a == 8 ~ 6,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-V083222a)

data_us16_wi <- data_us16_wi %>%
  mutate(
    employment = case_when(
      lfs == 100 ~ 1,
      lfs == 200 ~ 2,
      lfs == 310 ~ 3,
      lfs == 330 ~ 4,
      lfs == 340 ~ 5,
      lfs == 320 ~ 6,
      lfs == 300 ~ NA_integer_,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-lfs)

#modify the income variable for data fusion
anes08_data_selected$V083249 <- as.factor(anes08_data_selected$V083249)

anes08_data_selected <- anes08_data_selected %>%
  mutate(
    income = as.numeric(case_when(
      V083249 %in% c("1", "2", "3","4", "5", "6", "7", "8", "9") ~ 1,
      V083249 %in% c("10", "11", "12", "13", "14") ~ 2,
      V083249 %in% c( "15", "16", "17") ~ 3,
      V083249 %in% c("18", "19", "20") ~ 4,
      V083249 %in% c( "21", "22", "23","24","25") ~ 5,
      TRUE ~ NA_real_
    ))
  ) %>%
  select(-V083249)


data_us16_wi <- data_us16_wi %>%
  mutate(
    income = case_when(
      pitotal < 20712 ~ 1,
      pitotal >= 20712 & pitotal <= 39000 ~ 2,
      pitotal >= 39001 & pitotal <= 62725 ~ 3,
      pitotal >= 62726 & pitotal <= 100240 ~ 4,
      pitotal > 100240 ~ 5,
      TRUE ~ NA_real_
    )
  )



### matching process ###
#Random distance hot deck

anes08_data_selected1 <- anes08_data_selected[complete.cases(anes08_data_selected$income), ]
unique(anes08_data_selected1$income)
data_us16_wi1 <- data_us16_wi[complete.cases(data_us16_wi$income), ]
unique(data_us16_wi1$income)



group.v <- c("income")
rnd.1 <- RANDwNND.hotdeck(data.rec = data_us16_wi1, data.don = anes08_data_selected1,
                          match.vars = NULL, don.class = group.v)

fA.rnd.1 <- create.fused(data.rec = data_us16_wi1, data.don = anes08_data_selected1,
                         mtc.ids = rnd.1$mtc.ids, z.vars = c("V085044a"))
unique(fA.rnd.1$V085044a)


# Merge household and personal data based on 'hid' and 'pid'
merged_data <- merge(data_us16_wh, fA.rnd.1, by.x = "hid", by.y = "hid", all.x = TRUE)

########################          US12       ##############################################

#Load data will need to adapt in Lissy
data_us16_wi <- read_dta("us16wp (4).dta")
data_us16_wh <- read_dta("us16wh (1).dta")

zip_file_path_anes12 <- "C:/Users/ducie/Documents/WU/distribution field/Project/Election data/anes_timeseries_2012_dta.zip"
extracted_dir <- "C:/Users/ducie/Documents/WU/distribution field/Project/Election data/"
unzip(zip_file_path_anes12, exdir = extracted_dir)
list.files(extracted_dir)
anes_12 <- read_dta(file.path(extracted_dir, "anes_timeseries_2012.dta"))


#create data frame with variables of interest from anes 

selected_vars <- c("prevote_intpres", "dem_age_r_x","dem_edugroup_x", "dem_empstatus_1digitfin_x", "postvote_presvtwho", "incgroup_prepost_x")
anes12_data_selected <- anes_12 %>% 
  select(all_of(selected_vars))


#transforming the education variable for data fusion
data_us16_wi <- data_us16_wi %>%
  mutate(
    education = case_when(
      educlev %in% c(111, 110,120,100,130) ~ 1,
      educlev %in% c(200,210) ~ 2,
      educlev %in% c(220, 300, 311) ~ 3,
      educlev %in% c(310, 312) ~ 4,
      educlev %in% c(313, 320) ~ 5,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-educlev)  

anes12_data_selected <- anes12_data_selected %>%
  mutate(
    education = case_when(
      dem_edugroup_x == 1 ~ 1,
      dem_edugroup_x == 2 ~ 2,
      dem_edugroup_x == 3 ~ 3,
      dem_edugroup_x == 4 ~ 4,
      dem_edugroup_x == 5 ~ 5,
      TRUE ~ NA_real_
    )
  ) %>%
  select(-dem_edugroup_x)

#modify the employment status variable for data fusion
anes12_data_selected <- anes12_data_selected %>%
  mutate(
    employment = case_when(
      dem_empstatus_1digitfin_x == 1 ~ 1,
      dem_empstatus_1digitfin_x == 4 ~ 2,
      dem_empstatus_1digitfin_x == 5 ~ 3,
      dem_empstatus_1digitfin_x == 6 ~ 4,
      dem_empstatus_1digitfin_x == 7 ~ 5,
      dem_empstatus_1digitfin_x == 8 ~ 6,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-dem_empstatus_1digitfin_x)

data_us16_wi <- data_us16_wi %>%
  mutate(
    employment = case_when(
      lfs == 100 ~ 1,
      lfs == 200 ~ 2,
      lfs == 310 ~ 3,
      lfs == 330 ~ 4,
      lfs == 340 ~ 5,
      lfs == 320 ~ 6,
      lfs == 300 ~ NA_integer_,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-lfs)

#modify the income variable for data fusion
anes12_data_selected$incgroup_prepost_x <- as.factor(anes12_data_selected$incgroup_prepost_x)

anes12_data_selected <- anes12_data_selected %>%
  mutate(
    income = as.numeric(case_when(
      incgroup_prepost_x %in% c("1", "2", "3","4", "5", "6") ~ 1,
      incgroup_prepost_x %in% c("7","8","9","10", "11", "12") ~ 2,
      incgroup_prepost_x %in% c( "13", "14", "15", "16", "17") ~ 3,
      incgroup_prepost_x %in% c("18", "19", "20","21","22") ~ 4,
      incgroup_prepost_x %in% c("23","24","25","26", "27", "28") ~ 5,
      TRUE ~ NA_real_
    ))
  ) %>%
  select(-incgroup_prepost_x)


data_us16_wi <- data_us16_wi %>%
  mutate(
    income = case_when(
      pitotal < 20599 ~ 1,
      pitotal >= 20600 & pitotal <= 39764 ~ 2,
      pitotal >= 39765 & pitotal <= 64582 ~ 3,
      pitotal >= 64583 & pitotal <= 104096 ~ 4,
      pitotal > 104096 ~ 5,
      TRUE ~ NA_real_
    )
  )



### matching process ###
#Random distance hot deck

anes12_data_selected1 <- anes12_data_selected[complete.cases(anes12_data_selected$income), ]
unique(anes12_data_selected1$income)
data_us16_wi1 <- data_us16_wi[complete.cases(data_us16_wi$income), ]
unique(data_us16_wi1$income)



group.v <- c("income")
rnd.1 <- RANDwNND.hotdeck(data.rec = data_us16_wi1, data.don = anes12_data_selected1,
                          match.vars = NULL, don.class = group.v)

fA.rnd.1 <- create.fused(data.rec = data_us16_wi1, data.don = anes12_data_selected1,
                         mtc.ids = rnd.1$mtc.ids, z.vars = c("postvote_presvtwho", "prevote_intpres"))
unique(fA.rnd.1$postvote_presvtwho)


# Merge household and personal data based on 'hid' and 'pid'
merged_data <- merge(data_us16_wh, fA.rnd.1, by.x = "hid", by.y = "hid", all.x = TRUE)

########################          US20       ##############################################

#Load data will need to adapt in Lissy
data_us16_wi <- read_dta("us16wp (4).dta")
data_us16_wh <- read_dta("us16wh (1).dta")

zip_file_path_anes20 <- "C:/Users/ducie/Documents/WU/distribution field/Project/Election data/anes_timeseries_2020_stata_20220210.zip"
extracted_dir <- "C:/Users/ducie/Documents/WU/distribution field/Project/Election data/"
unzip(zip_file_path_anes20, exdir = extracted_dir)
list.files(extracted_dir)
anes_20 <- read_dta(file.path(extracted_dir, "anes_timeseries_2020_stata_20220210.dta"))


#create data frame with variables of interest from anes 

selected_vars <- c("V201004", "V201029","V202073", "V201507x", "V201511x", "V201534x", "V202468x")
anes20_data_selected <- anes_20 %>% 
  select(all_of(selected_vars))


#transforming the education variable for data fusion
data_us16_wi <- data_us16_wi %>%
  mutate(
    education = case_when(
      educlev %in% c(111, 110,120,100,130) ~ 1,
      educlev %in% c(200,210) ~ 2,
      educlev %in% c(220, 300, 311) ~ 3,
      educlev %in% c(310, 312) ~ 4,
      educlev %in% c(313, 320) ~ 5,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-educlev)  

anes20_data_selected <- anes20_data_selected %>%
  mutate(
    education = case_when(
      V201511x == 1 ~ 1,
      V201511x == 2 ~ 2,
      V201511x == 3 ~ 3,
      V201511x == 4 ~ 4,
      V201511x == 5 ~ 5,
      TRUE ~ NA_real_
    )
  ) %>%
  select(-V201511x)

#modify the employment status variable for data fusion
anes20_data_selected <- anes20_data_selected %>%
  mutate(
    employment = case_when(
      V201534x == 1 ~ 1,
      V201534x == 4 ~ 2,
      V201534x == 5 ~ 3,
      V201534x == 6 ~ 4,
      V201534x == 7 ~ 5,
      V201534x == 8 ~ 6,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-V201534x)

data_us16_wi <- data_us16_wi %>%
  mutate(
    employment = case_when(
      lfs == 100 ~ 1,
      lfs == 200 ~ 2,
      lfs == 310 ~ 3,
      lfs == 330 ~ 4,
      lfs == 340 ~ 5,
      lfs == 320 ~ 6,
      lfs == 300 ~ NA_integer_,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-lfs)

#modify the income variable for data fusion
anes20_data_selected$V202468x <- as.factor(anes20_data_selected$V202468x)

anes20_data_selected <- anes20_data_selected %>%
  mutate(
    income = as.numeric(case_when(
      V202468x %in% c("1", "2", "3","4") ~ 1,
      V202468x %in% c( "5", "6", "7","8","9") ~ 2,
      V202468x %in% c( "10", "11", "12", "13", "14", "15") ~ 3,
      V202468x %in% c("16", "17", "18", "19") ~ 4,
      V202468x %in% c("20","21","22") ~ 5,
      TRUE ~ NA_real_
    ))
  ) %>%
  select(-V202468x)


data_us16_wi <- data_us16_wi %>%
  mutate(
    income = case_when(
      pitotal < 27026 ~ 1,
      pitotal >= 27026 & pitotal <= 52179 ~ 2,
      pitotal >= 52180 & pitotal <= 85076 ~ 3,
      pitotal >= 85077 & pitotal <= 141110 ~ 4,
      pitotal > 141110 ~ 5,
      TRUE ~ NA_real_
    )
  )

### matching process ###
#Random distance hot deck

anes20_data_selected1 <- anes20_data_selected[complete.cases(anes20_data_selected$income), ]
unique(anes20_data_selected1$income)
data_us16_wi1 <- data_us16_wi[complete.cases(data_us16_wi$income), ]
unique(data_us16_wi1$income)



group.v <- c("income")
rnd.1 <- RANDwNND.hotdeck(data.rec = data_us16_wi1, data.don = anes20_data_selected1,
                          match.vars = NULL, don.class = group.v)

fA.rnd.1 <- create.fused(data.rec = data_us16_wi1, data.don = anes20_data_selected1,
                         mtc.ids = rnd.1$mtc.ids, z.vars = c("V202073", "V201029"))
unique(fA.rnd.1$V202073)


# Merge household and personal data based on 'hid' and 'pid'
merged_data <- merge(data_us16_wh, fA.rnd.1, by.x = "hid", by.y = "hid", all.x = TRUE)

## need to do the weight
##need to ask about issue on income from 12, 20
##need to try for matching with 3 categories can run to see missing class and enter them manually
