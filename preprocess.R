library("tidyverse")
library("psych")
library("ggthemes")
library("lubridate")
library("scales")
library("ggrepel")
theme_set(theme_clean(base_size = 16))

setwd("~/MSBA/MOD5")

invoice <- read.csv('newdata/Vast_Invoices_010118_123120_WithVisit.csv')
# invoice <- read.csv('newdata/181920invoiceWithVisitNum.csv')
invoice$Store.ID  <-  as.numeric(invoice$Store.ID)
invoice <-  invoice %>%   mutate(Store.ID = case_when(
  Store.ID == 200 ~ 'Main Street Shell',
  Store.ID == 210 ~ 'Falls Church',
  Store.ID == 300 ~ 'Springfield',
  Store.ID == 400 ~ 'Centreville',
  Store.ID == 410 ~ 'Vienna',
  Store.ID == 411 ~ 'Tysons',
  Store.ID == 415 ~ 'Herndon',
  Store.ID == 420 ~ 'Chantilly',
  Store.ID == 421 ~ 'South Riding',
  Store.ID == 430 ~ 'Broadlands',
  Store.ID == 440 ~ 'Ashburn',
  Store.ID == 441 ~ 'Dulles',
  Store.ID == 450 ~ 'One Loudoun',
  Store.ID == 460 ~ 'Gainesville',
  Store.ID == 465 ~ 'Bristow',
  Store.ID == 480 ~ 'Cascades',
  Store.ID == 500 ~ 'Chesterfield',
  Store.ID == 610 ~ 'Leesburg',
  TRUE ~ 'NA'))

invoice$Store.ID <- as.factor(invoice$Store.ID)
  
invoice$CUST_CREATE_DATE <- lubridate::mdy(invoice$CUST_CREATE_DATE)

invoice$INVOICE_DATE <- lubridate::ymd(invoice$INVOICE_DATE)

invoice <- invoice %>% mutate(createYear =year(CUST_CREATE_DATE))
invoice$InvoiceYear <- year(invoice$INVOICE_DATE)
    
invoice <-  invoice %>%
mutate(BrandSum = case_when(grepl("Chevrolet", invoice$MAKE , ignore.case = TRUE) ~ "Chevrolet",
                                                            grepl("BMW", invoice$MAKE , ignore.case = TRUE) ~ "BMW",
                                                            grepl("Buick", invoice$MAKE , ignore.case = TRUE) ~ "Buick",
                                                            grepl("Chrysler", invoice$MAKE , ignore.case = TRUE) ~ "Chrysler",
                                                            grepl("Cadillac", invoice$MAKE , ignore.case = TRUE) ~ "Cadillac",
                                                            grepl("Dodge", invoice$MAKE , ignore.case = TRUE) ~ "Dodge",
                                                            grepl("Ford", invoice$MAKE , ignore.case = TRUE) ~ "Ford",
                                                            grepl("GMC", invoice$MAKE , ignore.case = TRUE) ~ "GMC",
                                                            grepl("Hyundai ", invoice$MAKE , ignore.case = TRUE) ~ "Hyundai ",
                                                            grepl("Infiniti", invoice$MAKE , ignore.case = TRUE) ~ "Infiniti",
                                                            grepl("Jeep", invoice$MAKE , ignore.case = TRUE) ~ "Jeep",
                                                            grepl("Kia", invoice$MAKE , ignore.case = TRUE) ~ "Kia",
                                                            grepl("Lexus", invoice$MAKE , ignore.case = TRUE) ~ "Lexus",
                                                            grepl("Lincoln", invoice$MAKE , ignore.case = TRUE) ~ "Lincoln",
                                                            grepl("Mercedes", invoice$MAKE , ignore.case = TRUE) ~ "Mercedes",
                                                            grepl("Mazda", invoice$MAKE , ignore.case = TRUE) ~ "Mazda",
                                                            grepl("Cadillac", invoice$MAKE , ignore.case = TRUE) ~ "Cadillac",
                                                            grepl("Toyota", invoice$MAKE , ignore.case = TRUE) ~ "Toyota",
                                                            grepl("MERCEDES", invoice$MAKE , ignore.case = TRUE) ~ "Mercedes",
                                                            grepl("NISSAN", invoice$MAKE , ignore.case = TRUE) ~ "Nissan",
                                                            grepl("Mitsubishi", invoice$MAKE , ignore.case = TRUE) ~ "Mitsubishi",
                                                            grepl("Honda", invoice$MAKE , ignore.case = TRUE) ~ "Honda",
                                                            grepl("Acura", invoice$MAKE , ignore.case = TRUE) ~ "Acura",
                                                            grepl("Suburu", invoice$MAKE , ignore.case = TRUE) ~ "Suburu",
                                                            grepl("Isuzu", invoice$MAKE , ignore.case = TRUE) ~ "Isuzu",
                                                            grepl("Suzuki", invoice$MAKE , ignore.case = TRUE) ~ "Suzuki",
                                                            grepl("SATURN", invoice$MAKE , ignore.case = TRUE) ~ "Saturn",
                                                            grepl("CHEVY", invoice$MAKE , ignore.case = TRUE) ~ "Chevrolet",
                                                            grepl("Ram", invoice$MAKE , ignore.case = TRUE) ~ "Dodge",
                                                            grepl("PONTIAC", invoice$MAKE , ignore.case = TRUE) ~ "Pontiac",
                                                            grepl("MERCURY", invoice$MAKE , ignore.case = TRUE) ~ "Mercury",
                                                            grepl("SUBARU", invoice$MAKE , ignore.case = TRUE) ~ "Subaru",
                                                            TRUE ~ as.character(invoice$MAKE , ignore.case = TRUE)))

brandFix <- invoice %>% group_by(BrandSum) %>% mutate(cbrand=n()) %>% arrange(cbrand)

brandFix$BrandSum[brandFix$cbrand<100] = NA
brandFix$BrandSum[brandFix$BrandSum=="NULL"] = NA
    
  # brandFix$BrandSum
invoice$BrandSum=brandFix$BrandSum
invoice$BrandSum = as.factor(invoice$BrandSum)

# write.csv(invoice, "newdata/181920invoiceWithVisitNumClean.csv")
write.csv(invoice, "newdata/InvoiceSummary.csv")
      