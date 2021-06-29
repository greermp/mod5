library("tidyverse")
library("psych")
library("ggthemes")
library("lubridate")
library("scales")
library("ggrepel")
theme_set(theme_few(base_size = 14))
setwd("~/MSBA/MOD5")

allinv <- read.csv("newdata/Vast_Invoices_010118_123120.csv")

allinv$INVOICE_DATE <- lubridate::mdy(allinv$INVOICE_DATE)
# source("preprocess.R")
samp <- allinv %>% slice_sample(prop=.2)
samp=samp %>% group_by(CUSTOMER_NUMBER) %>% arrange(INVOICE_DATE) %>% mutate(visit = row_number())
# 
# 
# vsamp %>% group_by(CUSTOMER_NUMBER, INVOICE_NUMBER) %>% mutate(visit = row_number())





invVisit <- allinv %>% group_by(CUSTOMER_NUMBER) %>% arrange(INVOICE_DATE) %>% mutate(visit=row_number())
write.csv(invVisit, "newdata/Vast_Invoices_010118_123120_WithVisit.csv")
